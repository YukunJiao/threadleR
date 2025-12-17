# R client package for Threadle CLIconsole

library(processx)
library(jsonlite)

#' Start a Threadle CLI console process
#'
#' Launches the Threadle.CLIconsole executable and stores the process handle
#' in the global environment as `.threadle_proc`. The process is started with
#' silent mode and an end marker to delimit responses.
#'
#' @param path Path to the Threadle CLI executable.
#' @return Invisibly returns the `processx` process object.
#' @export
th_start_threadle <- function(path = "Threadle.CLIconsole.exe") {
  if (exists(".threadle_proc", envir=.GlobalEnv)) {
    stop("'.threadle_proc' process already running.")
  }
  proc <- process$new(path, args=c("--silent","--endmarker"), stdin="|", stdout="|", stderr = "|")
  #proc <- process$new(path, args=c("--endmarker"), stdin="|", stdout="|", stderr = "|")
  assign(".threadle_proc", proc, envir=.GlobalEnv)
  invisible(proc)
}

#' Stop the running Threadle CLI console process
#'
#' Terminates the Threadle process previously started with `.start_threadle()`.
#'
#' @return None; prints status messages.
#' @export
th_stop_threadle <- function() {
  if (exists(".threadle_proc", envir=.GlobalEnv)) {
    proc <- get(".threadle_proc", envir=.GlobalEnv)
    
    if (proc$is_alive()) {
      proc$kill()
      message("'.threadle_proc' process terminated.")
    }
    else {
      message("'.threadle_proc' process is already not running.")
    }
    rm(".threadle_proc", envir = .GlobalEnv)
  }
  else {
    message("No '.threadle_proc' process found.")
  }
}

#' Send a command to the Threadle CLI backend
#'
#' Internal helper function used by all wrappers. Sends a command string and
#' waits for the `__END__` marker, returning all intermediate output lines.
#'
#' @param cmd A character string containing the CLI command to send.
#' @return A character vector with output lines.
#' @keywords internal
.send_command <- function(cmd) {
  #print(cmd)
  proc <- get(".threadle_proc", envir=.GlobalEnv)
  proc$write_input(paste0(cmd,"\n"))
  #print("ok, sent")
  out <- character()
  repeat {
    new <- proc$read_output_lines()
    #print(new)
    if (length(new) > 0) {
      out <- c(out, new)
      #out
      if (any(new == "__END__")) {
        out <- out[out != "__END__"]  # drop the marker
        break
      }
    } else {
      Sys.sleep(0.01)  # small pause between polls
    }
  }
  if (length(out)>0)
    out
}

#' Set name and structure name
#' 
#' @param name description
.th_name <- function(x) {
  if (is.character(x) && length(x) == 1) return(x)
  if (is.list(x) && !is.null(x$name) && is.character(x$name) && length(x$name) == 1) return(x$name)
  stop("Expected a Threadle structure object (with $name) or a single character name.")
}


#' Set working directory inside the Threadle CLI environment
#'
#' @param dir Path to the directory.
#' @return CLI output as a character vector.
#' @examples
#' set_workdir("~/data")
#' @export
th_set_workdir <- function(dir) {
  .send_command(sprintf("setwd(dir=\"%s\")", dir))
}


#' Get the current working directory from Threadle CLI
#'
#' @return The working directory as returned by Threadle.
#' @export
th_get_workdir <- function() {
  .send_command(sprintf("getwd()"))
  #out <- .send_command(sprintf("getwd()"))
  #out[1]
}

#' View a structure by variable in the Threadle CLI environment
#'
#' @param name Name of the structure to view.
#' @return CLI output.
#' @export
th_view <- function(structure) {
  name <- .th_name(structure)
  .send_command(sprintf("view(structure=%s)", name))
}

#' Create a new nodeset in Threadle and assign it to variable 'name' in the Threadle CLI environment
#'
#' @param name Name of the R variable to assign in the CLI environment.
#' @return A `threadle_nodeset` object.
#' @export
th_create_nodeset <- function(name) {
  .send_command(sprintf("%s = createnodeset()", name))
  structure(list(name=name), class="threadle_nodeset")
}

#' Create a new network in Threadle and assign it to variable 'name' in the Threadle CLI environment
#' Create a new network in Threadle
#'
#' @param name Name of the assigned variable in the Threadle CLI environment.
#' @param nodeset A `threadle_nodeset` object.
#' @param label Optional internal name of network in Threadle.
#'
#' @return A `threadle_network` object.
#' @export
th_create_network <- function(name, nodeset, label = NULL) {
  label_arg <- if (!is.null(label)) sprintf(",name=%s", label) else ""
  .send_command(sprintf("%s = createnetwork(nodeset=%s%s)", name, nodeset$name, label_arg))
  structure(list(name = name), class = "threadle_network")
}

#' Load a file into Threadle and assigns the structure(s) to the provided variable name in the Threadle CLI environment
#'
#' @param name Name of the assigned variable in the Threadle CLI environment.
#' @param file File path.
#' @param type Type of structure ("network" or "nodeset").
#'
#' @return An object with class corresponding to the loaded type.
#' @export
th_load_file <- function(name, file, type) {
  .send_command(sprintf("%s = loadfile(file=\"%s\", type=%s)",name, file, type))
  structure(list(name=name), class=paste0("threadle_",type))
}

#' Load a network structure from a file
#'
#' @param name Name for the network object.
#' @param file Path to the network file.
#'
#' @return A `threadle_network` object.
#' @export
th_load_network <- function(name, file) {
  .send_command(sprintf("%s = loadfile(file=\"%s\", type=network)",name, file))
  structure(list(name=name, nodeset=paste0(name,"_nodeset"), class="threadle_network"))
  #structure(list(name=paste0(name,"_nodeset"), class="threadle_nodeset"))
}

#' Retrieve meta information from a Threadle object
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object.
#' @param format Output format ("json"(default) or "console").
#'
#' @return Parsed JSON or raw CLI text.
#' @export
th_info <- function(structure, format="json") {
  name <- .th_name(structure)
  retval <- .send_command(sprintf("info(structure=%s, format=%s)",name, format))
  if (format =="json")
    fromJSON(retval)
  else
    retval
}

#' Add a node to a nodeset (or network)
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object.
#' @param id Node ID.
#'
#' @return CLI output.
#' @export
th_add_node <- function(structure, id) {
  name <- .th_name(structure)
  cli <- sprintf("addnode(structure=%s, id=%d)", name, id)
  .send_command(cli)
}

#' List all objects currently stored as variables in Threadle
#'
#' @param format Output format ("json"(default) or console").
#'
#' @return Parsed JSON or raw text.
#' @export
th_inventory <- function(format = "json") {
  retval <- .send_command(sprintf("i(format=json)"))
  if (format == "json")
    fromJSON(retval)
  else
    retval
}

#' Define an attribute for a nodeset (or network)
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object.
#' @param attrname Name of the attribute.
#' @param attrtype Attribute type ("int","float", "char" or "bool")
#'
#' @return CLI output.
#' @export
th_define_attr <- function(structure, attrname, attrtype) {
  cli <- sprintf("defineattr(structure=%s, attrname=%s, attrtype=%s)", structure$name, attrname, attrtype)
  .send_command(cli)
}

#' Set the value of a node attribute for a nodeset (or network)
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object.
#' @param nodeid Node ID.
#' @param attrname Attribute name.
#' @param attrvalue Value to assign.
#'
#' @return CLI output.
#' @export
th_set_attr <- function(structure, nodeid, attrname, attrvalue) {
  cli <- sprintf("setattr(structure=%s,nodeid=%d,attrname=%s,attrvalue=%s)",structure$name,nodeid,attrname, attrvalue)
  .send_command(cli)
}

#' Get the value of a node attribute for a nodeset (or network)
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object.
#' @param nodeid Node ID.
#' @param attrname Attribute name.
#'
#' @return CLI output.
#' @export
th_get_attr <- function(structure, nodeid, attrname) {
  name <- .th_name(structure)
  cli <- sprintf("getattr(structure=%s,nodeid=%d,attrname=%s)",name,nodeid,attrname)
  .send_command(cli)
}

#' Adds/defines a relational layer in a network
#'
#' @param network A `threadle_network` object.
#' @param layername Name of the layer.
#' @param mode Layer mode (1 or 2).
#' @param directed Logical; whether ties are directed (only for 1-mode layers).
#' @param valuetype "binary" or "valued" (only for 1-mode layers).
#' @param selfties Logical; whether self-ties are allowed (only for 1-mode layers).
#'
#' @return CLI output.
#' @export
th_add_layer <- function(network, layername, mode, directed=FALSE, valuetype="binary", selfties=FALSE) {
  cli <- sprintf("addlayer(network=%s, layername=%s, mode=%d, directed=%s, valuetype=%s, selfties=%s)", network$name, layername, mode, directed,valuetype,selfties)
  .send_command(cli)
}

#' Get the number of nodes in a structure
#'
#' @param name Name of the structure (can be a network or nodeset).
#'
#' @return A numeric value.
#' @export
th_get_nbr_nodes <- function(structure) {
  name <- .th_name(structure)
  cli <- sprintf("getnbrnodes(structure=%s)",name)
  as.numeric(.send_command(cli))
}

#' Get a node ID by index
#'
#' @param name Name of the structure (can be network or nodeset).
#' @param index Numeric index.
#'
#' @return The node ID.
#' @export
th_get_nodeid_by_index <- function(structure, index) {
  name <- .th_name(structure)
  cli <- sprintf("getnodeidbyindex(structure=%s, index=%d)",name, index)
  as.numeric(.send_command(cli))
}

#' Get alters of a node within a network layer
#'
#' @param name Name of the network.
#' @param layername Layer to query.
#' @param nodeid Node ID.
#' @param direction Tie direction ("both", "in", "out").
#'
#' @return Parsed JSON list of alters.
#' @export
th_get_node_alters <- function(name,layername,nodeid,direction="both") {
  cli <- sprintf("getnodealters(network=%s, layername=%s,nodeid=%d, direction=%s)",name, layername, nodeid, direction)
  fromJSON(.send_command(cli))
}

#' Get a random alter for a node
#'
#' @param name Network name.
#' @param nodeid Node ID.
#' @param layername Optional layer. If left blank, will pick from all layers
#' @param direction Direction ("both"(default), "in", "out").
#' @param balanced Whether selection should be balanced for multiple layers.
#'
#' @return A node ID (numeric).
#' @export
th_get_random_alter <- function(network, nodeid, layername="", direction="both", balanced=FALSE) {
  name <- .th_name(network)
  cli <- sprintf("getrandomalter(network=%s, nodeid=%d, layername=%s, direction=%s, balanced=%s)",name, nodeid, layername,direction,balanced)
  as.numeric(.send_command(cli))
}

#' Get a random node from a structure
#'
#' @param name Structure name.
#' @return A node ID (numeric).
#' @export
th_get_random_node <- function(structure) {
  name <- .th_name(structure)
  cli <- sprintf("getrandomnode(structure=%s)",name)
  as.numeric(.send_command(cli))
}

#' Remove a node from a network and its nodeset
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object.
#' @param nodeid Node ID.
#'
#' @return CLI output.
#' @export
th_remove_node <- function(structure, nodeid) {
  name <- .th_name(structure)
  cli <- sprintf("removenode(structure = %s, nodeid = %d)",name,nodeid)
  as.numeric(.send_command(cli))
}

#---

# th_help <- function(){
#   cli <- sprintf("help")
#   .send_command(cli)
# }

th_exit <- function(){
  
}

#---

#' Add an affiliation edge (from node to hyperedge) in a network layer
th_add_aff <- function(network, layername, nodeid, hypername, 
                       addmissingnode = TRUE, addmissingaffiliation = TRUE) {
  name <- .th_name(network)
  cli <- sprintf("addaff(network=%s, layername=%s, nodeid=%d, hypername=%s, addmissingnode=%s, addmissingaffiliation=%s)",
                 name, layername, nodeid, hypername, addmissingnode, addmissingaffiliation)
  .send_command(cli)
}

#' Add an edge to a network
#'
#' @param network A `threadle_nodeset` or `threadle_network` object.
#' @param id Node ID.
#'
#' @return CLI output.
#' @export
th_add_edge <- function(network, layername, node1id, node2id,
                        value = 1, addmissingnodes = TRUE) {
  name <- .th_name(network)
  cli <- sprintf("addedge(network=%s, layername=%s, %d, %d, value=%d, addmissingnodes=%s)",
                 name, layername, node1id, node2id, value, addmissingnodes)
  .send_command(cli)
}

#' Add a hyperedge to a specified layer of a network (the hyperedge set of a 2-mode layer).
#'
#' @param network A `threadle_network` object.
#' @param id Node ID.
#'
#' @return CLI output.
#' @export
th_add_hyper <- function(network, layername, hypername,
                         nodes = c(1,2,3), addmissingnodes = FALSE) {
  name <- .th_name(network)
  nodes_arg <- ""
  if (!is.null(nodes)) {
    nodes <- as.integer(nodes)
    nodes_str <- paste(nodes, collapse = ";")
    nodes_arg <- sprintf(", nodes=%s", nodes_str)
  }
  cli <- sprintf(
    "addhyper(network=%s, layername=%s, hypername=%s, nodes = %s, addmissingnodes=%s)",
    name, layername, hypername, nodes_arg, addmissingnodes
  )
  .send_command(cli)
}

#' check edge if existed
th_check_edge <- function(network, layername, nodeid1, nodeid2) {
  name <- .th_name(network)
  cli <- sprintf("checkedge(network=%s, layername=%s, %d, %d)", name, layername, nodeid1, nodeid2)
  .send_command(cli)
}

#' clear all nodes in one layer
th_clear_layer <- function(network, layername) {
  name <- .th_name(network)
  cli <- sprintf("clearlayer(network=%s, layername=%s)", name, layername)
  .send_command(cli)
}

#' Calculates the degree centrality for the specified layer and network
th_degree <- function(network, layername, attrname = NULL, direction = "in") {
  name <- .th_name(network)
  attr_part <- if (!is.null(attrname)) attrname else ""
  cli <- sprintf(
    "degree(network=%s, layername=%s, attrname=%s, direction=%s)",
    name, layername, attr_part, direction
  )
  .send_command(cli)
}

#' Graph density
#' 
#' Computes the density of a layer in a Threadle network by calling the Threadle
#' CLI command `density(network=..., layername=...)`.
#' 
#' #' Density is returned as a numeric scalar, typically defined as the ratio of the
#' number of observed edges to the maximum possible number of edges for a simple
#' graph (i.e., assuming no multi-edges). The exact treatment of self-loops and
#' directionality follows Threadle's `density` implementation.
#'
#' @param network A Threadle network object or the name of a network variable.
#' @param layername Name of the layer (string or Threadle string variable) for
#'   which to compute density.
#' @return A numeric scalar giving the layer density.
#' @export
th_density <- function(network, layername) {
  network <- .th_name(network)
  cli <- sprintf("density(network = %s, layername = %s)",network,layername)
  as.numeric(.send_command(cli))
}

#' dichotomize
th_dichotomize <- function(network, layername,
                           cond = "ge", threshold = 1,
                           truevalue = 1, falsevalue = 0,
                           newlayername = NULL) {
  name <- .th_name(network)
  newlayer_arg <- if (!is.null(newlayername)) newlayername else ""
  cli <- sprintf(
    "dichotomize(network=%s, layername=%s, cond=%s, threshold=%s, truevalue=%s, falsevalue=%s, newlayername = %s)",
    name, layername, cond, threshold,
    truevalue, falsevalue, newlayer_arg
  )
  .send_command(cli)
}

#' filter a nodeset to get a new filtered nodeset.
th_filter <- function(name, nodeset, attrname, cond, attrvalue) {
  nodeset_name <- .th_name(nodeset)
  cli <- sprintf(
    "%s = filter(nodeset=%s, attrname=%s, cond=%s, attrvalue=%s)",
    name, nodeset_name, attrname, cond, attrvalue
  )
  .send_command(cli)
  structure(list(name = name), class = "threadle_nodeset")
}


#' generate
th_generate <- function(name, size, p, directed = TRUE, selfties = FALSE) {
  cli <- sprintf(
    "%s = generate(type=er, size=%d, p=%s, directed=%s, selfties=%s, newname=%s)",
    name, size, as.character(p), directed, selfties, name
  )
  .send_command(cli)
  structure(list(name = name), class = "threadle_network")
}

#' get edge value
th_get_edge <- function(network, layername, node1id, node2id) {
  name <- .th_name(network)
  cli <- sprintf(
    "getedge(network=%s, layername=%s, %d, %d)",
    name, layername, node1id, node2id
  )
  .send_command(cli)
}

# Remove something
th_remove <- function(structure) {
  name <- .th_name(structure)
  cli <- sprintf("remove(structure=%s)", name)
  .send_command(cli)
}

# Remove everything!
th_remove_all <- function() {
  cli <- sprintf("removeall()")
  .send_command(cli)
}

#' Remove affiliation
th_remove_aff <- function(network, layername, nodeid, hypername) {
  name <- .th_name(network)
  cli <- sprintf(
    "removeaff(network=%s, layername=%s, nodeid=%d, hypername=%s)",
    name, layername, nodeid, hypername
  )
  .send_command(cli)
}

#' remove an attribute
th_remove_attr <- function(structure, nodeid, attrname) {
  name <- .th_name(structure)
  cli <- sprintf("removeattr(structure=%s, nodeid=%d, attrname=%s)", name, nodeid, attrname)
  .send_command(cli)
}

#' Remove an edge from a network layer
#' 
#' Removes an edge between two nodes from a specified layer in a network.
#'
#' @param network A network object or the name of a network variable.
#' @param layername The name of the layer.
#' @param node1id The ID of the first node.
#' @param node2id The ID of the second node.
#' @return Invisibly returns the result of the remove operation.
#' @export
th_remove_edge <- function(network, layername, node1id, node2id) {
  name <- .th_name(network)
  cli <- sprintf("removeedge(network=%s, layername=%s, %d, %d)",
                 name, layername, node1id, node2id)
  .send_command(cli)
}

#' Remove hyperedge
th_remove_hyper <- function(network, layername, hypername) {
  name <- .th_name(network)
  cli <- sprintf(
    "removehyper(network=%s, layername=%s, hypername=%s)",
    name, layername, hypername
  )
  .send_command(cli)
}

#' Remove layer
th_remove_layer <- function(network, layername) {
  name <- .th_name(network)
  cli <- sprintf("removelayer(network=%s, layername=%s)", network$name, layername)
  .send_command(cli)
}


#' save a sturcture into a file
th_save_file <- function(structure, file = "") {
  name <- .th_name(structure)
  if (!nzchar(file)) file <- paste0(name, ".tsv")
  cli <- sprintf("savefile(structure=%s, file=\"%s\")", name, file)
  .send_command(cli)
}

#' setting
th_setting <- function(name, value) {
  cli <- sprintf("setting(name=%s, value=%s)", name, value)
  .send_command(cli)
}

#' subnet
th_subnet <- function(name, network, nodeset) {
  network_name <- .th_name(network)
  nodeset_name <- .th_name(nodeset)
  cli <- sprintf("%s = subnet(network=%s, nodeset=%s)", name, network_name, nodeset_name)
  .send_command(cli)
  structure(list(name = name), class = "threadle_network")
}

#' undefine_attr
th_undefine_attr <- function(structure, attrname) {
  name <- .th_name(structure)
  cli <- sprintf("undefineattr(structure=%s, attrname=%s)", name, attrname)
  .send_command(cli)
}
