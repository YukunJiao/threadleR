# R client package for Threadle CLIconsole

library(processx)

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
  if (length(cmd) == 0) return(character(0)) # cmd <- character(0)
  if (!nzchar(cmd)) return(character(0))
  #print(cmd)
  proc <- get(".threadle_proc", envir=.GlobalEnv)
  proc$write_input(paste0(cmd,"\n"))
  #print("ok, sent")
  out <- character()
  repeat {
    new <- proc$read_output_lines()
    # print(new)
    if (length(new) > 0) {
      # cat("RAW new:\n")
      # dput(new)

      out <- c(out, new)
      #out
      if (any(new %in% c("__END__", "> __END__"))) {
        out <- out[!(out %in% c("__END__", "> __END__"))]  # drop the marker
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
#' @param x description
.th_name <- function(x) {
  if (is.character(x) && length(x) == 1) return(x)
  if (is.list(x) && !is.null(x$name) && is.character(x$name) && length(x$name) == 1) return(x$name)
  stop("Expected a Threadle structure object (with $name) or a single character name.")
}

#' Nodeid error
#'
#' @param name description
.th_normalize_nodeid <- function(nodeid) {
  if (length(nodeid) != 1) stop("`nodeid` must be length 1.", call. = FALSE)
  if (is.character(nodeid)) {
    if (!nzchar(nodeid) || !grepl("^[0-9]+$", nodeid)) {
      stop("`nodeid` must be an integer (or a numeric string like \"123\").", call. = FALSE)
    }
    nodeid <- as.integer(nodeid)
  } else if (is.numeric(nodeid)) {
    if (is.na(nodeid)) stop("`nodeid` cannot be NA.", call. = FALSE)
    if (nodeid %% 1 != 0) stop("`nodeid` must be an integer.", call. = FALSE)
    if (nodeid < 0) stop("`nodeid` must be >= 0.", call. = FALSE)
    nodeid <- as.integer(nodeid)
  } else {
    stop("`nodeid` must be numeric or character.", call. = FALSE)
  }
  nodeid
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
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a nodeset in the Threadle CLI environment.
#' @param label Optional internal name of network in Threadle.
#'
#' @return A `threadle_network` object.
#' @export
th_create_network <- function(name, nodeset, label = NULL) {
  nodeset_name <- .th_name(nodeset)
  label_arg <- if (!is.null(label)) sprintf(",name=%s", label) else ""
  .send_command(sprintf("%s = createnetwork(nodeset=%s%s)", name, nodeset_name, label_arg))
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
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param format Output format ("json"(default) or "console").
#'
#' @return Parsed JSON or raw CLI text.
#' @export
th_info <- function(structure, format="json") {
  name <- .th_name(structure)
  retval <- .send_command(sprintf("info(structure=%s, format=%s)",name, format))
  print(retval)
  if (format =="json")
    jsonlite::fromJSON(retval)
  else
    retval
}

#' Add a node to a nodeset (or network)
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
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
    jsonlite::fromJSON(retval)
  else
    retval
}

#' Define an attribute for a nodeset (or network)
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param attrname Attribute name.
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
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
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
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
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
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param mode Layer mode (1 or 2).
#' @param directed Logical; whether ties are directed (only for 1-mode layers).
#' @param valuetype "binary" or "valued" (only for 1-mode layers).
#' @param selfties Logical; whether self-ties are allowed (only for 1-mode layers).
#'
#' @return CLI output.
#' @export
th_add_layer <- function(network, layername, mode, directed=FALSE, valuetype="binary", selfties=FALSE) {
  name <- .th_name(network)
  cli <- sprintf("addlayer(network=%s, layername=%s, mode=%d, directed=%s, valuetype=%s, selfties=%s)", name, layername, mode, directed,valuetype,selfties)
  .send_command(cli)
}

#' Get the number of nodes in a structure
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
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
  jsonlite::fromJSON(.send_command(cli))
}

#' Get a random alter for a node
#'
#' `th_get_random_alter()` get a random alter for a node.
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
  nodeid <- .th_normalize_nodeid(nodeid)
  cli <- sprintf("getrandomalter(network=%s, nodeid=%d, layername=%s, direction=%s, balanced=%s)",name, nodeid, layername,direction,balanced)
  as.numeric(.send_command(cli))
}

#' Get a random node from a structure
#'
#' `th_get_random_node()` gets a random node.
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
#' `th_remove_node()` removes a node from a structure.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param nodeid Node ID.
#'
#' @return CLI output.
#' @export
th_remove_node <- function(structure, nodeid) {
  name <- .th_name(structure)
  cli <- sprintf("removenode(structure = %s, nodeid = %d)",name,nodeid)
  as.numeric(.send_command(cli))
}

#' Add an affiliation (hyperedge) in a 2-mode layer
#'
#' `th_add_aff()` adds a hyperedge in a 2-mode layer. Being similar to `th_add_hyper`
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param nodeid Node ID.
#' @param hypername Name of the hyperedge.
#' @param addmissingnode Logical; if `TRUE`, missing nodes are created automatically.
#' @param addmissingaffiliation Logical; if `TRUE`, missing affiliations are created automatically.
#'
#' @return CLI output.
#' @export
th_add_aff <- function(network, layername, nodeid, hypername,
                       addmissingnode = TRUE, addmissingaffiliation = TRUE) {
  name <- .th_name(network)
  cli <- sprintf("addaff(network=%s, layername=%s, nodeid=%d, hypername=%s, addmissingnode=%s, addmissingaffiliation=%s)",
                 name, layername, nodeid, hypername, addmissingnode, addmissingaffiliation)
  .send_command(cli)
  invisible(name)
}

#' Add an edge to a network
#'
#' `th_add_edge()` adds an edge between two nodes in a layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id The source node (from).
#' @param node2id The destination node (to).
#' @param value Edge value, defaults to 1.
#' @param addmissingnodes Logical; if `TRUE`, missing nodes are created and added. Defaults to `TRUE`.
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

#' Add a hyperedge to a specified layer of a network.
#'
#' `th_add_hyper()` adds a hyperedge to the hyperedge set of a 2-mode layer
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param hypername Name of the hyperedge.
#' @param nodes Vector of node to attach to the hyperedge. If `NULL`,
#'   creates the hyperedge without adding nodes.
#' @param addmissingnodes Logical; if `TRUE`, missing nodes are created and added. Defaults to `FALSE`.
#'
#' @return CLI output.
#' @export
th_add_hyper <- function(network, layername, hypername,
                         nodes = c(), addmissingnodes = FALSE) {
  name <- .th_name(network)
  nodes_arg <- ""
  if (!is.null(nodes)) {
    nodes_arg <- paste(nodes, collapse = ";")
  }
  cli <- sprintf(
    "addhyper(network=%s, layername=%s, hypername=%s, nodes = %s, addmissingnodes=%s)",
    name, layername, hypername, nodes_arg, addmissingnodes
  )
  return(cli)
  .send_command(cli)
}

#' Check whether an edge exists in a layer
#'
#' `th_check_edge()` checks whether an edge between
#' two nodes exists in the specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id The source node (from).
#' @param node2id The destination node (to).
#'
#' @return CLI output.
#' @export
th_check_edge <- function(network, layername, node1id, node2id) {
  name <- .th_name(network)
  cli <- sprintf("checkedge(network=%s, layername=%s, %d, %d)", name, layername, node1id, node2id)
  .send_command(cli)
}

#' Clear all edges in a layer
#'
#' `th_clear_layer()` removes all edges from the
#' specified layer while keeping the layer definition.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @return CLI output.
#' @export
th_clear_layer <- function(network, layername) {
  name <- .th_name(network)
  cli <- sprintf("clearlayer(network=%s, layername=%s)", name, layername)
  .send_command(cli)
}

#' Calculate the degree centrality for a layer
#'
#' `th_degree()` computes the degree centrality for the specified network and layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param attrname attribute name.
#' @param direction Edge direction: `"in"`, `"out"`, or `"both"`. Defaults to `"in"`.
#' @return CLI output.
#' @export
th_degree <- function(network, layername, attrname = NULL, direction = "in") {
  name <- .th_name(network)
  attr_part <- if (!is.null(attrname)) attrname else ""
  cli <- sprintf(
    "degree(network=%s, layername=%s, attrname=%s, direction=%s)",
    name, layername, attr_part, direction
  )
  .send_command(cli)
}

#' Calculate density of a layer
#'
#' `th_density()` computes the density of a layer in a Threadle network.
#'
#' Density is returned as a numeric scalar, typically defined as the ratio of the
#' number of observed edges to the maximum possible number of edges for a simple
#' graph (i.e., assuming no multi-edges). The exact treatment of self-loops and
#' directionality follows Threadle's `density` implementation.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @return A numeric scalar giving the layer density.
#' @export
th_density <- function(network, layername) {
  network <- .th_name(network)
  cli <- sprintf("density(network = %s, layername = %s)",network,layername)
  as.numeric(.send_command(cli))
}

#' Dichotomize a layer
#'
#' `dichotomize()` creates a recoded version of
#' a layer based on a threshold rule.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param cond Comparison operator: `"eq"`, `"ne"`, `"gt"`, `"lt"`, `"ge"`, or `"le"`.
#'   Defaults to `"ge"`.
#' @param threshold Threshold used with `cond`. Defaults to `1`.
#' @param truevalue Value to assign when the condition is `TRUE`. Defaults to `1`.
#' @param falsevalue Value to assign when the condition is `FALSE`. Defaults to `0`.
#' @param newlayername Optional name for the new layer.
#' @return CLI output.
#' @export
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

#' Filter a nodeset by an attribute condition
#'
#' `filter()` creates a new nodeset based names and values of attributes.
#'
#' @param name Name of the new nodeset variable
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a nodeset in the Threadle CLI environment.
#' @param attrname Attribute name.
#' @param cond Condition operator: `"eq"`, `"ne"`, `"gt"`, `"lt"`, `"ge"`, `"le"`,
#'   `"isnull"`, or `"notnull"`.
#' @param attrvalue Attribute value used for the condition.
#' @return A `threadle_nodeset`
#' @export
th_filter <- function(name, nodeset, attrname, cond, attrvalue) {
  nodeset_name <- .th_name(nodeset)
  cli <- sprintf(
    "%s = filter(nodeset=%s, attrname=%s, cond=%s, attrvalue=%s)",
    name, nodeset_name, attrname, cond, attrvalue
  )
  .send_command(cli)
  structure(list(name = name), class = "threadle_nodeset")
}


#' Generate an Erdős–Rényi random network
#'
#' `generate()` creates a random
#' network and store it under `name`
#'
#' @param name Name of the new network variable to create.
#' @param size Number of nodes.
#' @param p Edge probability.
#' @param directed Logical; whether the generated network is directed. Defaults to `TRUE`.
#' @param selfties Logical; whether self-edges are allowed. Defaults to `FALSE`.
#'
#' @return A `threadle_network`.
#' @export
th_generate <- function(name, size, p, directed = TRUE, selfties = FALSE) {
  cli <- sprintf(
    "%s = generate(type=er, size=%d, p=%s, directed=%s, selfties=%s, newname=%s)",
    name, size, as.character(p), directed, selfties, name
  )
  .send_command(cli)
  structure(list(name = name), class = "threadle_network")
}

#' Get an edge value
#'
#' `getedge()` retrieves the edge value between
#' two nodes in the specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id The source node (from).
#' @param node2id The destination node (to).
#' @return CLI output.
#' @export
th_get_edge <- function(network, layername, node1id, node2id) {
  name <- .th_name(network)
  cli <- sprintf(
    "getedge(network=%s, layername=%s, %d, %d)",
    name, layername, node1id, node2id
  )
  .send_command(cli)
}


#' Remove an affiliation (node -> hyperedge) from a layer
#'
#' `th_remove_aff()` removes an affiliation between
#' a node and a hyperedge in the specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param nodeid Node ID.
#' @param hypername Name of the hyperedge.
#' @return CLI output.
#' @export
th_remove_aff <- function(network, layername, nodeid, hypername) {
  name <- .th_name(network)
  cli <- sprintf(
    "removeaff(network=%s, layername=%s, nodeid=%d, hypername=%s)",
    name, layername, nodeid, hypername
  )
  .send_command(cli)
}

#' Remove an attribute value from a node
#'
#' `th_remove_attr()` removes an attribute value for
#' a given node.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param nodeid Node ID.
#' @param attrname Attribute name.
#'
#' @return CLI output.
#' @export
th_remove_attr <- function(structure, nodeid, attrname) {
  name <- .th_name(structure)
  cli <- sprintf("removeattr(structure=%s, nodeid=%d, attrname=%s)", name, nodeid, attrname)
  .send_command(cli)
}

#' Remove an edge from a layer
#'
#' `th_remove_edge` removes an edge between two
#' nodes in the specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id The source node (from).
#' @param node2id The destination node (to).
#' @return Invisibly returns the result of the remove operation.
#' @export
th_remove_edge <- function(network, layername, node1id, node2id) {
  name <- .th_name(network)
  cli <- sprintf("removeedge(network=%s, layername=%s, %d, %d)",
                 name, layername, node1id, node2id)
  .send_command(cli)
}

#' Remove a hyperedge from a 2-mode layer
#'
#' `th_remove_hyper()` remove a hyperedge from the
#' specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param hypername Name of the hyperedge.
#' @return CLI output.
#' @export
th_remove_hyper <- function(network, layername, hypername) {
  name <- .th_name(network)
  cli <- sprintf(
    "removehyper(network=%s, layername=%s, hypername=%s)",
    name, layername, hypername
  )
  .send_command(cli)
}

#' Remove a layer from a network
#'
#' `th_remove_layer()` removes a layer
#' (and its ties) from the network.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @return CLI output.
#' @export
th_remove_layer <- function(network, layername) {
  name <- .th_name(network)
  cli <- sprintf("removelayer(network=%s, layername=%s)", network$name, layername)
  .send_command(cli)
}


#' Save a structure to file
#'
#' `th_save_file` saves a network or nodeset.
#' If `file` is `""`, the file name defaults to `<structure-name>.tsv`.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param file Output file path. Defaults to `""`.
#' @return CLI output.
#' @export
th_save_file <- function(structure, file = "") {
  name <- .th_name(structure)
  if (!nzchar(file)) file <- paste0(name, ".tsv")
  cli <- sprintf("savefile(structure=%s, file=%s)", name, shQuote(file, "cmd2"))
  .send_command(cli)
  print(cli)
}

#' Set a Threadle backend setting
#'
#' `th_setting()` changes setting in the Threadle backend.
#'
#' @param name Setting name.
#' @param value Setting value (typically `"true"` or `"false"`).
#'
#' @return CLI output.
#' @export
th_setting <- function(name, value) {
  cli <- sprintf("setting(name=%s, value=%s)", name, value)
  print(cli)
  .send_command(cli)
}

#' Create a subnet from a network and a nodeset
#'
#' `th_subnet()` constructs a new network induced
#' by the nodes in `nodeset` and stores it under `name`.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network.
#'
#' @param name Name of the new network variable to create.
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @return CLI output.
#' @export
th_subnet <- function(name, network, nodeset) {
  network_name <- .th_name(network)
  nodeset_name <- .th_name(nodeset)
  cli <- sprintf("%s = subnet(network=%s, nodeset=%s)", name, network_name, nodeset_name)
  .send_command(cli)
  structure(list(name = name), class = "threadle_network")
}

#' Undefine an attribute on a structure
#'
#' `th_undefine_attr()` removes an attribute
#' from a nodeset or network.
#'
#' @param attrname Attribute name.
#'
#' @return CLI output.
#' @export
th_undefine_attr <- function(structure, attrname) {
  name <- .th_name(structure)
  cli <- sprintf("undefineattr(structure=%s, attrname=%s)", name, attrname)
  .send_command(cli)
}

# th_help <- function(){
#   cli <- sprintf("help")
#   .send_command(cli)
# }

# th_exit <- function() {}


#' Remove a structure
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @return CLI output.

# th_remove <- function(structure) {
#   name <- .th_name(structure)
#   cli <- sprintf("remove(structure=%s)", name)
#   .send_command(cli)
# }

#' Remove all structures
#'
#' `removeall()` removes all stored variables.
#'
#' @return CLI output.

# th_remove_all <- function() {
#   cli <- sprintf("removeall()")
#   .send_command(cli)
# }
