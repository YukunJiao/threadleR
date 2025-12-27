# R client package for Threadle CLIconsole

library(processx)

#' Set name and structure name
#'
#' @param x description
.th_name <- function(x) {
  if (is.character(x) && length(x) == 1) return(x)
  if (is.list(x) && !is.null(x$name) && is.character(x$name) && length(x$name) == 1) return(x$name)
  stop("Expected a Threadle structure object (with $name) or a single character name.")
}

#'
.th_args <- function(env, drop = character()) {
  fmls <- names(formals(sys.function(sys.parent())))
  keep <- setdiff(fmls, drop)
  args <- as.list(env)[keep]

  if ("name" %in% names(args)) args$name <- .th_name(args$name)
  if ("network" %in% names(args)) args$network <- .th_name(args$network)
  if ("structure" %in% names(args)) args$structure <- .th_name(args$structure)
  args
}

#' json cmd
.th_json_cmd <- function(command, args = NULL, assign = NULL) {
  dto <- list(
    Assign  = if (!is.null(assign)) as.character(assign) else NULL,
    Command = as.character(command),
    Args    = NULL
  )

  if (!is.null(args)) {
    args <- lapply(args, function(x) {
      if (is.null(x)) "" else as.character(x)
    })
    dto$Args <- args
  }

  jsonlite::toJSON(dto, auto_unbox = TRUE, null = "null")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

.th_stop_if_fail <- function(resp) {
  if (is.null(resp) || is.null(resp$Success)) {
    stop("Invalid JSON response from Threadle.", call. = FALSE)
  }
  if (!isTRUE(resp$Success)) {
    stop(sprintf("[%s] %s",
                 resp$Code %||% "Error",
                 resp$Message %||% "Threadle error"),
         call. = FALSE)
  }
  invisible(resp)
}

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
  proc <- process$new(path, args=c("--json", "--silent"), stdin="|", stdout="|", stderr = "|")
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
  if (length(cmd) == 0) return(NULL)
  if (!nzchar(cmd)) return(NULL)

  proc <- get(".threadle_proc", envir = .GlobalEnv)
  proc$write_input(paste0(cmd, "\n"))

  out <- character()
  repeat {
    new <- proc$read_output_lines()

    if (length(new) > 0) {
      out <- c(out, new)

      for (line in new) {
        s <- sub("^\\s*>\\s*", "", line)
        s <- trimws(s)

        if (grepl("^\\{.*\\}$", s)) {
          resp <- tryCatch(
            jsonlite::fromJSON(s, simplifyVector = TRUE),
            error = function(e) NULL
          )
          if (!is.null(resp)) {
            if (isTRUE(getOption("threadle.debug_raw", FALSE))) {
              attr(resp, "raw_lines") <- out
            }
            return(resp)
          }
        }
      }
    } else {
      Sys.sleep(0.01)
    }
  }
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

#' Call the Threadle function
.th_call <- function(cmd,
                     args = NULL,
                     assign = NULL,
                     return = getOption("threadle.return", "payload"),
                     .print_message = getOption("threadle.print_message", TRUE)) {
  return <- match.arg(return)
  cmd <- .th_json_cmd(command = cmd, args = args, assign = assign)
  print(cmd)
  resp <- .send_command(cmd)
  .th_stop_if_fail(resp)

  if (.print_message && !is.null(resp$Message) && nzchar(resp$Message)) {
    message(resp$Message)
  }

  if (return == "payload") resp$Payload else resp
}

#' Set working directory inside the Threadle CLI environment
#'
#' @param dir Path to the directory.
#' @return CLI output as a character vector.
#' @examples
#' set_workdir("~/data")
#' @export
th_set_workdir <- function(dir) {
  invisible(.th_call(cmd = "setwd", args = list(dir = dir)))
}


#' Get the current working directory from Threadle CLI
#'
#' @return The working directory as returned by Threadle.
#' @export
th_get_workdir <- function() {
  .th_call(cmd = "getwd")
  #out <- .th_call(sprintf("getwd()"))
  #out[1]
}

#' View a structure by variable in the Threadle CLI environment
#'
#' @param name Name of the structure to view.
#' @return CLI output.
#' @export
th_view <- function(structure) {
  name <- .th_name(structure)
  .th_call(cmd = "view", structure = name)
}

#' Create a new nodeset in Threadle and assign it to variable 'name' in the Threadle CLI environment
#'
#' @param name Name of the R variable to assign in the CLI environment.
#' @return A `threadle_nodeset` object.
#' @export
th_create_nodeset <- function(name) {
  .th_call("createnodeset", assign = name)
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
  name <- .th_name(name)
  nodeset <- .th_name(nodeset)
  label <- if (!is.null(label)) sprintf(",name=%s", label) else ""
  .th_call(cmd = "createnetwork", args = list(name, nodeset, label))
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
  args <- .th_args(environment(), drop = "name")
  cmd <- "loadfile"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name=name), class=paste0("threadle_",type))
}

#' Load a network structure from a file
#'
#' @param name Name for the network object.
#' @param file Path to the network file.
#'
#' @return A `threadle_network` object.
#' @export
th_load_network <- function(name, file, type = "network") {
  args <- .th_args(environment())
  cmd <- "loadfile"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name=name), class=paste0("threadle_",type))
}

#' Retrieve meta information from a Threadle object
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param format Output format ("json"(default) or "console").
#'
#' @return Parsed JSON or raw CLI text.
#' @export
th_info <- function(structure, format = "json") {
  cmd <- "info"
  args <- list(structure = .th_name(structure), format = format)
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "addnode"
  assign <- NULL
  invisible(.th_call(cmd = cmd, args = args, assign = assign))
}

#' List all objects currently stored as variables in Threadle
#'
#' @param format Output format ("json"(default) or console").
#'
#' @return Parsed JSON or raw text.
#' @export
th_inventory <- function() {
  cmd <- "i"
  args <- NULL
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  cmd <- "defineattr"
  args <- list(structure = .th_name(structure), attrname = attrname, attrtype = attrtype)
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  .th_call(cli)
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
  args <- .th_args(environment())
  cmd <- "getattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "addlayer"
  assign <- NULL
  invisible(.th_call(cmd = cmd, args = args, assign = assign))
}

#' Get the number of nodes in a structure
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @return A numeric value.
#' @export
th_get_nbr_nodes <- function(structure) {
  cmd <- "getnbrnodes"
  args <- list(structure = .th_name(structure))
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a node ID by index
#'
#' @param name Name of the structure (can be network or nodeset).
#' @param index Numeric index.
#'
#' @return The node ID.
#' @export
th_get_nodeid_by_index <- function(structure, index) {
  cmd <- "getnodeidbyindex"
  args <- list(structure = .th_name(structure), index = index)
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  jsonlite::fromJSON(.th_call(cli))
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
  args <- as.list(environment())
  args$network <- .th_name(args$network)
  cmd <- "getrandomalter"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a random node from a structure
#'
#' `th_get_random_node()` gets a random node.
#'
#' @param name Structure name.
#' @return A node ID (numeric).
#' @export
th_get_random_node <- function(structure) {
  args <- .th_args(environment())
  cmd <- "getrandomnode"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "removenode"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "addaff"
  assign <- NULL
  invisible(.th_call(cmd = cmd, args = args, assign = assign))
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
  args <- .th_args(environment())
  cmd <- "addedge"
  assign <- NULL
  invisible(.th_call(cmd = cmd, args = args, assign = assign))
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
  args <- .th_args(environment())
  args$nodes <- if (is.null(args$nodes)) "" else paste(args$nodes, collapse = ";")
  cmd <- "addhyper"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = NULL)
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
  args <- .th_args(environment())
  cmd <- "checkedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "clearlayer"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "degree"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "density"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "dichotomize"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment(), drop = "name")
  cmd <- "filter"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
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
th_generate <- function(name, size, p, directed = TRUE, selfties = FALSE, type = "network") {
  args <- .th_args(environment())
  cmd <- "generate"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "getedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "removeaff"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "removeattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "removeedge"
  assign <- NULL
  invisible(.th_call(cmd = cmd, args = args, assign = assign))
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
  args <- .th_args(environment())
  cmd <- "removehyper"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  cmd <- "removelayer"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  args <- .th_args(environment())
  if (!nzchar(file)) args$file <- shQuote(paste0(args$structure, ".tsv"), "cmd2")
  cmd <- "savefile"
  assign <- NULL
  invisible(.th_call(cmd = cmd, args = args, assign = assign))
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
  args <- .th_args(environment())
  cmd <- "setting"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
  .th_call(cli)
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
  args <- .th_args(environment())
  cmd <- "undefineattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

# th_help <- function(){
#   cli <- sprintf("help")
#   .th_call(cli)
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
#   .th_call(cli)
# }

#' Remove all structures
#'
#' `removeall()` removes all stored variables.
#'
#' @return CLI output.

# th_remove_all <- function() {
#   cli <- sprintf("removeall()")
#   .th_call(cli)
# }
