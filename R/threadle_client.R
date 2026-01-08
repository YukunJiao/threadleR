# R client package for Threadle CLIconsole

#' Coerce Threadle structure inputs to a backend name
#'
#' Internal helper to normalize inputs that can be either:
#' a single character string (already a backend variable name),
#' or a Threadle structure object (a list with a scalar character `$name`).
#'
#' @param x A single character name, or a Threadle structure object with `$name`.
#' @return A length-1 character string giving the backend variable name.
#' @keywords internal
.th_name <- function(x) {
  if (is.character(x) && length(x) == 1) return(x)
  if (is.list(x) && !is.null(x$name) && is.character(x$name) && length(x$name) == 1) return(x$name)
  stop("Expected a Threadle structure object (with $name) or a single character name.")
}

#' Collect wrapper arguments for backend calls
#'
#' Internal helper that collects arguments from a wrapper's environment and
#' prepares them for sending to the Threadle backend.
#'
#' It keeps only the wrapper's formal arguments (optionally dropping some),
#' extracts their current values from `env`,
#' normalizes known structure parameters (`name`, `network`, `structure`)
#' to backend names via `.th_name()`.
#'
#' @param env An environment, usually `environment()` from the wrapper.
#' @param drop Character vector of argument names to drop.
#' @return A named list of arguments suitable for `.th_json_cmd()`.
#' @keywords internal
.th_args <- function(env, drop = character()) {
  fmls <- names(formals(sys.function(sys.parent())))
  keep <- setdiff(fmls, drop)
  args <- as.list(env)[keep]

  if ("name" %in% names(args)) args$name <- .th_name(args$name)
  if ("network" %in% names(args)) args$network <- .th_name(args$network)
  if ("structure" %in% names(args)) args$structure <- .th_name(args$structure)
  args
}

#' Build a JSON command DTO for the Threadle CLI backend
#'
#' Internal helper that constructs the JSON object sent to the Threadle CLI
#' process.
#'
#' @param command Command name (character scalar).
#' @param args Named list of arguments, or `NULL`.
#' @param assign Optional backend variable name to assign output to, or `NULL`.
#' @return A JSON string.
#' @keywords internal
.th_json_cmd <- function(command, args = NULL, assign = NULL) {
  mode <- getOption("threadle.command", default = "json")

  # plain cli mode
  if (identical(mode, "cli")) {
    cmd <- as.character(command)
    asg <- if (!is.null(assign) && nzchar(as.character(assign))) as.character(assign) else NULL

    fmt_val <- function(v) {
      if (is.null(v)) "" else as.character(v)
    }

    fmt_one <- function(nm, v) {
      v_out <- fmt_val(v)
      if (!is.null(nm) && nzchar(nm)) {
        paste0(nm, " = ", v_out)
      } else {
        v_out
      }
    }

    if (is.null(args) || length(args) == 0) {
      call_str <- paste0(cmd, "()")
    } else {
      nms <- names(args)
      if (is.null(nms)) nms <- rep("", length(args))

      parts <- Map(fmt_one, nms, args)

      # optional
      parts <- Filter(function(x) nzchar(x), parts)

      call_str <- paste0(cmd, "(", paste(parts, collapse = ", "), ")")
    }

    if (!is.null(asg)) return(paste0(asg, " = ", call_str))
    return(call_str)
  }

  # default json mode
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

#' Null-coalescing operator
#'
#' Returns `a` if it is not `NULL`, otherwise returns `b`.
#'
#' @name op-null-coalesce
#' @aliases %||%
#' @param a First value.
#' @param b Fallback value.
#' @return `a` if non-`NULL`, else `b`.
#' @keywords internal
NULL

#' @rdname op-null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Stop if backend response indicates failure
#'
#' Validates that the backend response is a structured JSON object with a
#' logical `Success` field and throws an R error when `Success` is not `TRUE`.
#'
#' @param resp Parsed JSON response (a list) from `.send_command()`.
#' @return Invisibly returns `resp` when successful.
#' @keywords internal
.th_stop_if_fail <- function(resp) {
  mode <- getOption("threadle.command", "json")

  if (identical(mode, "cli")) {
    if (is.null(resp)) {
      stop("No response from Threadle (CLI mode).", call. = FALSE)
    }
    return(invisible(resp))
  }

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

#' Send a command to the Threadle CLI
#'
#' Internal helper function used by all wrappers. Sends a JSON command and
#' waits for a complete one-line JSON response. It also support CLI command now.
#'
#' @param cmd A JSON command string to send to the Threadle CLI process.
#' @return A parsed JSON response as a list.
#' @keywords internal
.send_command <- function(cmd) {
  if (length(cmd) == 0) return(NULL)
  if (!nzchar(cmd)) return(NULL)

  proc <- get(".threadle_proc", envir = .GlobalEnv)
  proc$write_input(paste0(cmd, "\n"))

  mode <- getOption("threadle.command", default = "json")

  if (identical(mode, "cli")){
    out <- character()
    idle_ticks <- 0L
    repeat {
      new <- proc$read_output_lines()

      if (length(new) > 0) {
        out <- c(out, new)
        idle_ticks <- 0L
      } else {
        Sys.sleep(0.01)
        idle_ticks <- idle_ticks + 1L

        # after ~200ms with no new output, treat as finished
        if (idle_ticks >= 20L) {
          return(out)
        }
      }
    }
  }

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
            # if debug raw
            # attr(resp, "raw_lines") <- out
            return(resp)
          }
        }
      }
    } else {
      Sys.sleep(0.01)
    }
  }
}

#' Call a Threadle CLI command and handle the response
#'
#' Internal helper for wrappers. Builds a JSON DTO, sends it to the running
#' Threadle CLI process, checks success, and returns either the payload
#' or the full response.
#'
#' @param cmd Command name (character scalar).
#' @param args Named list of command arguments, or `NULL`.
#' @param assign Optional backend variable name to assign the result to, or `NULL`.
#' @param return What to return: `"payload"` (default) or `"response"`.
#' @param .print_message Logical; if `TRUE`, print `resp$Message` when present.
#' @return If `return = "payload"`, returns `resp$Payload`; otherwise returns the full response.
#' @keywords internal
.th_call <- function(cmd,
                     args = NULL,
                     assign = NULL,
                     return = getOption("threadle.return", "payload"),
                     .print_message = getOption("threadle.print_message", TRUE)) {

  mode <- getOption("threadle.command", "json")

  if (identical(mode, "cli")) {
    if (!isTRUE(getOption("threadle.warn_return_cli_shown", FALSE))) {
      if (!is.null(getOption("threadle.return", NULL)) || length(return) > 1L) {
        message("Note: `threadle.return` = \"payload\"/\"response\" applies only in JSON mode; it is ignored in CLI mode.")
        options(threadle.warn_return_cli_shown = TRUE)
      }
    }

    cmd_str <- .th_json_cmd(command = cmd, args = args, assign = assign)
    if (isTRUE(getOption("threadle.print_cmd", FALSE))) {
      print(cmd_str)
    }

    out <- .send_command(cmd_str)
    .th_stop_if_fail(out)
    if (is.null(out) || length(out) == 0L) return(invisible(NULL))
    return(out)
  }

  cmd_json <- .th_json_cmd(command = cmd, args = args, assign = assign)
  if (isTRUE(getOption("threadle.print_cmd", FALSE))) {
    print(cmd_json)
  }
  resp <- .send_command(cmd_json)
  if (identical(mode, "json")) {.th_stop_if_fail(resp)}

  if (.print_message && !is.null(resp$Message) && nzchar(resp$Message)) {
    message(resp$Message)
  }

  if (return == "payload") {
    p <- resp$Payload
    if (is.null(p) || (is.atomic(p) && length(p) == 0L)) {
      return(invisible(NULL))
    }
    return(p)
  }

  resp
}

#' Start a Threadle CLI process
#'
#' Launches the Threadle CLI process executable and stores the process handle
#' in the global environment as `.threadle_proc`. The process is started with
#' silent mode and an end marker to delimit responses.
#'
#' @param path Path to the Threadle CLI executable.
#' @return Invisibly returns the `processx` process object.
#' @export
th_start_threadle <- function(path = "Threadle.CLIconsole.exe") {
  mode <- getOption("threadle.command", "json")
  if (exists(".threadle_proc", envir=.GlobalEnv)) {
    stop("'.threadle_proc' process already running.")
  }

  if (identical(mode, "cli")) {
    args <- c("--silent")
  } else if (identical(mode, "json")) {
    args <- c("--json", "--silent")
  } else {
    stop("Invalid option threadle.command. Use 'json' or 'cli'.", call. = FALSE)
  }
  proc <- processx::process$new(path, args=args, stdin="|", stdout="|", stderr = "|")
  # proc <- processx::process$new(path, args=c("--json", "--silent"), stdin="|", stdout="|", stderr = "|")
  #proc <- processx::process$new(path, args=c("--endmarker"), stdin="|", stdout="|", stderr = "|")
  # assign(".threadle_proc", proc, envir=.GlobalEnv)
  assign(".threadle_proc", proc, envir=.GlobalEnv)
  invisible(proc)
}

#' Stop the running Threadle CLI process
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

#' Load a network structure from a file
#'
#' @param name Name for the network object.
#' @param file Path to the network file.
#' @param type Type of the structure to create.
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
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Add an edge to a network
#'
#' `th_add_edge()` adds an edge between two nodes in a layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id Node ID of the first vertex in the dyad.
#' @param node2id Node ID of the second vertex in the dyad.
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
  .th_call(cmd = cmd, args = args, assign = assign)
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
                         nodes = c(), addmissingnodes = TRUE) {
  args <- .th_args(environment())
  args$nodes <- if (is.null(args$nodes)) "" else paste(args$nodes, collapse = ";")
  cmd <- "addhyper"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = NULL)
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
th_add_layer <- function(network, layername, mode, directed=FALSE, valuetype = c("binary", "valued"), selfties=FALSE) {
  valuetype <- match.arg(valuetype)
  args <- .th_args(environment())
  cmd <- "addlayer"
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
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Check whether an edge exists in a layer
#'
#' `th_check_edge()` checks whether an edge between
#' two nodes exists in the specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id Node ID of the first vertex in the dyad.
#' @param node2id Node ID of the second vertex in the dyad.
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

#' Create a new network in Threadle
#'
#' Create a new network in Threadle and assign it to variable 'name' in the Threadle CLI environment
#'
#' @param name Name of the assigned variable in the Threadle CLI environment.
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a nodeset in the Threadle CLI environment.
#'
#' @return A `threadle_network` object.
#' @export
th_create_network <- function(nodeset, name) {
  args <- .th_args(environment(), drop = "name")
  cmd <- "createnetwork"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name = name), class = "threadle_network")
}

#' Create a new nodeset in Threadle and assign it to variable 'name' in the Threadle CLI environment
#'
#' @param name Name of the R variable to assign in the CLI environment.
#' @param createnodes Number of nodes
#' @return A `threadle_nodeset` object.
#' @export
th_create_nodeset <- function(name, createnodes = 0) {
  args <- .th_args(environment(), drop = "name")
  cmd <- "createnodeset"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name=name), class="threadle_nodeset")
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
th_define_attr <- function(structure, attrname, attrtype = c('int','char','float','bool')) {
  attrtype <- match.arg(attrtype)
  cmd <- "defineattr"
  args <- list(structure = .th_name(structure), attrname = attrname, attrtype = attrtype)
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
th_degree <- function(network, layername, attrname = NULL, direction = c("in", "out", "both")) {
  direction <- match.arg(direction)
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
#' `th_dichotomize()` creates a recoded version of
#' a layer based on a threshold rule.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param cond Comparison operator: `"eq"`, `"ne"`, `"gt"`, `"lt"`, `"ge"`, or `"le"`.
#'   Defaults to `"ge"`.
#' @param threshold Threshold used with `cond`. Defaults to `1`.
#' @param truevalue Value to assign when the condition is `TRUE`. Defaults to `1`.
#' Can also be `"keep"` to retain the original value for ties that satisfy the condition.
#' @param falsevalue Value to assign when the condition is `FALSE`. Defaults to `0`.
#' Can also be `"keep"` to retain the original value for ties that do not satisfy the condition.
#' @param newlayername Optional name for the new layer.
#' @return CLI output.
#' @export
th_dichotomize <- function(network, layername,
                           cond = c('ge','eq','ne','gt','lt','le','isnull','notnull'),
                           threshold = 1,
                           truevalue = 1,
                           falsevalue = 0,
                           newlayername = NULL) {
  args <- .th_args(environment())
  cmd <- "dichotomize"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Filter a nodeset by an attribute condition
#'
#' `th_filter()` creates a new nodeset based names and values of attributes.
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

#' Generate a random network layer
#'
#' `th_generate()` creates a random
#' network layer and store it under `network`.
#'
#' #' Two generators are supported:
#' \describe{
#'   \item{`type = "er"`}{Erdős–Rényi. Provide `p` (edge probability / density).}
#'   \item{`type = "ws"`}{Watts–Strogatz. Provide `k` (even mean degree) and
#'   `beta` (rewiring probability in [0,1]). The layer must be symmetric.}
#'   \item{`type = "bt"`}{Barabási–Albert. Provide `m` (number of edges each new node attaches; must be >= 1 and typically < number of nodes).}
#' }
#'
#' @param network Name of the new network variable to create.
#' @param layername Name of the layer to create in `network`.
#' @param type Random graph model, one of `"er"` or `"ws"`.
#' @param p For `type = "er"`: edge probability in `[0, 1]`. Required when `type = "er"`.
#' @param k For `type = "ws"`: mean degree (must be even). Required when `type = "ws"`.
#' @param beta For `type = "ws"`: rewiring probability in `[0, 1]`. Required when `type = "ws"`.
#'
#' @return Invisibly returns the CLI response.
#' @export
th_generate <- function(network, layername, type, p, k, beta, m) {
  args <- .th_args(environment())
  cmd <- "generate"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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

#' Get an edge value
#'
#' `th_get_edge()` retrieves the edge value between
#' two nodes in the specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id Node ID of the first vertex in the dyad.
#' @param node2id Node ID of the second vertex in the dyad.
#' @return CLI output.
#' @export
th_get_edge <- function(network, layername, node1id, node2id) {
  args <- .th_args(environment())
  cmd <- "getedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get the number of nodes in a structure
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @return A numeric value.
#' @export
th_get_nbr_nodes <- function(structure) {
  args <- .th_args(environment())
  cmd <- "getnbrnodes"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get alters of a node within a network layer
#'
#' @param network Name of the network.
#' @param layername Layer to query.
#' @param nodeid Node ID.
#' @param direction Tie direction ("both", "in", "out").
#'
#' @return Parsed JSON list of alters.
#' @export
th_get_node_alters <- function(network,nodeid,layername,direction=c("both", "in", "out")) {
  direction <- match.arg(direction)
  args <- .th_args(environment())
  cmd <- "getnodealters"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a node ID by index
#'
#' @param structure Name of the structure (can be network or nodeset).
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

#' Get a random alter for a node
#'
#' `th_get_random_alter()` get a random alter for a node.
#'
#' @param network Network name.
#' @param nodeid Node ID.
#' @param layername Optional layer. If left blank, will pick from all layers
#' @param direction Direction ("both"(default), "in", "out").
#' @param balanced Whether selection should be balanced for multiple layers.
#' Must be `TRUE` or `FALSE`. Defaults to `FALSE`.
#'
#' @return A node ID (numeric).
#' @export
th_get_random_alter <- function(network, nodeid, layername="", direction=c("both", "in", "out"), balanced=FALSE) {
  direction <- match.arg(direction)
  args <- as.list(environment())
  cmd <- "getrandomalter"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a random node from a structure
#'
#' `th_get_random_node()` gets a random node.
#'
#' @param structure Structure name.
#' @return A node ID (numeric).
#' @export
th_get_random_node <- function(structure) {
  args <- .th_args(environment())
  cmd <- "getrandomnode"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get the current working directory from Threadle CLI
#'
#' @return The working directory as returned by Threadle.
#' @export
th_get_workdir <- function() {
  .th_call(cmd = "getwd")
}

#' Import a layer into a network
#'
#' `th_import_layer()` imports a layer from a file into an existing network.
#'
#' @param network Name of the network
#' @param layername Name of the layer to create inside `network`.
#' @param file Path to the input file.
#' @param format Input file format. One of `"edgelist"` or `"matrix"`.
#' @param sep Field separator used when `format = "edgelist"` (and for delimited
#' matrix formats, if applicable). Defaults to tab.
#' @param addmissingnodes Logical; if `TRUE`, create nodes referenced in the file
#' that are not yet present in `network`.
#'
#' @return Invisibly returns the CLI response.
#' @export
th_import_layer <- function(network, layername, file, format = c('edgelist','matrix'), sep = "\t",
                            addmissingnodes = FALSE) {
  format <- match.arg(format)
  cmd <- "importlayer"
  .th_call(cmd = cmd)
}

#' Retrieve meta information from a Threadle object
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @return Parsed JSON or raw CLI text.
#' @export
th_info <- function(structure) {
  args <- .th_args(environment())
  cmd <- "info"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' List all objects currently stored as variables in Threadle
#'
#' @return Parsed JSON or raw text.
#' @export
th_inventory <- function() {
  cmd <- "i"
  args <- NULL
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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

#' Delete a structure
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @return CLI output.
th_delete <- function(structure) {
  args <- .th_args(environment())
  cmd <- "delete"
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

#' Remove all structures
#'
#' `th_delete_all()` removes all stored variables.
#'
#' @return CLI output.
th_delete_all <- function() {
  args <- NULL
  cmd <- "deleteall"
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
#' @param node1id Node ID of the first vertex in the dyad.
#' @param node2id Node ID of the second vertex in the dyad.
#' @return Invisibly returns the result of the remove operation.
#' @export
th_remove_edge <- function(network, layername, node1id, node2id) {
  args <- .th_args(environment())
  cmd <- "removeedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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
#' (and its edges) from the network.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the layer to be removed.
#' @return CLI output.
#' @export
th_remove_layer <- function(network, layername) {
  args <- .th_args(environment())
  cmd <- "removelayer"
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
  args <- .th_args(environment())
  cmd <- "setattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
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

#' Set working directory inside the Threadle CLI environment
#'
#' @param dir Path to the directory.
#' @return CLI output as a character vector.
#' @examples
#' \dontrun{
#' th_set_workdir("~/data")
#' }
#' @export
th_set_workdir <- function(dir) {
  args <- .th_args(environment())
  .th_call(cmd = "setwd", args = args)
}

#' Calculate the shortest path length between two nodes
#' `th_shortest_path()` computes the shortest path length from `node1id` to
#' `node2id` in a Threadle network. By default, the computation uses all layers
#' in the network. If `layername` is provided, the shortest path is computed
#' using only that specific layer.
#'
#'
#' Note that shortest path measures are directional: for directed layers, the
#' shortest path from `node1id` to `node2id` may differ from the shortest path
#' from `node2id` to `node1id`. For undirected (symmetric) layers, directionality
#' is moot.
#'
#' If no path exists from `node1id` to `node2id` under the specified layer
#' selection, the backend returns `-1` to indicate that the target is unreachable.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param node1id Node ID of the first vertex in the dyad.
#' @param node2id Node ID of the second vertex in the dyad.
#' @param layername Optional layer name. If omitted or empty, all layers are used.
#' @return An integer giving the shortest path length.
#' @export
th_shortest_path <- function(network, node1id, node2id, layername) {
  args <- .th_args(environment())
  cmd <- "shortestpath"
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
  args <- .th_args(environment(), drop = "name")
  cmd <- "subnet"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name = name), class = "threadle_network")
}

#' Undefine an attribute on a structure
#'
#' `th_undefine_attr()` removes an attribute
#' from a nodeset or network.
#'
#' @param structure Name of the structure.
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
