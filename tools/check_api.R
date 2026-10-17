# Compare threadleR's wrapper functions with the Threadle backend API.
#
# Reads tools/threadle-api.json (see tools/update_threadle_api.py) and
# tools/threadle-mapping.yaml, statically inspects every function in R/ that
# sends a fixed backend command (`cmd <- "..."`), and reports:
#   errors   - things that break calls: unknown commands, arguments the backend
#              does not know, required backend arguments missing in R
#   warnings - things that may mislead users: different defaults or choices,
#              backend commands with no wrapper and no entry in the mapping file
#
# Usage (from the package root):
#   Rscript tools/check_api.R                 # print report, exit 1 on errors
#   Rscript tools/check_api.R --report FILE   # also write a Markdown report

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
})

args <- commandArgs(trailingOnly = TRUE)
report_file <- if ("--report" %in% args) args[[match("--report", args) + 1L]] else NULL

api <- fromJSON("tools/threadle-api.json", simplifyVector = FALSE)
mapping <- read_yaml("tools/threadle-mapping.yaml")
backend <- setNames(api$commands, vapply(api$commands, `[[`, "", "name"))

# ---- Backend parameters ------------------------------------------------------

backend_params <- function(cmd) {
  out <- list()
  for (p in cmd$params) {
    alts <- if (!is.null(p$one_of)) p$one_of else list(p)
    for (a in alts) {
      if (is.null(a$name)) next
      a$required <- isTRUE(a$required) && is.null(p$one_of)
      out[[a$name]] <- a
    }
  }
  out
}

# ---- R wrappers --------------------------------------------------------------

find_calls <- function(expr, fun) {
  hits <- list()
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name(fun))) hits[[length(hits) + 1L]] <<- e
      parts <- as.list(e)[-1]
      for (i in seq_along(parts)) {
        if (identical(parts[[i]], quote(expr = ))) next  # empty argument, e.g. x[, 1]
        walk(parts[[i]])
      }
    }
  }
  walk(expr)
  hits
}

literal_default <- function(x) {
  if (is.symbol(x) && identical(as.character(x), "")) return(list(has = FALSE))
  if (is.null(x)) return(list(has = TRUE, value = NULL))
  if (is.call(x) && identical(x[[1]], as.name("c"))) {
    vals <- vapply(as.list(x)[-1], function(v) paste(as.character(v), collapse = ""), "")
    if (length(vals) == 0L) return(list(has = TRUE, value = NULL))  # c() is NULL
    return(list(has = TRUE, value = vals[[1]], choices = vals))
  }
  if (is.atomic(x)) return(list(has = TRUE, value = x))
  list(has = TRUE, value = paste(deparse(x), collapse = ""), expr = TRUE)
}

wrappers <- list()
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  exprs <- parse(file, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")
  for (k in seq_along(exprs)) {
    e <- exprs[[k]]
    if (!(is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function")))) next
    fname <- as.character(e[[2]])
    # The backend command: `cmd <- "x"`, or `.th_call(cmd = "x", ...)` / `.th_call("x", ...)`
    cmd_names <- character()
    for (a in find_calls(e[[3]][[3]], "<-")) {
      if (identical(a[[2]], as.name("cmd")) && is.character(a[[3]])) cmd_names <- c(cmd_names, a[[3]])
    }
    for (call in find_calls(e[[3]][[3]], ".th_call")) {
      arg <- if (!is.null(call$cmd)) call$cmd else if (length(call) >= 2L && is.null(names(call)[2]) ||
                                                        identical(names(call)[2], "")) call[[2]]
      if (is.character(arg)) cmd_names <- c(cmd_names, arg)
    }
    if (length(cmd_names) == 0L) next

    fmls <- e[[3]][[2]]
    drop <- character()
    for (call in find_calls(e[[3]][[3]], ".th_args")) {
      if (!is.null(call$drop)) drop <- c(drop, eval(call$drop))
    }
    choices <- list()
    for (call in find_calls(e[[3]][[3]], "match.arg")) {
      if (length(call) >= 3L && is.symbol(call[[2]])) {
        choices[[as.character(call[[2]])]] <- tryCatch(eval(call[[3]]), error = function(err) NULL)
      }
    }
    # Arguments added or removed after .th_args(): args$x <- ... / args[["x"]] <- NULL
    sent <- setdiff(names(fmls), drop)
    for (a in find_calls(e[[3]][[3]], "<-")) {
      lhs <- a[[2]]
      if (is.call(lhs) && as.character(lhs[[1]]) %in% c("$", "[[") &&
            identical(lhs[[2]], as.name("args"))) {
        key <- as.character(lhs[[3]])
        sent <- if (is.null(a[[3]])) setdiff(sent, key) else union(sent, key)
      }
    }
    wrappers[[fname]] <- list(
      name = fname,
      cmd = cmd_names[[1]],
      line = srcrefs[[k]][[1]],
      file = file,
      formals = lapply(as.list(fmls), literal_default),
      sent = sent,
      choices = choices
    )
  }
}

# ---- Compare -----------------------------------------------------------------

errors <- character()
warnings <- character()
notes <- character()  # informational: R-side defaults for required backend arguments
err <- function(w, msg) errors <<- c(errors, sprintf("%s (%s:%d): %s", w$name, w$file, w$line, msg))
wrn <- function(w, msg) warnings <<- c(warnings, sprintf("%s (%s:%d): %s", w$name, w$file, w$line, msg))

same_value <- function(r, b) {
  if (is.null(r) || is.null(b)) return(TRUE)  # NULL in R = let the backend decide
  unescape <- function(x) gsub("\\t", "\t", gsub("\\n", "\n", x, fixed = TRUE), fixed = TRUE)
  rv <- tolower(as.character(r)); bv <- tolower(unescape(as.character(b)))
  num <- suppressWarnings(c(as.numeric(rv), as.numeric(bv)))
  if (!anyNA(num)) return(isTRUE(all.equal(num[1], num[2])))
  identical(rv, bv)
}

ignored_args <- mapping$r_only_args %||% list()
for (w in wrappers) {
  b <- backend[[w$cmd]]
  if (is.null(b)) {
    err(w, sprintf("sends command '%s', which the backend does not have", w$cmd))
    next
  }
  bp <- backend_params(b)
  skip <- c(unlist(ignored_args[["*"]]), unlist(ignored_args[[w$name]]))

  for (a in setdiff(w$sent, skip)) {
    if (is.null(bp[[a]])) err(w, sprintf("argument '%s' is not a parameter of backend '%s'", a, w$cmd))
  }
  for (p in bp) {
    if (p$required && !(p$name %in% w$sent)) {
      err(w, sprintf("backend '%s' requires '%s', which the R function never sends", w$cmd, p$name))
    }
  }
  for (a in intersect(w$sent, names(bp))) {
    p <- bp[[a]]; f <- w$formals[[a]]
    if (is.null(f)) next  # added inside the function body, not an R argument
    if (p$required && isTRUE(f$has) && !is.null(f$value)) {
      notes <- c(notes, sprintf("%s: '%s' is required by the backend; R supplies the default %s",
                                w$name, a, deparse(f$value)))
    }
    if (!p$required && !isTRUE(f$has)) {
      wrn(w, sprintf("'%s' is optional in the backend but required in R", a))
    }
    if (isTRUE(f$has) && !isTRUE(f$expr) && !is.null(p$default) && !same_value(f$value, p$default)) {
      wrn(w, sprintf("default of '%s' is %s in R but %s in the backend",
                     a, deparse(f$value), deparse(p$default)))
    }
    r_choices <- w$choices[[a]] %||% f$choices
    if (!is.null(r_choices) && !is.null(p$choices)) {
      b_choices <- tolower(as.character(unlist(p$choices)))
      if (!setequal(tolower(r_choices), b_choices)) {
        wrn(w, sprintf("choices for '%s' are {%s} in R but {%s} in the backend", a,
                       paste(r_choices, collapse = ", "), paste(b_choices, collapse = ", ")))
      }
    }
  }
}

wrapped <- unique(vapply(wrappers, `[[`, "", "cmd"))
not_wrapped <- names(mapping$not_wrapped %||% list())
planned <- names(mapping$planned %||% list())
unmapped <- setdiff(names(backend), c(wrapped, not_wrapped, planned))
stale <- intersect(c(not_wrapped, planned), wrapped)
unknown <- setdiff(c(not_wrapped, planned), names(backend))
for (u in unmapped) warnings <- c(warnings, sprintf("backend command '%s' has no R wrapper and is not listed in threadle-mapping.yaml", u))
for (s in stale) warnings <- c(warnings, sprintf("'%s' is listed in threadle-mapping.yaml but now has an R wrapper", s))
for (u in unknown) errors <- c(errors, sprintf("threadle-mapping.yaml lists '%s', which the backend does not have", u))

# ---- Report ------------------------------------------------------------------

summary_lines <- c(
  sprintf("Threadle backend %s: %d commands", api$threadle_version, length(backend)),
  sprintf("  wrapped in R:        %d (by %d functions)", length(intersect(wrapped, names(backend))), length(wrappers)),
  sprintf("  not wrapped (by design): %d", length(not_wrapped)),
  sprintf("  planned:             %d", length(planned)),
  sprintf("  unmapped:            %d", length(unmapped)),
  sprintf("Errors: %d, warnings: %d, notes: %d", length(errors), length(warnings), length(notes))
)
cat(summary_lines, sep = "\n")
if (length(errors)) cat("\nERRORS\n", paste0("  - ", errors, "\n"), sep = "")
if (length(warnings)) cat("\nWARNINGS\n", paste0("  - ", warnings, "\n"), sep = "")
if (length(notes)) cat("\nNOTES\n", paste0("  - ", notes, "\n"), sep = "")

if (!is.null(report_file)) {
  status <- function(cmd) {
    fns <- names(Filter(function(w) identical(w$cmd, cmd), wrappers))
    if (length(fns)) return(paste0("`", fns, "()`", collapse = ", "))
    if (cmd %in% not_wrapped) return(paste("not wrapped:", mapping$not_wrapped[[cmd]]))
    if (cmd %in% planned) return(paste("planned:", mapping$planned[[cmd]]))
    "**unmapped**"
  }
  md <- c(
    "# threadleR API coverage", "",
    sprintf("Generated by `tools/check_api.R` against Threadle %s.", api$threadle_version), "",
    paste0("    ", summary_lines), "",
    "| Backend command | R |", "|---|---|",
    sprintf("| `%s` | %s |", names(backend), vapply(names(backend), status, "")),
    if (length(errors)) c("", "## Errors", "", paste0("- ", errors)),
    if (length(warnings)) c("", "## Warnings", "", paste0("- ", warnings)),
    if (length(notes)) c("", "## Notes", "", paste0("- ", notes))
  )
  writeLines(md, report_file)
}

quit(status = if (length(errors)) 1L else 0L)
