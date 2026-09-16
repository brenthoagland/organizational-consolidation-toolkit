# check_data(): check a loaded table against the input requirements in
# data/data-dictionary.md. It first prints the resolved schema, what the
# declarations in import-data.R made of each column, so an accidental
# default is visible before the verdict. A problem breaks a requirement
# and stops the run with the list printed. An advisory flags something
# worth knowing (missing values, small cells, missing display labels, a
# default the form left to fall to) and lets the run proceed.
# import-data.R calls this when it is run on its own.

check_data <- function(data = oc_load_data(), cfg = oc_config) {
  problems <- character(0)
  advisories <- character(0)
  note <- function(...) problems <<- c(problems, sprintf(...))
  advise <- function(...) advisories <<- c(advisories, sprintf(...))

  required <- c(cfg$id, cfg$ascriptive, cfg$criteria, cfg$outcomes, cfg$controls)
  absent <- setdiff(required, names(data))
  if (length(absent) > 0) note("missing columns: %s", paste(absent, collapse = ", "))

  # the settings the later methods read must name declared columns
  if (!is.null(cfg$focal_outcome) && !cfg$focal_outcome %in% cfg$outcomes)
    note("focal_outcome \"%s\" is not one of the declared outcomes (%s)",
         cfg$focal_outcome, paste(cfg$outcomes, collapse = ", "))
  for (v in setdiff(c(names(cfg$ordinal), names(cfg$discretize), cfg$nominal,
                      names(cfg$reference)), names(data)))
    note("%s is declared in import-data.R but is not a column of the table", v)

  print_resolved_schema(data, cfg)

  if (cfg$id %in% names(data)) {
    if (anyNA(data[[cfg$id]]))
      note("%s has missing values: every member needs an id", cfg$id)
    if (anyDuplicated(data[[cfg$id]]) > 0)
      note("%s is not unique: not one row per member", cfg$id)
  }

  # a default the form left to fall to, named so it is a choice
  for (a in intersect(cfg$ascriptive, names(data))) {
    if (!a %in% names(cfg$reference) && is.factor(data[[a]]))
      advise("%s has no reference category declared; the regressions compare against its first level, %s",
             a, levels(data[[a]])[1])
  }
  for (v in intersect(cfg$controls, names(data))) {
    x <- data[[v]]
    if (is.numeric(x) && !v %in% cfg$nominal &&
        length(unique(x[!is.na(x)])) <= 12)
      advise("%s is numeric and not named in `nominal`: it enters the regressions as a quantity (%d distinct values)",
             v, length(unique(x[!is.na(x)])))
  }

  for (out in intersect(cfg$outcomes, names(data))) {
    y <- data[[out]]
    if (!is.numeric(y) || any(y < 0, na.rm = TRUE) || any(y != round(y), na.rm = TRUE))
      note("%s must be non-negative integer counts", out)
    if (anyNA(y))
      advise(paste0("%s has NA counts. An unknown count is not a zero: the ",
                    "recognition models (methods/04 and 05) leave those members ",
                    "out of the fit and count them in n_unknown; rule mining ",
                    "(methods/02) needs a known count for every member and stops ",
                    "until those rows are dropped or recoded. A subgroup with no ",
                    "known count cannot carry its intersection term; methods/04 ",
                    "and 05 stop and name it."), out)
    if (is.numeric(y) && all(y == 0, na.rm = TRUE))
      note("%s is all zeros -- rule mining and the ZIP model need variation", out)
  }

  # Missing values are reported by role, never imputed. LCA keeps members
  # with missing criteria (na.rm = FALSE); the regressions drop incomplete
  # rows.
  for (v in intersect(c(cfg$ascriptive, cfg$criteria, cfg$controls), names(data))) {
    na_n <- sum(is.na(data[[v]]))
    if (na_n > 0)
      advise("%s has %d missing value(s), left as NA", v, na_n)
  }

  for (v in intersect(c(cfg$ascriptive, cfg$criteria), names(data))) {
    x <- data[[v]]
    if (is.numeric(x) && length(unique(x[!is.na(x)])) > 12)
      note("%s looks continuous: name it in import-data.R's discretize block", v)
    if (is.factor(x) || is.character(x)) {
      small <- table(x)[table(x) < 5]
      if (length(small) > 0)
        advise("%s has small cells (<5): %s; expect unstable estimates there",
               v, paste(names(small), collapse = ", "))
    }
  }

  for (v in intersect(names(cfg$labels), names(data))) {
    unlabeled <- setdiff(levels(factor(data[[v]])), names(cfg$labels[[v]]))
    if (length(unlabeled) > 0)
      advise("%s has levels without display labels (%s); figures show the raw names",
             v, paste(unlabeled, collapse = ", "))
  }

  if (length(advisories) > 0) {
    cat("Advisories (read them, then proceed):\n")
    for (a in advisories) cat(" -", a, "\n")
  }
  if (length(problems) == 0) {
    cat("Data meets the input requirements.\n")
  } else {
    cat("Problems found:\n")
    for (p in problems) cat(" -", p, "\n")
    stop("fix the problems above before running the method scripts",
         call. = FALSE)
  }
  invisible(list(problems = problems, advisories = advisories))
}

# print_resolved_schema(): what the declarations made of each column, in
# the order the methods use them. Ordinal criteria show their order,
# categorical columns their levels with the reference first, outcomes
# their type, and a numeric control the fact that it enters as a quantity.
print_resolved_schema <- function(data, cfg = oc_config) {
  describe <- function(v) {
    if (!v %in% names(data)) return("(missing)")
    x <- data[[v]]
    if (is.ordered(x)) return(paste("ordinal", paste(levels(x), collapse = " < ")))
    if (is.factor(x)) {
      lv <- levels(x)
      ref <- if (v %in% names(cfg$reference)) sprintf(" (reference %s)", lv[1]) else ""
      return(paste0("categorical ", paste(lv, collapse = " | "), ref))
    }
    if (is.numeric(x)) {
      k <- length(unique(x[!is.na(x)]))
      return(if (v %in% cfg$outcomes) "count" else
        sprintf("quantity (%d distinct values)", k))
    }
    "text"
  }
  cat("Resolved schema:\n")
  cat(sprintf("  %-11s %s (%s)\n", "id", cfg$id,
              if (cfg$id %in% names(data) && is.character(data[[cfg$id]])) "text" else describe(cfg$id)))
  roles <- list(ascriptive = cfg$ascriptive, criteria = cfg$criteria,
                outcomes = cfg$outcomes, controls = cfg$controls)
  for (r in names(roles)) {
    for (i in seq_along(roles[[r]])) {
      v <- roles[[r]][i]
      cat(sprintf("  %-11s %s: %s\n", if (i == 1) r else "", v, describe(v)))
    }
  }
  if ("local_type" %in% names(data))
    cat(sprintf("  %-11s local_type: %s\n", "supplied", describe("local_type")))
  cat("\n")
  invisible(NULL)
}

# run_on_its_own(): TRUE when import-data.R is the file being run or
# sourced by itself (Rscript import-data.R, the Source button in RStudio,
# source("import-data.R") at the console, or the file run line by line).
# FALSE when a method script, the walkthrough, or a test sources it, so
# those read the form and the loader without repeating the check.
run_on_its_own <- function(file = "import-data.R") {
  if (isTRUE(getOption("knitr.in.progress"))) return(FALSE)
  if (identical(Sys.getenv("TESTTHAT"), "true")) return(FALSE)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(script) > 0 && !identical(basename(script[1]), basename(file))) return(FALSE)
  sourced <- unlist(lapply(sys.frames(), function(fr) {
    if (!exists("ofile", envir = fr, inherits = FALSE)) return(NULL)
    f <- get("ofile", envir = fr, inherits = FALSE)
    if (is.character(f)) basename(f) else "<connection>"
  }))
  all(sourced == basename(file))
}
