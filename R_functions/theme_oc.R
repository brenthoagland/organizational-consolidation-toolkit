# theme_oc.R
#
# What every figure and saved file shares: display labels, output paths,
# the provenance note, and the ggplot2 theme.
#
#   oc_label()          display label for one level of one variable
#   oc_dummy_labels()   labels for the rule network's attribute values
#   oc_synthetic_run()  TRUE when the configured data are the bundled synthetic files
#   oc_output_file()    where a section saves a file (output/, run_name, prefix)
#   oc_provenance()     the provenance note on a figure's caption
#   theme_oc()          the shared ggplot2 theme
#
# Labels come from the optional `labels` list in import-data.R; anything
# unmapped displays as "variable: level".

#' Display label for one level of one variable.
#' @param var    variable name (column in your data)
#' @param level  the level as a character string
#' @param labels the config `labels` list (variable -> named vector); NULL ok
oc_label <- function(var, level, labels = NULL) {
  level <- as.character(level)
  fallback <- paste0(var, ": ", level)
  map <- labels[[var]]
  if (is.null(map)) return(fallback)
  mapped <- unname(map[level])
  ifelse(is.na(mapped), fallback, mapped)
}

#' Labels for the var_level dummy names built by noms_to_transactions().
#' Returns a named vector: dummy name -> display label, one entry per
#' observed level of each attribute (plus each outcome in caps).
oc_dummy_labels <- function(data, attributes, outcomes, labels = NULL) {
  out <- character(0)
  for (v in attributes) {
    for (l in levels(factor(data[[v]]))) {
      out[paste0(v, "_", sanitize_level(l))] <- oc_label(v, l, labels)
    }
  }
  for (o in outcomes) out[o] <- toupper(o)
  out
}

#' TRUE when import-data.R points at one of the bundled synthetic datasets.
#' On a synthetic run, oc_output_file() and oc_provenance() mark
#' everything saved; outputs from your own data save under plain names,
#' unmarked.
oc_synthetic_run <- function(cfg = oc_config) {
  grepl("^synthetic_", basename(cfg$data_path))
}

#' Path for a saved output. On a synthetic run the filename gains a
#' "synthetic_" prefix so nothing produced from the example data can be
#' mistaken for the article's exhibits -- or for your results.
oc_output_file <- function(name, cfg = oc_config) {
  dir <- if (is.null(cfg$run_name)) "output" else file.path("output", cfg$run_name)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  prefix <- if (oc_synthetic_run(cfg)) "synthetic_" else ""
  file.path(dir, paste0(prefix, name))
}

#' On a synthetic run, stamp a provenance caption onto a ggplot;
#' otherwise return the plot unchanged.
oc_provenance <- function(plot, cfg = oc_config) {
  if (!oc_synthetic_run(cfg)) return(plot)
  prov <- "Synthetic data illustration — not the article's results"
  # append, so a plot's own explanatory caption (e.g. the hierarchy's
  # "arrows point from dominant to dominated") is kept, not overwritten
  existing <- plot$labels$caption
  caption <- if (is.null(existing) || !nzchar(existing)) prov else
    paste0(existing, "\n", prov)
  plot +
    ggplot2::labs(caption = caption) +
    ggplot2::theme(plot.caption = ggplot2::element_text(
      size = 7, colour = "grey40", hjust = 0))
}

#' Minimal shared theme: Helvetica, white background (ASR figure specs).
theme_oc <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Helvetica") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}
