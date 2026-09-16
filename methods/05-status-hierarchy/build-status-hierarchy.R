# =====================================================================
# 05 Mapping Intersectionality: Partial Order Set Modeling
# Builds the status hierarchy of the article's Figure 9 and draws it as
# a Hasse diagram in two steps: the formal structure, then the same cube
# with each profile at its predicted recognition.
# Fill in import-data.R and run it once first.
# Run from the repo root:  Rscript methods/05-status-hierarchy/build-status-hierarchy.R
# =====================================================================
library(tidyverse)
library(pscl)
for (f in list.files("R_functions", pattern = "[.]R$", full.names = TRUE)) source(f)
source("import-data.R")

data <- oc_load_data()
focal <- oc_config$focal_outcome

# --- 1. The local types ---------------------------------------------------
# From a local_type column in your data, which enters as a categorical
# predictor, or from the methods/03 run of record, as in methods/04.
# Without a matching run of record, oc_local_types() stops and points to
# methods/03; a fresh fit could renumber the classes.
if ("local_type" %in% names(data)) {
  data$local_type <- factor(data$local_type)   # categorical, even if coded 1, 2, 3
  if (nlevels(data$local_type) < 2) stop(one_class_message, call. = FALSE)
  type_terms <- "local_type"
} else {
  lt <- oc_local_types(data, need_probs = TRUE)
  if (lt$nclass < 2) stop(one_class_message, call. = FALSE)
  data$local_type <- lt$display     # "type 1", "type 2", ... or your type_labels
  # Proportional assignment, the article's main specification: the type
  # terms are each member's posterior class probabilities. Class 1 is
  # the reference. The modal class only groups members into positions.
  type_probs <- setNames(as.data.frame(lt$posterior[, -1, drop = FALSE]),
                         paste0("type_prob", 2:lt$nclass))
  data <- cbind(data, type_probs)
  type_terms <- names(type_probs)
}

# --- 2. The dimensions and the members with a position --------------------
# Each ascriptive status and the local type is one axis of the hierarchy;
# a member with a missing value on any of them has no position.
ascriptive <- oc_config$ascriptive
dimensions <- c(ascriptive, "local_type")
has_position <- complete.cases(data[dimensions])
if (!all(has_position)) {
  message(sum(!has_position), " member(s) dropped for missing values on the ",
          "poset dimensions (", paste(dimensions, collapse = ", "), ")")
  data <- data[has_position, , drop = FALSE]
}

# --- 3. One recognition model ---------------------------------------------
# The same model as methods/04 (pscl::zeroinfl), for the focal outcome:
# the ascriptive statuses, their pairwise intersections, the type terms,
# and the controls in both the zero-inflation model and the count model,
# as treatment-coded dummies; count_model in import-data.R sets the count
# family.
intersections <- if (length(ascriptive) >= 2) {
  combn(ascriptive, 2, paste, collapse = ":")
} else character(0)
spec <- c(ascriptive, intersections, type_terms, oc_config$controls)
options(contrasts = c("contr.treatment", "contr.treatment"))   # as Appendix D coded them
data <- dummy_coded(data, spec)
dist <- if (is.null(oc_config$count_model)) "poisson" else oc_config$count_model
# an ascriptive cell with no member whose count is known cannot carry an
# intersection term; the check names it before the fit
check_ascriptive_cells(data, ascriptive, focal)
model <- zeroinfl(reformulate(spec, response = focal), data = data, dist = dist,
                  link = "logit")
if (!isTRUE(model$converged)) warning(
  "zeroinfl did not converge for `", focal, "` -- on sparse data try dropping ",
  "interactions, collapsing thin factor levels, or simplifying the zero ",
  "component (y ~ x | 1)")

# --- 4. Every position valued by prediction --------------------------------
# For each position, the model predicts every member's recognition as if
# they held that position, with their own controls; the average is the
# position's value.
positions <- predict_positions(model, data, dimensions, type_terms)
print(head(positions, 5), digits = 3)
# Every position is saved with its value, interval, and occupancy (n
# members with a known count, n_unknown without), so a thin position can
# be found behind the cube cell that pools it.
csv <- oc_output_file(sprintf("status_hierarchy_%s_positions.csv", focal))
write.csv(positions, csv, row.names = FALSE)
cat("wrote", csv, "\n")

# --- 5. The ideal actor -----------------------------------------------------
# Among positions with at least 3 members, the one with the highest lower
# confidence bound. rule = "max" takes the highest point value, the
# article's rule; ideal = list(race = "White", ...) sets it directly.
ideal <- choose_ideal(positions, rule = "conf")
rule_text <- c(conf = "highest lower confidence bound among occupied positions",
               max = "highest predicted value among occupied positions",
               supplied = "supplied by you")[ideal$rule]
cat(sprintf("Ideal actor (%s): %s -- predicted %s %.2f [%.2f, %.2f], n = %d\n",
            rule_text, paste(unlist(ideal$full), collapse = " / "), focal,
            ideal$row$predicted, ideal$row$conf.low, ideal$row$conf.high,
            ideal$row$n))
if (!identical(unname(unlist(ideal$full)),
               as.character(unlist(ideal$raw_top[dimensions])))) {
  cat(sprintf("The raw maximum among positions with n >= %d is %s (%.2f, n = %d); rule = \"max\" takes it.\n",
              ideal$min_n, paste(unlist(ideal$raw_top[dimensions]), collapse = " / "),
              ideal$raw_top$predicted, ideal$raw_top$n))
}
if (!identical(as.character(unlist(ideal$top_any[dimensions])),
               as.character(unlist(ideal$raw_top[dimensions])))) {
  cat(sprintf("The highest prediction on the whole grid is %s (%.2f, n = %d), below the cutoff of %d.\n",
              paste(unlist(ideal$top_any[dimensions]), collapse = " / "),
              ideal$top_any$predicted, ideal$top_any$n, ideal$min_n))
}
cat(sprintf(paste0("Read it as the organization's ideal only if %s is a valued ",
                   "recognition; for a stigma label it is that label's ",
                   "prototype (see docs/decisions-you-will-face.md).\n"), focal))

# --- 6. The ideal-vs-rest cube ---------------------------------------------
# Each dimension is reduced to the ideal's level versus the rest, and
# each of the 2^k cells takes the pooled prediction of its positions.
cube <- collapse_to_cube(model, positions, data, ideal)
print(cube, digits = 3)

# --- 7. The two-step drawing ---------------------------------------------
# Step one draws the formal structure (height = characteristics shared
# with the ideal); step two keeps the same cube and places each cell at
# its predicted value. poset_axis_limits in import-data.R pins step two to
# one scale across runs.
formal <- status_hierarchy_poset(cube, layout = "formal", label_style = "match")
values <- status_hierarchy_poset(cube, layout = "outcome", label_style = "match",
                                 axis_limits = oc_config$poset_axis_limits)
values$shape_metrics     # compression, spread, violations
values$violations        # dominance pairs the outcome order contradicts

# Save the cube's cells (value, interval, occupancy) and the shape
# metrics with the ideal.
csv <- oc_output_file(sprintf("status_hierarchy_%s_profiles.csv", focal))
write.csv(cube, csv, row.names = FALSE)
cat("wrote", csv, "\n")
shape <- data.frame(focal_outcome = focal,
                    ideal = paste(unlist(attr(cube, "ideal")), collapse = " / "),
                    ideal_rule = ideal$rule,          # how the ideal was chosen
                    ideal_min_n = ideal$min_n,        # the occupancy cutoff for candidates
                    ideal_n = ideal$row$n,
                    ideal_predicted = ideal$row$predicted,
                    top_any = paste(unlist(ideal$top_any[dimensions]), collapse = " / "),
                    top_any_predicted = ideal$top_any$predicted,
                    top_any_n = ideal$top_any$n,
                    as.data.frame(values$shape_metrics))
csv <- oc_output_file(sprintf("status_hierarchy_%s_shape.csv", focal))
write.csv(shape, csv, row.names = FALSE)
cat("wrote", csv, "\n")

fig1 <- oc_output_file(sprintf("status_hierarchy_formal_%s.pdf", focal))
ggsave(fig1, oc_provenance(formal$plot), width = 11, height = 6,
       device = cairo_pdf)
cat("wrote", fig1, "(step one: the formal structure)\n")

fig2 <- oc_output_file(sprintf("status_hierarchy_%s.pdf", focal))
ggsave(fig2, oc_provenance(values$plot +
         labs(y = sprintf("Expected %s nominations", focal))),
       width = 9, height = 6, device = cairo_pdf)
cat("wrote", fig2, "(step two: profiles at their predicted values)\n")

# --- step two with whiskers ---------------------------------------------
# Each cell's confidence interval draws as a vertical whisker. Check the
# whiskers before reading a close ordering, especially in small cells.
poset_ci <- status_hierarchy_poset(cube, layout = "outcome", label_style = "match",
                                   outcome_lo = "conf.low", outcome_hi = "conf.high",
                                   axis_limits = oc_config$poset_axis_limits)
fig_ci <- oc_output_file(sprintf("status_hierarchy_%s_ci.pdf", focal))
ggsave(fig_ci, oc_provenance(poset_ci$plot +
         labs(y = sprintf("Expected %s nominations", focal))),
       width = 9, height = 5.5,
       device = cairo_pdf)
cat("wrote", fig_ci, "\n")

# --- 8. Who meets the ideal's criteria but goes unrecognized? -------------
# The article's diagnostic step after Figure 9. Among members who share
# the ideal's local type, by ascriptive profile: how many were recognized,
# and how many would have been had that type been rewarded at the ideal's
# rate. `at_ideal_rate` uses the model with each member's ascriptive
# statuses set to the ideal's; `at_ideal_share` applies the ideal cell's
# observed share. `additional` is the difference, summed in the Total row
# over every profile but the ideal's own.
unrec <- unrecognized_members(model, data, ideal)
cat(sprintf(paste0("\nMembers of the ideal's local type (%s), by ascriptive ",
                   "profile: recognized (>= 1 %s nomination) vs. recognized ",
                   "if rewarded at the ideal's rate\n"),
            ideal$full$local_type, focal))
print(unrec, digits = 3, row.names = FALSE)
csv <- oc_output_file(sprintf("unrecognized_%s.csv", focal))
write.csv(unrec, csv, row.names = FALSE)
cat("wrote", csv, "\n")

# docs/interpreting-results.md explains how to read the output.
