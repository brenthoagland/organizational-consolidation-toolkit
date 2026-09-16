# =====================================================================
# 04 Local Types as Regression Inputs
# Models the recognition counts for each status position (pscl) with
# the local types as predictors; saves the predicted profiles and the
# article's model comparison.
# Fill in import-data.R and run it once first.
# Run from the repo root:  Rscript methods/04-regression/predict-recognition.R
# =====================================================================
library(tidyverse)
library(pscl)
for (f in list.files("R_functions", pattern = "[.]R$", full.names = TRUE)) source(f)
source("import-data.R")

data <- oc_load_data()

# The local types come from a local_type column in your data, which
# enters as a categorical predictor, or from the methods/03 run of
# record. Without a matching run of record, oc_local_types() stops and
# points to methods/03; a fresh fit could renumber the classes.
if ("local_type" %in% names(data)) {
  data$local_type <- factor(data$local_type)   # categorical, even if coded 1, 2, 3
  if (nlevels(data$local_type) < 2) stop(one_class_message, call. = FALSE)
  type_terms <- "local_type"
} else {
  lt <- oc_local_types(data)
  if (lt$nclass < 2) stop(one_class_message, call. = FALSE)
  data$local_type <- lt$assignment   # the profile grid needs categories
  # Proportional assignment, the article's main specification (Appendix
  # D): the type terms are each member's posterior probabilities of class
  # membership, entered as they are. Class 1 is the reference. Modal
  # assignment is the robustness check at the end. As Appendix D states,
  # neither modal nor proportional assignment eliminates classification-
  # error bias.
  type_probs <- setNames(as.data.frame(lt$posterior[, -1, drop = FALSE]),
                         paste0("type_prob", 2:lt$nclass))
  data <- cbind(data, type_probs)
  type_terms <- names(type_probs)
}

# One model per status position, with the same predictors: ascriptive
# statuses, their pairwise intersections, the local-type terms, and the
# controls.
ascriptive <- oc_config$ascriptive
intersections <- if (length(ascriptive) >= 2) {
  combn(ascriptive, 2, paste, collapse = ":")
} else character(0)
spec <- c(ascriptive, intersections, type_terms, oc_config$controls)

# The predictors enter as treatment-coded dummies against a reference
# level, as the article coded them (Appendix D): an ordered control enters
# as level dummies too. The coding is set here, not left to the session.
options(contrasts = c("contr.treatment", "contr.treatment"))
data <- dummy_coded(data, c(spec, "local_type"))

# One zero-inflated model per status position (pscl::zeroinfl), as in
# Appendix D. The model specifies two processes with the same
# predictors: a zero-inflation model that predicts the probability of
# receiving no nominations, and a count model that estimates the rate of
# nominations among members at risk of being nominated. count_model in
# import-data.R sets the count family for methods/04 and 05 alike:
# "poisson", the article's zero-inflated Poisson, or "negbin".
dist <- if (is.null(oc_config$count_model)) "poisson" else oc_config$count_model
models <- map(set_names(oc_config$outcomes), function(out) {
  # an ascriptive cell with no member whose count is known cannot carry
  # an intersection term; the check names it before the fit
  check_ascriptive_cells(data, ascriptive, out)
  zeroinfl(reformulate(spec, response = out), data = data, dist = dist,
           link = "logit")
})
iwalk(models, ~ if (!isTRUE(.x$converged)) warning(
  "zeroinfl did not converge for `", .y, "` -- on sparse data try dropping ",
  "interactions, collapsing thin factor levels, or simplifying the zero ",
  "component (y ~ x | 1)"))
walk(models, ~ print(summary(.x)))

# Predicted nominations for each observed ascriptive x local-type profile.
profile_vars <- c(ascriptive, "local_type")
profiles <- imap_dfr(models, function(m, out) {
  predicted_profiles(m, data, profile_vars = profile_vars) %>%
    mutate(status_position = out)
})

print(profiles)

# Save the predicted profiles. Methods/05 does not read this CSV; it
# fits the same model and predicts its own grid.
csv <- oc_output_file("predicted_profiles.csv")
write.csv(profiles, csv, row.names = FALSE)
cat("wrote", csv, "\n")

# --- The article's three-model comparison (Appendix D, Tables D3-D5) ---
# For each status position: the ascriptive statuses with their
# intersections, the local types alone, and both combined (the model
# above), all with the same controls. Coefficients are exponentiated:
# odds ratios in the zero block, incidence rate ratios in the count block.
spec_ascriptive <- c(ascriptive, intersections, oc_config$controls)
spec_types      <- c(type_terms, oc_config$controls)
for (out in oc_config$outcomes) {
  three <- list(
    "Ascrip."    = zeroinfl(reformulate(spec_ascriptive, response = out), data = data, dist = dist, link = "logit"),
    "Local Type" = zeroinfl(reformulate(spec_types, response = out), data = data, dist = dist, link = "logit"),
    "Combined"   = models[[out]])
  tab <- recognition_model_table(
    three, term_order = c(type_terms, ascriptive, oc_config$controls))
  cat(sprintf("\n%s: zero-inflated %s, exponentiated coefficients\n", out, dist))
  print(tab, row.names = FALSE, right = FALSE)
  csv <- oc_output_file(sprintf("recognition_models_%s.csv", out))
  write.csv(tab, csv, row.names = FALSE)
  cat("wrote", csv, "\n")
}

# --- Robustness checks (Appendix D) ------------------------------------
for (out in oc_config$outcomes) {
  # 1. Modal assignment: each member's modal class as a categorical
  #    predictor instead of the posteriors. With well-separated classes
  #    the two agree closely; report both when they do not.
  if (!identical(type_terms, "local_type")) {
    m_modal <- zeroinfl(
      reformulate(c(ascriptive, intersections, "local_type", oc_config$controls),
                  response = out),
      data = data, dist = dist, link = "logit")
    both <- merge(
      predicted_profiles(models[[out]], data, profile_vars),
      predicted_profiles(m_modal, data, profile_vars),
      by = profile_vars, suffixes = c("_proportional", "_modal"))
    cat(sprintf(
      paste0("\nModal-assignment check (%s): correlation of profile",
             " predictions %.3f; largest absolute difference %.2f",
             " nominations.\n"),
      out,
      cor(both$predicted_proportional, both$predicted_modal),
      max(abs(both$predicted_proportional - both$predicted_modal))))
  }

  # 2. Count-model comparison: Vuong tests of the zero-inflated model
  #    against ordinary Poisson and negative binomial (read the
  #    AIC-corrected row; Desmarais and Harden 2013). A positive statistic
  #    favors the first model, the zero-inflated one.
  f <- reformulate(spec, response = out)
  cat("\nVuong tests,", out, "\n")
  vuong(models[[out]], glm(f, data = data, family = poisson))
  vuong(models[[out]], MASS::glm.nb(f, data = data))
}

# docs/interpreting-results.md explains how to read the output.
