# How the methods connect

Every script in `methods/` reads [`import-data.R`](../import-data.R), takes
the member-level table described in
[`data/data-dictionary.md`](../data/data-dictionary.md), and calls
functions in [`R_functions/`](../R_functions). For each section, this
page lists what it calls, the terms it uses, the settings it passes,
what it saves, and what it hands to the next section.

```
member-level table (data/data-dictionary.md)
        │
        ▼
[01] initial assessment ──────────► pairwise magnitudes + MI (diagnostic)
        │
        ▼
[02] association rule mining ─────► rules + rule network (diagnostic)
        │
        ▼
[03] latent class analysis ───────► output/local_types.rds  ─┐  (the run
        │                                                    │   of record)
        ▼                                                    │
[04] regression ──────────────────► predicted profiles   ◄───┤
        │                                                    │
        ▼                                                    │
[05] poset ───────────────────────► status hierarchy     ◄───┘
```

## [01 — An Initial Assessment of Organizational Consolidation](../methods/01-measure-consolidation)

- **Calls:** [`measure_consolidation()`](../R_functions/measure_consolidation.R),
  which chooses phi, `point_biserial()`, `eta_ratio()`, or
  `cramers_v()` for each pair and computes `mutual_information()` and
  `total_correlation()`.
- **Terms:** *magnitude*, the pair's association statistic on a 0–1
  scale; *mutual information*, how much knowing one variable reduces
  uncertainty about the other, in nats; *total correlation*, dependence
  among all the listed variables at once; *permutation null*, the total
  correlation expected from sparse-table bias alone, estimated by
  shuffling each column.
- **Settings:** the statistic follows each pair's levels of measurement
  (binary, ordinal or continuous, nominal; ordered factors count as
  ordinal); `n_permutations = 20` for the null. A numeric variable with
  more than 12 distinct values stops the script until it is named in
  `import-data.R`'s `discretize` block.
- **Saves:** `pairwise_associations.csv` (measure, magnitude, mutual
  information, complete-case n per pair), `initial_assessment.pdf`.
- **Feeds:** nothing reads these files; you read the later sections
  against this diagnostic.

## [02 — Surfacing Salient Attributes: Association Rule Mining](../methods/02-association-rule-mining)

- **Calls:** [`noms_to_transactions()`](../R_functions/association_rules.R),
  then `arules::apriori()` in the script,
  [`add_rule_measures()`](../R_functions/association_rules.R),
  [`rules_for()`](../R_functions/association_rules.R),
  [`plot_rule_network()`](../R_functions/plot_rule_network.R),
  [`oc_dummy_labels()`](../R_functions/theme_oc.R).
- **Terms:** *transaction*, one recognition event (one nomination);
  *antecedent* and *consequent*, the attribute bundle on a rule's left
  side and the status position on its right; *support*, the share of
  transactions containing the rule; *confidence*, how often the
  consequent follows the antecedent; *lift*, co-occurrence relative to
  chance (1 = chance); *conviction*, how much rarer the rule's
  counterexamples are than chance; *polysemy*, the same attribute
  carrying different implications in different pairings.
- **Settings:** the article's chain: support 0.05, confidence 0.50,
  then `lift > 1`. The figure draws every surviving rule, as the
  article's Figure 6 did, until more than 200 survive; then it draws the
  25 highest-lift per position, or the number `max_rules_per_position`
  in `import-data.R` sets. The CSV keeps every rule.
- **Saves:** `attribute_rules.csv`, `rule_network.pdf`.
- **Feeds:** nothing reads these files; the rules inform how you read
  the local types.

## [03 — Identifying Local Types: Latent Class Analysis](../methods/03-latent-class-analysis)

- **Calls:** [`lca_fit_stats()`](../R_functions/latent_types.R) and
  `as_table_c1()` (the class-count sweep), then `poLCA::poLCA()` in the
  script on the criteria coded by
  [`as_lca_codes()`](../R_functions/latent_types.R),
  [`save_local_types()`](../R_functions/latent_types.R) (the run of
  record), [`classification_table()`](../R_functions/latent_types.R)
  (Table C2) and `classification_error_matrix()` (Table C3),
  [`cramers_v()`](../R_functions/measure_consolidation.R),
  `draw_type_mosaic()`.
- **Terms:** *local type* or *latent class*, a recurring profile of
  co-occurring criteria; *BIC* and *AIC*, fit statistics that inform the
  class count; *entropy R²*, how separated the classes are;
  *classification error*, how confidently members sort into their
  modal class; *run of record*, the saved fit every later section uses,
  so classes are never renumbered.
- **Settings:** a sweep over 1 to 6 classes with 10 EM restarts; the
  final fit uses `n_types` from `import-data.R`, also with 10 restarts;
  `na.rm = FALSE` keeps members with missing criteria; seeds are fixed.
- **Saves:** `class_count_sweep.csv` and `.pdf` (the sweep in Table
  C1's shape), `class_probabilities.csv` (the response profile of each
  class, with the responses' own labels), `type_by_ascriptive_V.csv`
  (the type-level V), `type_by_*_mosaic.pdf`, and
  `output/local_types.rds`, the run of record: the assignments, the
  posteriors, and a content hash of the criteria it was fit on.
- **Feeds:** the run of record. Methods/04 and 05 load it through
  [`oc_local_types()`](../R_functions/latent_types.R), which checks that
  the members, class count, criterion columns, and criterion values
  match and stops rather than refit if they do not (`allow_refit = TRUE`
  overrides).

## [04 — Local Types as Regression Inputs](../methods/04-regression)

- **Calls:** [`oc_local_types()`](../R_functions/latent_types.R),
  [`dummy_coded()`](../R_functions/recognition_models.R), then
  `pscl::zeroinfl()` in the script,
  [`predicted_profiles()`](../R_functions/recognition_models.R),
  [`recognition_model_table()`](../R_functions/recognition_models.R).
- **Terms:** *zero-inflated Poisson*, a count model with two parts: a
  logit for the odds of no nominations, and a count model for the rate
  of nominations among members at risk of being labeled; *predicted
  profile*, the model's expected recognition for each ascriptive-by-type
  combination, with confidence bounds and cell sizes.
- **Settings:** `count_model` from `import-data.R` (`"poisson"`, the
  article's specification, shared with methods/05; `"negbin"` for
  overdispersion). One model per outcome: the ascriptive statuses, their
  pairwise intersections, the local-type terms, and the controls. Local
  types enter by proportional assignment, the article's main
  specification (a supplied `local_type` column enters as categories);
  ordered factors enter as level dummies. For each outcome the script
  then prints the article's three-model comparison (ascriptive statuses
  with intersections, local types alone, both combined) in the shape of
  Appendix D's Tables D3–D5, and runs the robustness checks: modal
  assignment, and Vuong comparisons against ordinary Poisson and
  negative binomial.
- **Saves:** `predicted_profiles.csv`; `recognition_models_<outcome>.csv`.
- **Feeds:** methods/05 fits the same model again from the run of
  record and your data; it does not read this CSV. `predicted_profiles.csv`
  holds Figure D1's quantity: each observed ascriptive × local-type
  profile's predicted nominations, averaged over its members. Methods/05
  predicts every position of the full grid for the whole membership, as
  Figure 9's construction does, and the cube averages the positions in
  each of its cells.

## [05 — Mapping Intersectionality: Partial Order Set Modeling](../methods/05-status-hierarchy)

- **Calls:** [`oc_local_types()`](../R_functions/latent_types.R),
  [`dummy_coded()`](../R_functions/recognition_models.R), then
  `pscl::zeroinfl()` in the script, then the steps in order:
  [`predict_positions()`](../R_functions/status_hierarchy_steps.R),
  [`choose_ideal()`](../R_functions/status_hierarchy_steps.R),
  [`collapse_to_cube()`](../R_functions/status_hierarchy_steps.R) (which
  calls [`dichotomize_against_ideal()`](../R_functions/status_hierarchy_poset.R)),
  [`status_hierarchy_poset()`](../R_functions/status_hierarchy_poset.R)
  (the drawing and the shape metrics), and
  [`unrecognized_members()`](../R_functions/recognition_models.R)
  (the diagnostic step after Figure 9).
- **Terms:** *ideal actor*, the position with the most predicted
  recognition; *dominance*, sharing every ideal-matching dimension
  another profile has, and more; *Hasse diagram*, the standard drawing
  of a partial order; *monotonicity violation*, a profile predicted
  above one that dominates it, a finding rather than an error;
  *compression*, the hierarchy's top-to-bottom stretch.
- **Settings:** the ideal is chosen among positions with at least 3
  members, by the highest lower confidence bound (`rule = "conf"` in
  the `choose_ideal()` call; the script says when that differs from the
  highest point value, and `rule = "max"` restores the article's rule);
  `poset_axis_limits` in `import-data.R` pins comparable runs to one
  scale.
- **Saves:** the two-step pair (`status_hierarchy_formal_*.pdf`,
  `status_hierarchy_*.pdf`), the whiskered version
  (`status_hierarchy_*_ci.pdf`), `status_hierarchy_*_positions.csv`
  (every position of the full grid: value, interval, `n` members with a
  known count, `n_unknown` without), `status_hierarchy_*_profiles.csv`
  (each cube cell's value, interval, occupancy, and `min_position_n`,
  the smallest `n` among the positions it pools),
  `status_hierarchy_*_shape.csv` (the ideal and the shape metrics), and
  `unrecognized_*.csv` (members of the ideal's local type by ascriptive
  profile: recognized, and recognized if rewarded at the ideal's rate).
  Every section writes to `output/`, or to `output/<run_name>/` when
  `run_name` is set in `import-data.R`.
- **Feeds:** the end of the line: the figure the analysis was for.

[`synthetic_walkthrough.Rmd`](../synthetic_walkthrough.Rmd) pairs the
article's Empirical Illustration with the code for all five sections;
knit it for a page with the figures in place. [`decisions-you-will-face.md`](decisions-you-will-face.md)
collects the choices each section leaves to you, and
[`interpreting-results.md`](interpreting-results.md) explains how to
read what each one saves.
