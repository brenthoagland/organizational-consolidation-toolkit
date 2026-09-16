# Decisions you will face

The methods run on their defaults, but the analytic decisions are yours.
This page lists them in the order they come up, with where each is set,
its default, and what the article's case study chose. The case study's
choices are reference points, not recommendations.

| Decision | Where you set it | Comes up at |
|---|---|---|
| [Binning continuous criteria](#binning-continuous-criteria) | `discretize` in [`import-data.R`](../import-data.R) | at load |
| [Members or nominations as transactions](#members-or-nominations-as-transactions) | `arm_unit` in [`import-data.R`](../import-data.R) | methods/02 |
| [Rule thresholds](#rule-thresholds) | `supp`, `conf` in [`methods/02`](../methods/02-association-rule-mining/mine-attribute-bundles.R) | methods/02 |
| [How many local types](#how-many-local-types) | `n_types` in [`import-data.R`](../import-data.R) | methods/03 |
| [Members with missing criteria](#members-with-missing-criteria) | `na.rm` in [`methods/03`](../methods/03-latent-class-analysis/recover-local-types.R) | methods/03 |
| [Modal or proportional assignment](#modal-or-proportional-assignment) | the local-type block in [`methods/04`](../methods/04-regression/predict-recognition.R) | methods/04 |
| [Poisson or negative binomial](#poisson-or-negative-binomial) | `count_model` in [`import-data.R`](../import-data.R) | methods/04 and 05 |
| [Which outcome is focal](#which-outcome-is-focal) | `focal_outcome` in [`import-data.R`](../import-data.R) | methods/05 |
| [Which dimensions define a profile](#which-dimensions-define-a-profile) | `ascriptive` in [`import-data.R`](../import-data.R) | methods/05 |
| [Supplied or empirical ideal](#supplied-or-empirical-ideal) | the `choose_ideal()` call in [`methods/05`](../methods/05-status-hierarchy/build-status-hierarchy.R) | methods/05 |

Two behaviors are automatic: the [small-cell protections](#small-cells)
in methods/05, and the [defaults you can leave alone](#defaults-you-can-leave-alone).

## Binning continuous criteria

- **Where:** the `discretize` block in [`import-data.R`](../import-data.R),
  criterion → number of bins, e.g. `discretize = list(centrality = 4)`.
  The loader bins the criterion into quantiles and treats it as ordinal
  from then on.
- **Until you decide:** the data check and methods/01 stop on a numeric
  criterion with more than 12 distinct values.
- **The case study:** quartiles of degree centrality, quintiles of
  homework effort.

More bins give finer criteria but sparser categories. Match the
granularity the organization uses: if it talks in high, medium, and
low, three bins beat five. Heavy ties can collapse quantile edges, so
the result may have fewer levels than asked; check with `table()`.

## Members or nominations as transactions

- **Where:** `arm_unit` in [`import-data.R`](../import-data.R).
- **Default:** `"nomination"`, the case study's unit.

With `"nomination"`, each member counts once per nomination received,
and a rule reads: among the nominations attached to members holding
this bundle, what share are for this position? Lift compares that share
with the position's base rate, so a surviving rule marks a bundle
disproportionately tied to a position, not a cause of recognition.
Frequently nominated members weigh more, and the contrast needs two or
more status positions. With `"member"`, each member counts once, and a
rule reads: among members holding this bundle, what share were ever
nominated for this position? Use it when a few heavily nominated
members would dominate, or when you have one recognition outcome.

## Rule thresholds

- **Where:** `supp` and `conf` in the `apriori()` call and the
  `lift > 1` filter, all in [`methods/02`](../methods/02-association-rule-mining/mine-attribute-bundles.R).
- **Default and the case study:** support 0.05, confidence 0.50,
  lift > 1, no minimum number of attributes per rule. The article's
  finding that no single attribute is necessary or sufficient depends
  on leaving single-attribute rules in.

If many rules survive (the synthetic data yields 111), raise support or
confidence, or keep the full set and cap only the drawing:
`max_rules_per_position` in `import-data.R` keeps that many highest-lift
rules per position in the figure. Its default draws every rule, as the
article's Figure 6 did with its 48, until more than 200 survive; then
the script says so and draws 25 per position (`Inf` forces every
rule). The saved CSV always holds every rule.

## How many local types?

- **Where:** `n_types` in [`import-data.R`](../import-data.R).
- **When:** after methods/03's first run. Read the class-count sweep it
  prints and saves (Table C1's columns), set `n_types`, and run
  methods/03 again so that methods/04 and 05 use the choice.
- **The case study:** four classes, the solution that balanced fit,
  classification error, class sizes, and interpretability.
- **Also set here:** `class_range` in `import-data.R` names the counts
  the sweep compares (default `1:6`; the article searched one to ten),
  and `nrep` in methods/03's sweep and final fit sets the random starts
  (10; the article does not state a per-fit count).

Fit statistics narrow the candidates but rarely settle the choice. At
realistic sample sizes they split: on the synthetic data (n = 251), BIC
prefers 3 and AIC prefers 4, and BIC in particular can point low.
[`blrt()`](../R_functions/latent_types.R) breaks ties formally but is
slow. The deciding question is substantive: does the k-class solution
name types members of the organization would recognize? Check
`Entropy.R2` (separation) and the class shares; a class under about 5%
of members is usually too small to interpret.

## Members with missing criteria

- **Where:** `na.rm` in the `poLCA()` call in methods/03.
- **Default:** `na.rm = FALSE`. Members with missing criteria stay in;
  poLCA estimates from their observed values, and every member gets a
  type. The assignments then line up one-to-one with your table, which
  methods/04 and 05 depend on.

`na.rm = TRUE` would drop those members from the fit and leave fewer
assignments than rows, so the later sections do not support it. If you
want those members out, drop them from your data before methods/03, so
every section sees the same members.

## Modal or proportional assignment?

- **Where:** the local-type block in [`methods/04`](../methods/04-regression/predict-recognition.R).
- **Default and the article's main specification:** proportional. The
  type terms are each member's posterior class probabilities, entered as
  they are. Modal assignment is the article's robustness check, which
  methods/04 prints. A `local_type` column you supply enters as
  categories, since it has no posteriors.

How much this matters depends on class separation. With well-separated
classes (`Entropy.R2` near 1; the synthetic data reads 0.98) the
posteriors are nearly 0 or 1 and the two agree (the printed check reads
a correlation of 0.999). At lower separation the two diverge. As
Appendix D states, neither modal nor proportional assignment eliminates
classification-error bias; the intervals the methods report are
conditional on the fitted classes. Methods/05 groups members into
positions by their modal type either way, because a position is a
category.

## Poisson or negative binomial?

- **Where:** `count_model` in [`import-data.R`](../import-data.R), read by
  methods/04 and 05.
- **Default and the case study:** `"poisson"`, a zero-inflated Poisson.

The zero-inflated model has two parts: a logit for the odds of no
nominations, and a count model for the rate of nominations among
members at risk of being labeled. Switch to `"negbin"` when the counts
are overdispersed beyond what the zero-inflation absorbs.

## Which outcome is focal?

- **Where:** `focal_outcome` in [`import-data.R`](../import-data.R), one of
  your `outcomes`.
- **The case study:** "respected", the clearest proxy for institutional
  legitimacy.

Methods/04 models every outcome; methods/05 draws the hierarchy for the
focal outcome only. Change it and rerun methods/05 for another
hierarchy.

The construction defines the ideal actor as the profile with the highest
predicted count of the focal outcome, and the article's vocabulary
(ideal, esteem, stigma at a distance) assumes that outcome is a valued
recognition, as "respected" is. With a stigma label such as
"troublemaker" the arithmetic still runs, but the top of the hierarchy
is the profile most saddled with the label, its prototype rather than
the organization's ideal. Such a hierarchy can be worth drawing; read
its top as "most stigmatized". The article drew its poset for
"respected" only.

## Which dimensions define a profile?

- **Where:** `ascriptive` in [`import-data.R`](../import-data.R); the
  hierarchy's dimensions are those columns plus the local type.
- **The case study:** race, gender, and local type, each reduced to the
  ideal's level versus the rest (White/non-White,
  studious/non-studious).

Every added dimension multiplies the profiles and thins each one, so
declare the statuses your question is about. Methods/05 reduces each
dimension to the ideal's level versus the rest, as the article did, so
the lattice stays readable; the full multi-level grid stops being
legible past about a dozen profiles.

## Supplied or empirical ideal?

- **Where:** the `choose_ideal()` call in
  [`methods/05`](../methods/05-status-hierarchy/build-status-hierarchy.R):
  its `rule` and `ideal` arguments.
- **Default:** the occupied position with the highest lower confidence
  bound on predicted recognition (`rule = "conf"`). When that differs
  from the highest point value, the script names both.
- **The article's rule:** the highest point value (`rule = "max"`).

`ideal =` sets a theoretical ideal instead. If another profile outscores
it, the function warns and names that profile rather than substituting
it; read the divergence as a finding about the organization. The
confidence-bound default exists because the full grid in which the
ideal is found fragments quickly (four races × two genders × four types
= 32 cells), and a small cell's noisy peak would otherwise set the ideal
for everything downstream.

## Small cells

Two automatic protections in methods/05:

- The ideal is chosen among positions with at least 3 members, and the
  script says when a position or a cube cell has no members or fewer
  than 3. Every position is saved with its `n`
  (`status_hierarchy_<outcome>_positions.csv`), and each cube cell
  carries `min_position_n`, the smallest `n` among the positions it
  pools; the shape table records the rule and cutoff the ideal was
  chosen by.
- The confidence-bound rule discounts a noisy peak by its uncertainty.

## Defaults you can leave alone

These settings affect precision or presentation, not what is measured.
`run_name` in `import-data.R` names a run: NULL writes to `output/`, and a
name writes every section's outputs, run of record included, to
`output/<run_name>/`, so a second regime, wave, or subgroup does not
overwrite the first. Methods/01 draws its permutation null 20 times
(`n_permutations`). Methods/03 restarts EM 10 times in the sweep and 10
in the final fit. `max_rules_per_position` caps the rule-network
drawing (above). The seeds are fixed, so reruns reproduce.
