# Interpreting your results

[`decisions-you-will-face.md`](decisions-you-will-face.md) covers the
choices you make before results exist; this page is about reading what
comes out. It follows the article's Table 2: for each method, the
intuition, the analytic payoff, what characterizes organizational
consolidation, and what high versus low consolidation looks like.
Methods/04 has no Table 2 entry: its predicted profiles are what the
status hierarchy orders, and its model tables reproduce the shape of
Appendix D's.

## The initial assessment ([methods/01](../methods/01-measure-consolidation))

- **Association magnitudes.** Each pair is measured with the statistic
  matched to its levels of measurement, as in the article's Figure 4:
  phi (binary by binary), point-biserial (binary by ordinal or
  continuous), eta (nominal by ordinal or continuous), Cramér's V
  (nominal by nominal). All values are reported as magnitudes on a 0–1
  scale, and the `measure` column names each pair's statistic. Read them
  against Cohen's (1988) thresholds: about 0.1 low, 0.3 medium, 0.5
  high, as rough orientation only. The `by_ascriptive` rollup averages
  across criteria measured with different statistics, so treat it as a
  relative signal and check `$pairwise` for the values. Small samples
  with sparse cells inflate association measures a little, and Cohen's
  numbers are calibrated to 2×2 tables: for an r × c table the
  equivalent benchmark shrinks by sqrt(min(r, c) − 1), so the same V on
  a wider table is a larger effect.
- **Mutual information.** How much knowing a local criterion reduces
  uncertainty about an ascriptive status, and vice versa. It needs
  categories, which is why the script stops on a numeric variable with
  many distinct values. Mutual information and total correlation have no
  fixed thresholds; the synthetic low-consolidation file is a
  near-independence baseline. Total correlation measures dependence
  among all the listed variables at once, criterion with criterion
  included, so it is not on its own a measure of organizational
  consolidation; the pairwise magnitudes isolate the alignment of
  ascription with criteria.

## Association rule mining ([methods/02](../methods/02-association-rule-mining))

- **Intuition.** Attributes can signal meaning alone or only when
  bundled; their significance depends on patterned co-occurrences.
- **Analytic payoff.** Surfaces salient attributes: distinguishes
  standalone versus bundled attributes (conditional salience) and
  detects polysemy across pairings.
- **What characterizes organizational consolidation.** A combination
  space for categorization: rules can only form from attribute bundles
  that co-occur, with dense overlaps constraining and sparse overlaps
  expanding the combination space.
- **High vs. low.** High consolidation: rules dominated by ascriptive
  statuses, a narrow combination space, low polysemy. Low
  consolidation: rules independent of ascriptive statuses, a wider
  combination space, high polysemy.
- **Reading the output.** Lift = 1 is chance co-occurrence. Conviction,
  the diamond size in the rule network, penalizes rules that fail when
  their antecedents hold. Gondal (2022, *Social Networks* 68) is a
  sociological application of rule mining to the meanings of multiplex
  ties.

## Latent class analysis ([methods/03](../methods/03-latent-class-analysis))

- **Intuition.** Given local criteria clusters, attributes are better
  treated as bundles than as isolated attributes.
- **Analytic payoff.** Identifies local types in context: recovers
  co-occurring local criteria profiles and assesses their alignment
  with ascriptive statuses to compare organizational consolidation
  across settings.
- **What characterizes organizational consolidation.** Alignment
  between latent classes (local types) and ascriptive statuses,
  measured with Cramér's V, the most direct assessment. Methods/03
  saves it as `type_by_ascriptive_V.csv`; on the synthetic regimes it
  runs from about 0.12 to 0.52 as consolidation rises.
- **High vs. low.** High consolidation: local types add little
  predictive information beyond ascriptive statuses alone. Low
  consolidation: both contribute independent predictive information.

## Partial order set modeling ([methods/05](../methods/05-status-hierarchy))

- **Intuition.** Evaluations are multidimensional; hierarchies emerge
  by comparing intersectional profiles relative to an ideal actor.
- **Analytic payoff.** Maps intersectionality through status
  hierarchies: orders profiles relative to an ideal actor and
  visualizes lattice/diamond/linear structures.
- **What characterizes organizational consolidation.** The shape of
  the poset structure: whether recognition follows a linear chain, a
  stratified diamond, or lattice-like pathways.
- **High vs. low.** High consolidation: a chain-like hierarchy;
  recognition tracks ascriptive lines. Low consolidation: a
  lattice-like hierarchy; recognition varies within groups.
- **Reading the shape metrics.** The article describes a hierarchy by
  its verticality (the depth of status distinctions) and its clarity
  (how unambiguous relative positions are). `compression`, the
  hierarchy's top-to-bottom stretch, measures verticality and is the
  diagnostic that moves with consolidation: on the synthetic regimes it
  runs 1.8 → 4.3 → 6.2 while total recognition barely changes, so
  consolidation stretches the hierarchy without adding recognition.
  `within_tier_spread` and `tier_separation` measure clarity.
  `monotonicity_violations` counts profiles predicted above one that
  dominates them; a violation is a finding about dimensions pulling
  against each other, not an error (the synthetic regimes have none).
  `comparability` and `equivalence_rate` depend only on which cells the
  poset holds; every cube cell is valued whether or not anyone occupies
  it (`n = 0` in the profiles table marks an empty one), so on a fixed
  set of dimensions they do not move. In every saved profile, position,
  and cube table, `n` counts the members whose count for this outcome is
  known, the members the model was fit on, and `n_unknown` the members
  holding that profile whose count is NA; they enter the prediction with
  their covariates but never the fit. `status_hierarchy_*_positions.csv`
  lists every position of the full grid with its `n`, and the cube's
  `min_position_n` is the smallest of those among the positions a cell
  pools, so a thin position stays visible behind a well-populated cell. The association magnitudes of
  methods/01 diagnose consolidated criteria; the hierarchy diagnoses
  consolidated recognition. Criteria can consolidate before the
  recognition order does, which is why both are reported.
- **Who meets the ideal's criteria but goes unrecognized.** The
  `unrecognized_*` table lists the members who share the ideal's local
  type, by ascriptive profile: how many were recognized (one nomination
  or more), and how many the model expects when their ascriptive
  statuses are set to the ideal's (`at_ideal_rate`; `at_ideal_share`
  applies the ideal cell's observed share instead). `additional` is the
  difference, and the Total row sums it over every profile but the
  ideal's own. This is the article's diagnostic step after Figure 9: the
  profiles that merit recognition under the organization's own standards
  but go unrecognized. Quote the total with its condition: how many more
  members would be recognized if the ideal's type were rewarded at the
  ideal's rate. `additional` is `at_ideal_rate` minus `recognized`, and
  is below zero where a profile was recognized more often than the
  ideal's rate implies. Members whose count is unknown are never counted
  as recognized or not: each profile's `n_unknown` says how many were left
  out, a profile whose members are all unknown keeps its row with `n = 0`
  and nothing else, and the Total's `n_unknown` counts every one of them.
