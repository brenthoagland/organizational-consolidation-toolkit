# Synthetic example data

Every value in these three files is invented. They match the case study's
dimensions (251 members, 33 evaluators) so the examples feel realistic,
and they differ in one respect: how tightly the ascriptive statuses
couple to the local types that drive the criteria and the recognition.
That coupling is what organizational consolidation measures, so the three
files are low, moderate, and high consolidation by construction.

| File | Type-level V (race), methods/03 | Mean criterion-level association (race), methods/01 |
|---|---|---|
| `synthetic_low_consolidation.rds` | ~0.12 | ~0.08 |
| `synthetic_moderate_consolidation.rds` | ~0.28 | ~0.25 |
| `synthetic_high_consolidation.rds` | ~0.52 | ~0.47 |

The criterion-level associations are weaker than the type-level V because
the criteria measure the types with noise.

The direction of the group-to-type assignments follows patterns the
school-stratification literature documents; the magnitudes make no
empirical claim about any group, and none about the article's case study.

The method scripts, the walkthrough, and the README figures use the low
file, which stands in for the article's school, where consolidation was
low (main text, Figure 8). To see each diagnostic move with
consolidation, point `import-data.R`'s `data_path` at each file in turn.
