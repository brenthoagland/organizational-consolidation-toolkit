# Input data requirements

Every method reads one table: a row for each member of the organization
and a column for each role below. Name the columns as you like and
declare them once in `import-data.R`. Run that file on its own to check
the table before running the methods.

| Role | Type | What it holds |
|---|---|---|
| identifier | id | one unique value per member |
| ascriptive statuses (one or more) | nominal | the externally imposed categories, e.g. race, gender. Name each one's reference category (the level the regressions compare against, by convention the dominant one) in `import-data.R`'s `reference` block |
| local criteria (one or more) | categorical or ordinal | the context-specific attributes evaluators use. A criterion is treated as nominal unless declared otherwise: list an ordinal criterion with its levels, low to high, in the `ordinal` block; name a continuous criterion with its number of bins in the `discretize` block, and it is binned at load and treated as ordinal |
| status-position outcomes (one or more) | count | how much recognition each member received for a status position, e.g. the number of nominations as "respected" |
| controls (optional) | any | exposure or selection adjustments |

**One typing on both file routes.** `import-data.R` gives every declared column its type at load, whether the file is the bundled `.rds` or a `.csv` you exported, so the two reach the methods with the same types and the same levels in the same order. Ordinal criteria take the order you declare. Every other categorical column (ascriptive statuses, nominal criteria, categorical controls) takes the order of its `labels` declaration when it has one, numeric order when its values are codes, and alphabetical order otherwise, with the declared reference category first. The identifier stays text. A factor saved in an `.rds` is re-levelled by the same rule, so the class numbering and the labels in the figures do not depend on which file you started from.

Network measures are computed beforehand and enter as columns, as a
criterion or a control; `degree_centrality` in the synthetic data is one
such column, binned into quartiles. The methods do not read a network.

**The synthetic data** (a school, like the case study):

| Column | Role | Values |
|---|---|---|
| `member_id` | identifier | unique |
| `race`, `gender` | ascriptive statuses | White/Black/Latinx/Other; female/male |
| `grades` | local criterion | A–D |
| `homework`, `degree_centrality` | local criteria | quantile bins (discretized) |
| `sports` | local criterion | yes/no |
| `dating`, `change_friends` | local criteria | ordinal levels |
| `troublemaker`, `popular`, `respected` | outcomes | 0–33 nominations |
| `grade_level` | control | 9–12 |

In a workplace the criteria might be skills, performance indicators,
communication styles, or peer ties, and the status positions
"promotable" or "cultural fit".

## Two kinds of data

**Observed criteria.** Surveys or observation record the attributes
evaluators use but rarely formalize: communication style, demeanor,
rapport, presentation of self, and relational measures such as network
position and peer ties. Declare those columns as criteria; methods/03
recovers the local types from them, and methods/04 and 05 build on
those types.

**Administrative records.** Job titles, departments, and ranks record
formal roles, and the formalized measures that come with them:
performance metrics, attendance, evaluations. When nothing finer is
available, formal roles can stand in for local types: supply them as a
`local_type` column (labels or numeric codes), and methods/04 and 05 use
it in place of the latent-class step. Two cautions from the article
apply. Formalized measures follow the division of labor, so an analysis
built on them tends to recover the formal roles rather than the local
types. And crossing formal roles with ascriptive statuses measures
occupational segregation, who fills which position, which is related to
organizational consolidation but is not the same thing.

**Validation.** Run on its own, `import-data.R` checks that the declared
columns exist, that the identifier is unique with one row per member,
that outcome counts are non-negative integers, and that criteria are
categorical. It flags continuous criteria for the `discretize` block and
warns about small cells.
