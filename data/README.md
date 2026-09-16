# Data

This repository ships **synthetic data only**. The article's data were
collected under an IRB protocol that does not permit public sharing.

The three files in `synthetic/` mimic the structure of the case (variable
types, number of records) at three constructed levels of organizational
consolidation: low, moderate, high. [`synthetic/README.md`](synthetic/README.md)
says what each file contains.

Figures and tables produced from these files illustrate the methods; they
do not reproduce the article's results. Everything saved from a synthetic
run carries a `synthetic_` filename prefix and a provenance note on the
figure.

**Your own data:** requirements in `data-dictionary.md`. The form that
names your table, `import-data.R` at the top level, checks it against
them when run on its own.
