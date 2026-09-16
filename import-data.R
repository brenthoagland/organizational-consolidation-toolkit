# import-data.R
#
# This file names your table and its columns. Fill it in, then run it:
# on its own, it imports your table and checks it against the
# requirements in data/data-dictionary.md. Each method script reads it
# too, to load the table through oc_load_data() below.
#
# The data/ folder holds the requirements (data/data-dictionary.md) and
# the three synthetic files. As shipped, this file points at the
# synthetic low-consolidation file, so the toolkit runs before you
# change anything.
#
# Three blocks follow. Fill in REQUIRED before your first run. Set the
# two DECIDED ALONG THE WAY settings when methods/03 and methods/05 ask
# for them. Leave OPTIONAL as it is unless you want different labels or
# separate output folders.

oc_config <- list(

  # -- REQUIRED -----------------------------------------------------------

  # Your table: one row per member, as .rds or .csv.
  data_path = file.path("data", "synthetic", "synthetic_low_consolidation.rds"),

  # Your column names, by role (data/data-dictionary.md defines the roles).
  id         = "member_id",        # unique per member
  ascriptive = c("race", "gender"),
  criteria   = c("grades", "homework", "sports", "dating", "change_friends",
                 "degree_centrality"),
  outcomes   = c("troublemaker", "popular", "respected"),  # counts received
  controls   = c("grade_level"),   # character(0) if none

  # Ordinal criteria and their levels, low to high. Methods/01 measures
  # an ordinal criterion with the statistic for ordered categories. A
  # criterion left out is treated as nominal. A .csv cannot store order,
  # so this list is what gives a CSV the same types as the bundled .rds.
  ordinal = list(
    grades            = c("A", "B", "C", "D"),
    homework          = c("q1", "q2", "q3", "q4", "q5"),
    dating            = c("none", "1-2", ">2"),
    change_friends    = c("never", "rarely", "sometimes", "often"),
    degree_centrality = c("q1", "q2", "q3", "q4")
  ),

  # Columns that use numbers as category codes, such as a numeric race
  # code or a coded control. The loader treats them as categories.
  # Ascriptive statuses, criteria, and a supplied local_type are
  # categorical by role, so list only the extras. Name every categorical
  # control here: a control left out enters the regressions as a
  # quantity, and a CSV turns a categorical control into numbers.
  nominal = c("grade_level"),

  # The reference category of each ascriptive status: the level the
  # regressions (methods/04 and 05) compare the others against. The
  # article used the dominant category (White, male). Without this, a
  # .csv's first level in alphabetical order becomes the reference.
  reference = c(race = "White", gender = "male"),

  # Continuous criteria to bin into quantiles at load, criterion -> number
  # of bins (R_functions/discretize.R). A binned criterion is ordinal
  # from then on and needs no `ordinal` entry. The synthetic data arrive
  # binned, so this is empty; with raw tenure you would write
  # discretize = list(tenure = 4).
  discretize = list(),

  # The count model for the recognition regressions (methods/04 and 05).
  # "poisson" is the article's zero-inflated Poisson. Use "negbin" when
  # the counts are overdispersed beyond what the zero inflation absorbs.
  count_model = "poisson",

  # The transaction unit for rule mining (methods/02). "nomination", the
  # article's unit, counts each member once per nomination received and
  # needs two or more status positions to contrast. "member" counts each
  # member once (ever nominated or not); use it with a single outcome.
  arm_unit = "nomination",

  # -- DECIDED ALONG THE WAY ----------------------------------------------

  # How many local types your organization has. Run methods/03, read the
  # class-count sweep it prints and saves, set your number here, and run
  # methods/03 again so that methods/04 and 05 use it. The 4 below is the
  # synthetic data's answer.
  n_types = 4,

  # The class counts methods/03 compares before you set n_types (the rows
  # of its Table C1). The article searched one to ten; 1:6 keeps the
  # demonstration quick.
  class_range = 1:6,

  # The outcome the status hierarchy (methods/05) is drawn for: one of
  # the columns in `outcomes`. Choose a valued recognition. The ideal
  # actor is the profile with the highest count of this outcome, so for
  # a stigma label such as "troublemaker" the top of the hierarchy is the
  # most stigmatized profile.
  focal_outcome = "respected",

  # -- OPTIONAL -----------------------------------------------------------

  # A name for this run. NULL writes to output/. A name such as "wave1"
  # writes every section's outputs, the methods/03 run of record
  # included, to output/<run_name>/, so a run on another regime, wave, or
  # subgroup does not overwrite the last one.
  run_name = NULL,

  # Display names for the local types, class number -> label. Name a
  # class only after reading its class-probability table from
  # methods/03; class numbers belong to your run of record. Classes left
  # out display as "type 1", "type 2", and so on.
  type_labels = NULL,

  # Shared y-axis limits c(lo, hi) for the status hierarchy, for
  # comparing drawings across runs. NULL scales each drawing to its own values.
  poset_axis_limits = NULL,

  # How many rules the rule network (methods/02) draws per status
  # position, highest lift first. NULL draws every surviving rule, as the
  # article's Figure 6 did with its 48, until more than 200 survive; then
  # the script says so and draws 25 per position. A number sets the cap;
  # Inf draws every rule. The saved CSV always keeps every rule.
  max_rules_per_position = NULL,

  # Display names for variables in figure legends. A variable left out
  # shows its column name.
  variable_names = c(change_friends = "Friends", degree_centrality = "Centrality",
                     grades = "Grades", homework = "Homework",
                     sports = "Sports", dating = "Dating"),

  # Display labels for levels in figures, one named vector per variable,
  # level -> label. A level left out shows as "variable: level".
  labels = list(
    race           = c(White = "White", Black = "Black",
                       Latinx = "Latinx", Other = "Other"),
    gender         = c(female = "Female", male = "Male"),
    grades         = c(A = "A's", B = "B's", C = "C's", D = "D's"),
    homework       = c(q1 = "HW: lowest", q2 = "HW: low", q3 = "HW: mid",
                       q4 = "HW: high", q5 = "HW: highest"),
    sports         = c(yes = "Athlete", no = "Non-athlete"),
    dating         = c(none = "No dating", `1-2` = "Dated 1-2",
                       `>2` = "Dated 2+"),
    change_friends = c(never = "Keeps friends", rarely = "Rarely changes",
                       sometimes = "Sometimes changes",
                       often = "Changes friends"),
    degree_centrality = c(q1 = "Ties: lowest", q2 = "Ties: low",
                          q3 = "Ties: mid", q4 = "Ties: high")
  )
)

# Read the table named in data_path and apply the declarations above.
oc_load_data <- function(cfg = oc_config) {
  if (grepl("[.]csv$", cfg$data_path, ignore.case = TRUE)) {
    # a blank cell is a missing value, not an "" category; text stays
    # text here, and the schema below gives every declared column its type
    data <- read.csv(cfg$data_path, stringsAsFactors = FALSE,
                     na.strings = c("NA", ""))
  } else if (grepl("[.]rds$", cfg$data_path, ignore.case = TRUE)) {
    data <- readRDS(cfg$data_path)
  } else {
    stop("data_path must be .rds or .csv: ", cfg$data_path)
  }
  oc_apply_schema(data, cfg)
}

# Give each declared column the type its role requires, so a CSV and the
# bundled .rds reach the methods as the same types and the same levels in
# the same order: ordinal criteria become ordered factors in the declared
# order; ascriptive statuses, nominal columns, categorical controls, and
# nominal criteria become unordered factors whose level order is the
# order of their `labels` declaration when they have one, numeric order
# for codes, and alphabetical otherwise, with the declared reference
# first; continuous criteria named in `discretize` are binned; the id
# stays text. A factor saved in an .rds is re-levelled by the same rule,
# so the two routes cannot disagree.
oc_apply_schema <- function(data, cfg = oc_config) {
  to_factor <- function(x, v) {
    vals <- if (is.factor(x)) as.character(x) else x
    obs  <- unique(vals[!is.na(vals)])
    declared <- names(cfg$labels[[v]])
    numeric_like <- is.numeric(vals) ||
      !anyNA(suppressWarnings(as.numeric(as.character(obs))))
    lv <- if (!is.null(declared) && any(obs %in% declared)) {
      c(declared[declared %in% obs], sort(as.character(setdiff(obs, declared))))
    } else if (numeric_like) {
      as.character(sort(unique(as.numeric(as.character(obs)))))  # 9 < 10 < 12, not "10" < "9"
    } else {
      sort(as.character(obs))
    }
    factor(as.character(vals), levels = lv)
  }
  if (cfg$id %in% names(data)) data[[cfg$id]] <- as.character(data[[cfg$id]])
  # continuous criteria first: discretize() returns an ordered factor
  for (v in intersect(names(cfg$discretize), names(data)))
    data[[v]] <- discretize(data[[v]], n = cfg$discretize[[v]])
  # categorical by role, whether the file holds labels or numeric codes
  for (v in intersect(c(cfg$ascriptive, cfg$nominal, "local_type"), names(data)))
    data[[v]] <- to_factor(data[[v]], v)
  # a control that holds text or a saved factor is categorical too; a
  # numeric control not named in `nominal` stays a quantity
  for (v in intersect(cfg$controls, names(data))) {
    x <- data[[v]]
    if ((is.character(x) || (is.factor(x) && !is.ordered(x))) && !v %in% cfg$nominal)
      data[[v]] <- to_factor(x, v)
  }
  # the declared reference category first
  for (v in intersect(names(cfg$reference), names(data))) {
    if (!is.factor(data[[v]]) || is.ordered(data[[v]])) next
    ref <- cfg$reference[[v]]
    if (!ref %in% levels(data[[v]]))
      stop(sprintf("%s: reference level \"%s\" is not among its levels (%s)",
                   v, ref, paste(levels(data[[v]]), collapse = ", ")),
           call. = FALSE)
    data[[v]] <- stats::relevel(data[[v]], ref = ref)
  }
  for (v in intersect(cfg$criteria, names(data))) {
    x <- data[[v]]
    if (v %in% names(cfg$ordinal)) {
      lv <- cfg$ordinal[[v]]
      seen <- setdiff(unique(as.character(x)), c(lv, NA))
      if (length(seen) > 0)
        stop(sprintf("%s has values not in its declared ordinal levels: %s",
                     v, paste(seen, collapse = ", ")), call. = FALSE)
      data[[v]] <- factor(as.character(x), levels = lv, ordered = TRUE)
    } else if (is.ordered(x)) {
      # already ordered (binned above): keep its order
    } else if (is.numeric(x) && length(unique(x[!is.na(x)])) > 12) {
      # continuous and not declared for binning: leave it numeric, so
      # check_data() stops and points to the discretize block
    } else {
      data[[v]] <- to_factor(x, v)
    }
  }
  data
}

# Run this file on its own (Rscript import-data.R, or Source in RStudio),
# from the repository root, and it imports your table and checks it
# against the requirements in data/data-dictionary.md. The method scripts
# read this file for the form and the loader and skip the check. A script
# of your own that sources this file gets the same; to check the table
# from it, call check_data(oc_load_data()).
if (!exists("check_data")) {
  if (!dir.exists("R_functions")) {
    stop("run import-data.R from the repository root, the folder that holds ",
         "R_functions/ and data/: setwd() there, or open ",
         "organizational-consolidation-toolkit.Rproj", call. = FALSE)
  }
  for (f in list.files("R_functions", pattern = "[.]R$", full.names = TRUE)) source(f)
}
if (run_on_its_own("import-data.R")) check_data(oc_load_data(), oc_config)
