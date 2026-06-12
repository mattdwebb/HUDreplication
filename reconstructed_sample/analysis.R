# Load required libraries
library(lfe)
library(dplyr)
library(tidyr)
library(haven)

resolve_repo_root <- function() {
  cmd_args <- commandArgs(FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  start_dirs <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (length(file_arg) == 1) {
    script_arg <- sub("^--file=", "", file_arg)
    if (!file.exists(script_arg)) {
      script_arg <- gsub("~+~", " ", script_arg, fixed = TRUE)
    }
    script_path <- normalizePath(script_arg, winslash = "/", mustWork = TRUE)
    start_dirs <- c(dirname(script_path), start_dirs)
  }

  source_files <- unlist(
    lapply(sys.frames(), function(frame) {
      if (is.character(frame$ofile) && length(frame$ofile) == 1) frame$ofile else NULL
    }),
    use.names = FALSE
  )
  if (length(source_files) > 0) {
    source_paths <- normalizePath(source_files, winslash = "/", mustWork = TRUE)
    start_dirs <- c(dirname(source_paths), start_dirs)
  }

  for (start_dir in unique(start_dirs)) {
    repo_root <- start_dir
    while (repo_root != dirname(repo_root)) {
      if (
        dir.exists(file.path(repo_root, "reconstructed_sample")) &&
        dir.exists(file.path(repo_root, "Data"))
      ) {
        return(repo_root)
      }
      repo_root <- dirname(repo_root)
    }
  }

  stop("Could not infer repo_root. Run from HUDreplication or reconstructed_sample.")
}

repo_root <- resolve_repo_root()

setwd(repo_root)

args <- commandArgs(trailingOnly = TRUE)
cluster_level <- Sys.getenv("RECONSTRUCTED_SAMPLE_CLUSTER", "trial")
if (cluster_level == "") cluster_level <- "trial"
if (any(args %in% c("market", "--cluster=market", "--market-clustering"))) {
  cluster_level <- "market"
}
if (!cluster_level %in% c("trial", "market")) {
  stop("cluster_level must be either 'trial' or 'market'")
}
cluster_note <- if (cluster_level == "market") {
  "Cluster-robust standard errors in parentheses; clustered at the market level. 95\\% confidence intervals in square brackets."
} else {
  "Cluster-robust standard errors in parentheses; clustered at the trial level. 95\\% confidence intervals in square brackets."
}

data_dir <- file.path(repo_root, "Data")
reconstructed_sample_generated_dir <- file.path(data_dir, "Generated", "reconstructed_sample")
if (cluster_level == "market") {
  tables_dir <- file.path(repo_root, "reconstructed_sample", "market_clustering", "Tables")
  appendix_tables_dir <- file.path(repo_root, "reconstructed_sample", "market_clustering", "Appendix_Tables")
} else {
  tables_dir <- file.path(repo_root, "reconstructed_sample", "Tables")
  appendix_tables_dir <- file.path(repo_root, "reconstructed_sample", "Appendix_Tables")
}

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(appendix_tables_dir, showWarnings = FALSE, recursive = TRUE)

progress_message <- function(message) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), message))
  flush.console()
}

progress_message(paste0("Running reconstructed_sample/analysis.R from ", repo_root))
progress_message(paste0("Cluster level: ", cluster_level))

rank_warn_log <- list()
current_table <- "startup"

progress_message("Loading official-pass trial list from taf.sas7bdat")
official_pass_controls <- read_sas(file.path(data_dir, "HDS2012_Raw_Data", "taf.sas7bdat")) %>%
  filter(grepl("-S[A-Z]-", CONTROL)) %>%
  group_by(CONTROL) %>%
  summarise(
    release1 = any(RELEASE == "1", na.rm = TRUE),
    fpass1 = any(FPASS == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(release1, fpass1) %>%
  select(CONTROL)
progress_message(sprintf("Official-pass controls retained: %s", length(unique(official_pass_controls$CONTROL))))

restrict_to_official_pass <- function(df) {
  semi_join(df, official_pass_controls, by = "CONTROL")
}

rank_warning_handler <- function(w) {
  msg <- conditionMessage(w)
  if (grepl("rank-deficient", msg, ignore.case = TRUE)) {
    rank_warn_log <<- append(rank_warn_log, list(list(table = current_table, message = msg)))
    invokeRestart("muffleWarning")
  }
}

withCallingHandlers({
valid_controls <- c(
    "was_first_visitor", "am_indicator_first", "month", "ARELATE2",
    "THHEGAI", "TPEGAI", "THIGHEDU", "TCURTENR", "ALGNCUR",
    "AELNG1", "DPMTEXP", "AMOVERS", "age", "ALEASETP", "ACAROWN"
)
valid_controls_fml <- paste(valid_controls, collapse = " + ")
valid_control_factors <- setdiff(valid_controls, "age")
missing_category_label <- "Did not answer"

as_control_factor <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[!is.na(x_chr) & x_chr == "-1"] <- missing_category_label
  as.factor(x_chr)
}

appointment_valid_controls <- c(
    "visit_order", "am_indicator", setdiff(valid_controls, c("was_first_visitor", "am_indicator_first"))
)
appointment_valid_controls_fml <- paste(appointment_valid_controls, collapse = " + ")
appointment_valid_control_factors <- setdiff(appointment_valid_controls, "age")

current_table <- "Table 5"

# Import the data
progress_message("Table 5: loading sales/tester data")
data <- read.csv(file.path(reconstructed_sample_generated_dir, "sales_and_tester_merged.csv")) %>%
    restrict_to_official_pass() %>%
    filter(
        !is.na(STOTUNIT_TOTAL) & 
        !is.na(SAVLBAD_ANY) &
        !is.na(RACE) &
        if_all(all_of(valid_controls), ~ !is.na(.))
    ) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        across(all_of(valid_control_factors), as_control_factor),
        age = as.numeric(age)
    ) %>%
    mutate(
        site = as.factor(substr(as.character(CONTROL), 1, 2)),
        cluster_group = if (cluster_level == "market") site else CONTROL,
        ofcolor = ifelse(RACE %in% c(2,3,4), 1, 0),
        got_second_appointment = as.integer(num_visits >= 2)
    )
if (!"SAVLBAD_FIRST_LITERAL" %in% names(data) && "SAVLBAD_LITERAL_FIRST" %in% names(data)) {
    data$SAVLBAD_FIRST_LITERAL <- data$SAVLBAD_LITERAL_FIRST
}

# Note that the only RACE categories present in valid trials are those indicated by 1, 2, 3, 4 (white, black, hispanic, asian)
summary(data$RACE)

# Run regressions with felm
progress_message(sprintf("Table 5: running tester-level models (%s rows, %s trials)", nrow(data), n_distinct(data$CONTROL)))
recommended_total_races <- felm(
    as.formula(paste("STOTUNIT_TOTAL ~ RACE +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = data
)

available_any_races <- felm(
    as.formula(paste("SAVLBAD_ANY ~ RACE +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = data
)

recommended_total_ofcolor <- felm(
    as.formula(paste("STOTUNIT_TOTAL ~ ofcolor +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = data
)

available_any_ofcolor <- felm(
    as.formula(paste("SAVLBAD_ANY ~ ofcolor +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = data
)

available_any_literal_races <- NULL
available_any_literal_ofcolor <- NULL
literal_available_data <- NULL
if ("SAVLBAD_ANY_LITERAL" %in% names(data)) {
    literal_available_data <- data %>%
        filter(!is.na(SAVLBAD_ANY_LITERAL)) %>%
        group_by(CONTROL) %>%
        filter(n_distinct(TESTERID) == 2) %>%
        ungroup()

    available_any_literal_races <- felm(
        as.formula(paste("SAVLBAD_ANY_LITERAL ~ RACE +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
        data = literal_available_data
    )

    available_any_literal_ofcolor <- felm(
        as.formula(paste("SAVLBAD_ANY_LITERAL ~ ofcolor +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
        data = literal_available_data
    )
}

second_appointment_races <- felm(
    as.formula(paste("got_second_appointment ~ RACE +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = data
)

second_appointment_ofcolor <- felm(
    as.formula(paste("got_second_appointment ~ ofcolor +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = data
)

first_appointment_data <- data %>%
    filter(!is.na(SAVLBAD_FIRST)) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup()

progress_message(sprintf("Table 5: running first-appointment models (%s rows, %s trials)", nrow(first_appointment_data), n_distinct(first_appointment_data$CONTROL)))
available_first_races <- felm(
    as.formula(paste("SAVLBAD_FIRST ~ RACE +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = first_appointment_data
)

available_first_ofcolor <- felm(
    as.formula(paste("SAVLBAD_FIRST ~ ofcolor +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = first_appointment_data
)

available_first_literal_races <- NULL
available_first_literal_ofcolor <- NULL
literal_first_appointment_data <- NULL
if ("SAVLBAD_FIRST_LITERAL" %in% names(data)) {
    literal_first_appointment_data <- data %>%
        filter(!is.na(SAVLBAD_FIRST_LITERAL)) %>%
        group_by(CONTROL) %>%
        filter(n_distinct(TESTERID) == 2) %>%
        ungroup()

    available_first_literal_races <- felm(
        as.formula(paste("SAVLBAD_FIRST_LITERAL ~ RACE +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
        data = literal_first_appointment_data
    )

    available_first_literal_ofcolor <- felm(
        as.formula(paste("SAVLBAD_FIRST_LITERAL ~ ofcolor +", valid_controls_fml, "| CONTROL | 0 | cluster_group")),
        data = literal_first_appointment_data
    )
}

# Display summaries of previous models
summary(recommended_total_races)
summary(available_any_races)
summary(second_appointment_races)
summary(recommended_total_ofcolor)
summary(available_any_ofcolor)
summary(second_appointment_ofcolor)
summary(available_first_races)
summary(available_first_ofcolor)

# Alternate specification keeping each appointment as a separate row
progress_message("Table 5: loading appointment-level data")
appointments_data <- read.csv(file.path(reconstructed_sample_generated_dir, "sales_and_tester_appointments.csv"))  %>%
    restrict_to_official_pass() %>%
    filter(
        !is.na(STOTUNIT) &
        !is.na(SAVLBAD_BINARY) &
        !is.na(RACE) &
        if_all(all_of(appointment_valid_controls), ~ !is.na(.))
    ) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        across(all_of(appointment_valid_control_factors), as_control_factor),
        age = as.numeric(age)
    ) %>%
    mutate(
        site = as.factor(substr(as.character(CONTROL), 1, 2)),
        cluster_group = if (cluster_level == "market") site else CONTROL,
        ofcolor = ifelse(RACE %in% c(2,3,4), 1, 0)
    )


# Run regressions with felm for appointments data
progress_message(sprintf("Table 5: running appointment-level models (%s rows, %s trials)", nrow(appointments_data), n_distinct(appointments_data$CONTROL)))
recommended_apps_races <- felm(
    as.formula(paste("STOTUNIT ~ RACE +", appointment_valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = appointments_data
)

available_apps_races <- felm(
    as.formula(paste("SAVLBAD_BINARY ~ RACE +", appointment_valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = appointments_data
)

recommended_apps_ofcolor <- felm(
    as.formula(paste("STOTUNIT ~ ofcolor +", appointment_valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = appointments_data
)

available_apps_ofcolor <- felm(
    as.formula(paste("SAVLBAD_BINARY ~ ofcolor +", appointment_valid_controls_fml, "| CONTROL | 0 | cluster_group")),
    data = appointments_data
)

# Display results for appointments analysis
summary(recommended_apps_races)
summary(available_apps_races)
summary(recommended_apps_ofcolor)
summary(available_apps_ofcolor)

sample_summary <- data.frame(
    reconstructed_sample_rows = nrow(data),
    reconstructed_sample_controls = length(unique(data$CONTROL)),
    appointment_rows = nrow(appointments_data),
    appointment_controls = length(unique(appointments_data$CONTROL))
)
write.csv(sample_summary, file.path(tables_dir, "sample_summary.csv"), row.names = FALSE)

library(broom)
library(xtable)

# Get info for "ofcolor" (Racial Minority) and "RACE" levels
get_race_rows <- function(model, race_levels) {
    rows <- lapply(race_levels, function(race) {
        info <- extract_coef_info(model, race)
        c(
            sprintf("% .4f", info$est),
            sprintf("(% .4f)", info$se),
            sprintf("[% .4f,% .4f]", info$ci[1], info$ci[2])
        )
    })
    do.call(rbind, rows)
}
# Helper to get significance stars
get_stars <- function(pval) {
    if (is.na(pval)) return("")
    if (pval < 0.01) return("\\sym{***}")
    if (pval < 0.05) return("\\sym{**}")
    if (pval < 0.10) return("\\sym{*}")
    return("")
}

# Modified extract_coef_info to include p-value and stars
extract_coef_info <- function(model, varname) {
    coefs <- coef(summary(model))
    est <- coefs[varname, "Estimate"]
    se <- coefs[varname, "Cluster s.e."]
    pval <- coefs[varname, "Pr(>|t|)"]
    ci <- confint(model, level = 0.95)[varname, ]
    star <- get_stars(pval)
    list(est = est, se = se, ci = ci, star = star)
}

# Get info for "ofcolor" (Racial Minority) and "RACE" levels
get_race_rows <- function(model, race_levels) {
    rows <- lapply(race_levels, function(race) {
        info <- extract_coef_info(model, race)
        c(
            sprintf("% .4f%s", info$est, info$star),
            sprintf("(% .4f)", info$se),
            sprintf("[% .4f,% .4f]", info$ci[1], info$ci[2])
        )
    })
    do.call(rbind, rows)
}

# Prepare rows for each column
race_vars <- c("RACE2", "RACE3", "RACE4") # African American, Hispanic, Asian

table_specs <- list(
    list(
        label = "Total Recommended\\\\Properties",
        minority = extract_coef_info(recommended_total_ofcolor, "ofcolor"),
        race = get_race_rows(recommended_total_races, race_vars),
        minority_model = recommended_total_ofcolor,
        race_model = recommended_total_races,
        white_mean = mean(data$STOTUNIT_TOTAL[as.character(data$RACE) == "1"], na.rm = TRUE),
        trials = length(unique(data$CONTROL))
    ),
    list(
        label = "Ad Property Ever\\\\Available",
        minority = extract_coef_info(available_any_ofcolor, "ofcolor"),
        race = get_race_rows(available_any_races, race_vars),
        minority_model = available_any_ofcolor,
        race_model = available_any_races,
        white_mean = mean(data$SAVLBAD_ANY[as.character(data$RACE) == "1"], na.rm = TRUE),
        trials = length(unique(data$CONTROL))
    ),
    list(
        label = "Received Second\\\\Appointment",
        minority = extract_coef_info(second_appointment_ofcolor, "ofcolor"),
        race = get_race_rows(second_appointment_races, race_vars),
        minority_model = second_appointment_ofcolor,
        race_model = second_appointment_races,
        white_mean = mean(data$got_second_appointment[as.character(data$RACE) == "1"], na.rm = TRUE),
        trials = length(unique(data$CONTROL))
    ),
    list(
        label = "Recommended Properties\\\\per Appointment",
        minority = extract_coef_info(recommended_apps_ofcolor, "ofcolor"),
        race = get_race_rows(recommended_apps_races, race_vars),
        minority_model = recommended_apps_ofcolor,
        race_model = recommended_apps_races,
        white_mean = mean(appointments_data$STOTUNIT[as.character(appointments_data$RACE) == "1"], na.rm = TRUE),
        trials = length(unique(appointments_data$CONTROL))
    ),
    list(
        label = "Ad Property Available\\\\First Appointment",
        minority = extract_coef_info(available_first_ofcolor, "ofcolor"),
        race = get_race_rows(available_first_races, race_vars),
        minority_model = available_first_ofcolor,
        race_model = available_first_races,
        white_mean = mean(first_appointment_data$SAVLBAD_FIRST[as.character(first_appointment_data$RACE) == "1"], na.rm = TRUE),
        trials = length(unique(first_appointment_data$CONTROL))
    )
)

coef_summary_row <- function(model, term, estimand, label) {
    coefs <- coef(summary(model))
    if (!term %in% rownames(coefs)) return(NULL)
    se_col <- if ("Cluster s.e." %in% colnames(coefs)) "Cluster s.e." else "Std. Error"
    p_col <- grep("^Pr\\(", colnames(coefs), value = TRUE)
    if (length(p_col) == 0) p_col <- "Pr(>|t|)"
    data.frame(
        estimand = estimand,
        term = label,
        estimate = as.numeric(coefs[term, "Estimate"]),
        std_error = as.numeric(coefs[term, se_col]),
        p_value = as.numeric(coefs[term, p_col[1]]),
        observations = model$N,
        stringsAsFactors = FALSE
    )
}

if (!is.null(available_any_literal_ofcolor) && !is.null(available_any_literal_races)) {
    literal_sensitivity <- bind_rows(
        coef_summary_row(available_any_ofcolor, "ofcolor", "SAVLBAD_ANY", "Racial Minority"),
        coef_summary_row(available_any_races, "RACE2", "SAVLBAD_ANY", "African American"),
        coef_summary_row(available_any_races, "RACE3", "SAVLBAD_ANY", "Hispanic"),
        coef_summary_row(available_any_races, "RACE4", "SAVLBAD_ANY", "Asian"),
        coef_summary_row(available_any_literal_ofcolor, "ofcolor", "SAVLBAD_ANY_LITERAL", "Racial Minority"),
        coef_summary_row(available_any_literal_races, "RACE2", "SAVLBAD_ANY_LITERAL", "African American"),
        coef_summary_row(available_any_literal_races, "RACE3", "SAVLBAD_ANY_LITERAL", "Hispanic"),
        coef_summary_row(available_any_literal_races, "RACE4", "SAVLBAD_ANY_LITERAL", "Asian")
    )
    write.csv(
        literal_sensitivity,
        file.path(tables_dir, "table5_savlbad_literal_sensitivity.csv"),
        row.names = FALSE
    )
    progress_message("Wrote Table 5 SAVLBAD literal-availability sensitivity CSV")
}

cell_for <- function(spec, race_index, stat_index) {
    if (is.null(race_index)) {
        info <- spec$minority
        if (stat_index == 1) return(sprintf("% .4f%s", info$est, info$star))
        if (stat_index == 2) return(sprintf("(% .4f)", info$se))
        return(sprintf("[% .4f,% .4f]", info$ci[1], info$ci[2]))
    }
    if (nrow(spec$race) < race_index) return(NA)
    spec$race[race_index, stat_index]
}

table_rows <- rbind(
    c("Racial Minority", sapply(table_specs, cell_for, race_index = NULL, stat_index = 1)),
    c("", sapply(table_specs, cell_for, race_index = NULL, stat_index = 2)),
    c("", sapply(table_specs, cell_for, race_index = NULL, stat_index = 3)),
    c("African American", sapply(table_specs, cell_for, race_index = 1, stat_index = 1)),
    c("", sapply(table_specs, cell_for, race_index = 1, stat_index = 2)),
    c("", sapply(table_specs, cell_for, race_index = 1, stat_index = 3)),
    c("Hispanic", sapply(table_specs, cell_for, race_index = 2, stat_index = 1)),
    c("", sapply(table_specs, cell_for, race_index = 2, stat_index = 2)),
    c("", sapply(table_specs, cell_for, race_index = 2, stat_index = 3)),
    c("Asian", sapply(table_specs, cell_for, race_index = 3, stat_index = 1)),
    c("", sapply(table_specs, cell_for, race_index = 3, stat_index = 2)),
    c("", sapply(table_specs, cell_for, race_index = 3, stat_index = 3))
)

# Create LaTeX table with proper column labels
latex_table <- function(rows) {
    cat("\\begin{table}[p]\n\\centering\n")
    cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
    cat("\\caption{Discriminatory Steering and Availability of Advertised Properties\\\\[0.5em]\\textit{Table 5, C\\&T 2022}}\n")
    cat("\\label{tab:table5}\n")
    cat("\\resizebox{\\textwidth}{!}{\n")
    cat("\\begin{tabular}{l*{5}{c}}\n")
    cat("\\toprule\n")
    cat("& \\multicolumn{5}{c}{Dependent Variable} \\\\\n")
    cat("\\cmidrule(lr){2-6}\n")
    for (spec in table_specs) {
        cat(sprintf("&\\multicolumn{1}{c}{\\begin{tabular}{@{}c@{}}%s\\end{tabular}} ", spec$label))
    }
    cat("\\\\\n")
    cat("\\midrule\n")
    for (row in 1:nrow(rows)) {
        cat(paste(rows[row,], collapse=" & "), "\\\\\n")
        if (row %% 3 == 0 && row < nrow(rows)) cat("[1ex]\n")
    }
    cat("\\midrule\n")
    cat(sprintf("Comparison mean (white)      &%s\\\\\n",
        paste(sprintf("%.3f", sapply(table_specs, function(spec) spec$white_mean)), collapse = "&")))
    cat(sprintf("Observations      &%s\\\\\n",
        paste(sapply(table_specs, function(spec) spec$minority_model$N), collapse = "&")))
    cat(sprintf("Adjusted R$^2$ (Minority)      &%s\\\\\n",
        paste(sprintf("%.4f", sapply(table_specs, function(spec) summary(spec$minority_model)$adj.r.squared)), collapse = "&")))
    cat(sprintf("Adjusted R$^2$ (Category)      &%s\\\\\n",
        paste(sprintf("%.4f", sapply(table_specs, function(spec) summary(spec$race_model)$adj.r.squared)), collapse = "&")))
    cat(sprintf("Number of Trials      &%s\\\\\n",
        paste(sapply(table_specs, function(spec) spec$trials), collapse = "&")))
    cat("\\bottomrule\n")
    cat("\\multicolumn{6}{l}{\\footnotesize Comparison mean (white) is the raw mean for white testers in the corresponding estimation sample.}\\\\\n")
    cat("\\multicolumn{6}{l}{\\footnotesize ", cluster_note, "}\\\\\n", sep = "")
    cat("\\multicolumn{6}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
    cat("\\end{tabular}\n}\n\\end{table}\n")
}

write_table <- function(path, expr) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  out <- capture.output(expr)
  writeLines(out, path)
  progress_message(paste0("Wrote ", path))
}

progress_message("Table 5: formatting output table")
write_table(file.path(tables_dir, "table5.tex"), latex_table(table_rows))


# =================================================================================================== #
# TABLES 6-12 (PAIRED-TESTER DESIGN)
# =================================================================================================== #

progress_message("Loading cleaned HDS data for Tables 6-12 and appendix outputs")
cleaned_data <- read.csv(file.path(reconstructed_sample_generated_dir, "cleaned_hds.csv")) %>%
  restrict_to_official_pass() %>%
  mutate(
    RACE = as.factor(RACE),
    CONTROL = as.factor(CONTROL),
    TSEX_num = as.numeric(as.character(TSEX)),
    across(all_of(valid_control_factors), as_control_factor),
    age = as.numeric(age),
    ofcolor = ifelse(RACE %in% c(2, 3, 4), 1, 0),
    kids = as.numeric(kids),
    mother = if_else(!is.na(mother), mother, if_else(kids == 1 & TSEX_num == 0, 1, 0)),
    site = as.factor(substr(as.character(CONTROL), 1, 2)),
    cluster_group = if (cluster_level == "market") site else CONTROL
  )

base_covariates <- valid_controls
base_covariates_fml <- paste(base_covariates, collapse = " + ")
base_covariates_mom <- base_covariates
base_covariates_mom_fml <- paste(base_covariates_mom, collapse = " + ")
race_share_controls <- c("percent_white", "percent_black", "percent_hispanic", "percent_asian")
race_share_controls_fml <- paste(race_share_controls, collapse = " + ")

current_table <- "Table 6"
progress_message("Table 6: running racial-composition outcome models")

completed_pair_filter <- function(df) {
  df %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup()
}

prep_table_data <- function(df, outcome_vars, extra_vars = character(), require_completed_pair = TRUE) {
  required_vars <- unique(c(outcome_vars, "RACE", base_covariates, extra_vars))
  out <- df %>%
    filter(if_all(all_of(required_vars), ~ !is.na(.)))
  if (require_completed_pair) out <- completed_pair_filter(out)
  out
}

comparison_mean_note <- "Comparison mean (white) is the raw mean for white testers in the corresponding estimation sample."

comparison_mean_white <- function(outcomes, data_list, transform = identity) {
  mapply(function(outcome, df) {
    outcome_values <- transform(df[[outcome]])
    mean(outcome_values[as.character(df$RACE) == "1"], na.rm = TRUE)
  }, outcomes, data_list)
}

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("\\sym{***}")
  if (p < 0.05) return("\\sym{**}")
  if (p < 0.10) return("\\sym{*}")
  ""
}

fint <- function(x) gsub(",", "{,}", formatC(as.integer(x), format = "d", big.mark = ","))

extract_coef_info <- function(model, varname) {
  s <- coef(summary(model))
  if (is.null(s) || !varname %in% rownames(s)) {
    return(list(est = "", est_star = "", se = "", ci = c("", ""), p = NA))
  }
  se_col <- if ("Cluster s.e." %in% colnames(s)) "Cluster s.e." else "Std. Error"
  p_col  <- grep("^Pr\\(", colnames(s), value = TRUE)
  if (length(p_col) == 0) p_col <- "Pr(>|t|)"

  est <- as.numeric(s[varname, "Estimate"])
  se  <- as.numeric(s[varname, se_col])
  p   <- suppressWarnings(as.numeric(s[varname, p_col[1]]))

  df  <- model$df.residual
  tcrit <- qt(0.975, df = df)
  ci_lo <- est - tcrit * se
  ci_hi <- est + tcrit * se
  stars <- star_code(p)

  list(
    est      = sprintf("% .4f", est),
    est_star = paste0(sprintf("% .4f", est), stars),
    se       = sprintf("(% .4f)", se),
    ci       = c(sprintf("[% .4f", ci_lo), sprintf("% .4f]", ci_hi)),
    p        = p
  )
}

extract_coef_numeric <- function(model, varname) {
  s <- coef(summary(model))
  if (is.null(s) || !varname %in% rownames(s)) {
    return(NULL)
  }
  se_col <- if ("Cluster s.e." %in% colnames(s)) "Cluster s.e." else "Std. Error"
  p_col  <- grep("^Pr\\(", colnames(s), value = TRUE)
  if (length(p_col) == 0) p_col <- "Pr(>|t|)"

  est <- as.numeric(s[varname, "Estimate"])
  se  <- as.numeric(s[varname, se_col])
  p   <- suppressWarnings(as.numeric(s[varname, p_col[1]]))
  df  <- model$df.residual
  tcrit <- qt(0.975, df = df)

  list(
    estimate = est,
    se = se,
    ci_lo = est - tcrit * se,
    ci_hi = est + tcrit * se,
    p = p
  )
}

cells_for_coef <- function(models, coef_name, type = c("est_star", "se", "ci")) {
  type <- match.arg(type)
  sapply(models, function(m) {
    info <- extract_coef_info(m, coef_name)
    switch(type,
           est_star = info$est_star,
           se       = info$se,
           ci       = paste(info$ci, collapse = ", "))
  }, USE.NAMES = FALSE)
}

build_rows <- function(models_minority, models_race, outcome_labels,
                       race_vars = c("RACE2", "RACE3", "RACE4"),
                       race_labels = c("African American", "Hispanic", "Asian")) {
  rm_est <- c("Racial Minority", cells_for_coef(models_minority, "ofcolor", "est_star"))
  rm_se  <- c("",                 cells_for_coef(models_minority, "ofcolor", "se"))
  rm_ci  <- c("",                 cells_for_coef(models_minority, "ofcolor", "ci"))

  race_blocks <- lapply(seq_along(race_vars), function(i) {
    est <- c(race_labels[i], cells_for_coef(models_race, race_vars[i], "est_star"))
    se  <- c("",             cells_for_coef(models_race, race_vars[i], "se"))
    ci  <- c("",             cells_for_coef(models_race, race_vars[i], "ci"))
    rbind(est, se, ci)
  })

  rows <- rbind(rm_est, rm_se, rm_ci, do.call(rbind, race_blocks))
  colnames(rows) <- c(" ", outcome_labels)
  rows
}

build_race_only_rows <- function(models, outcome_labels,
                                 race_vars = c("RACE2", "RACE3", "RACE4"),
                                 race_labels = c("African American", "Hispanic", "Asian")) {
  race_blocks <- lapply(seq_along(race_vars), function(i) {
    est <- c(race_labels[i], cells_for_coef(models, race_vars[i], "est_star"))
    se  <- c("",             cells_for_coef(models, race_vars[i], "se"))
    ci  <- c("",             cells_for_coef(models, race_vars[i], "ci"))
    rbind(est, se, ci)
  })
  rows <- do.call(rbind, race_blocks)
  colnames(rows) <- c(" ", outcome_labels)
  rows
}

build_model_group_rows <- function(model_groups, outcome_labels, coef_names,
                                   group_labels = names(model_groups)) {
  if (is.null(group_labels)) {
    group_labels <- paste("Group", seq_along(model_groups))
  }
  if (length(coef_names) == 1) {
    coef_names <- rep(coef_names, length(model_groups))
  }

  group_blocks <- lapply(seq_along(model_groups), function(i) {
    est <- c(group_labels[i], cells_for_coef(model_groups[[i]], coef_names[i], "est_star"))
    se  <- c("",              cells_for_coef(model_groups[[i]], coef_names[i], "se"))
    ci  <- c("",              cells_for_coef(model_groups[[i]], coef_names[i], "ci"))
    rbind(est, se, ci)
  })

  rows <- do.call(rbind, group_blocks)
  colnames(rows) <- c(" ", outcome_labels)
  rows
}

latex_table_multi <- function(rows, outcome_labels, caption, label,
                              models_minority, models_race, n_trials,
                              stat_lines = NULL, note_lines = NULL) {
  n_cols <- length(outcome_labels)
  n_trials_vals <- if (length(n_trials) == 1) rep(n_trials, n_cols) else n_trials
  if (is.null(note_lines)) {
    note_lines <- c(
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  }
  cat("\\begin{table}[p]\n\\centering\n")
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  cat("\\resizebox{\\textwidth}{!}{%\n")
  cat(sprintf("\\begin{tabular}{l%s}\n", paste(rep("c", n_cols), collapse = "")))
  cat("\\toprule\n")
  cat("& \\multicolumn{", n_cols, "}{c}{Dependent variable} \\\\\n", sep = "")
  cat(sprintf("\\cmidrule(lr){2-%d}\n", n_cols + 1))
  cat(paste0("& ", paste(outcome_labels, collapse = " & "), " \\\\\n"))
  cat("\\midrule\n")

  apply(rows, 1, function(r) cat(paste(r, collapse = " & "), " \\\\\n"))

  cat("\\midrule\n")
  if (!is.null(stat_lines) && length(stat_lines) > 0) {
    for (stat_label in names(stat_lines)) {
      stat_vals <- stat_lines[[stat_label]]
      if (length(stat_vals) == 1) stat_vals <- rep(stat_vals, n_cols)
      cat(stat_label)
      for (val in stat_vals) cat(sprintf(" & %s", val))
      cat("\\\\\n")
    }
  }
  cat("Observations")
  for (j in seq_along(models_minority)) cat(sprintf(" & %s", fint(models_minority[[j]]$N)))
  cat("\\\\\n")
  cat("Adjusted R$^2$ (Minority)")
  for (j in seq_along(models_minority)) cat(sprintf(" & %.4f", summary(models_minority[[j]])$adj.r.squared))
  cat("\\\\\n")
  cat("Adjusted R$^2$ (Category)")
  for (j in seq_along(models_race)) cat(sprintf(" & %.4f", summary(models_race[[j]])$adj.r.squared))
  cat("\\\\\n")
  cat("Number of Trials")
  for (j in seq_along(models_minority)) cat(sprintf(" & %s", fint(n_trials_vals[j])))
  cat("\\\\\n")
  cat("\\bottomrule\n")
  if (!is.null(note_lines) && length(note_lines) > 0) {
    for (note in note_lines) {
      cat("\\multicolumn{", n_cols + 1,
          "}{l}{\\footnotesize ", note, "}\\\\\n", sep = "")
    }
  }
  cat("\\end{tabular}%\n")
  cat("}\n\\end{table}\n")
}

latex_table_race_only <- function(rows, outcome_labels, caption, label, models, n_trials,
                                  stat_lines = NULL, note_lines = NULL) {
  n_cols <- length(outcome_labels)
  n_trials_vals <- if (length(n_trials) == 1) rep(n_trials, n_cols) else n_trials
  if (is.null(note_lines)) {
    note_lines <- c(
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  }
  cat("\\begin{table}[p]\n\\centering\n")
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  cat("\\resizebox{\\textwidth}{!}{%\n")
  cat(sprintf("\\begin{tabular}{l%s}\n", paste(rep("c", n_cols), collapse = "")))
  cat("\\toprule\n")
  cat("& \\multicolumn{", n_cols, "}{c}{Dependent variable} \\\\\n", sep = "")
  cat(sprintf("\\cmidrule(lr){2-%d}\n", n_cols + 1))
  cat(paste0("& ", paste(outcome_labels, collapse = " & "), " \\\\\n"))
  cat("\\midrule\n")

  apply(rows, 1, function(r) cat(paste(r, collapse = " & "), " \\\\\n"))

  cat("\\midrule\n")
  if (!is.null(stat_lines) && length(stat_lines) > 0) {
    for (stat_label in names(stat_lines)) {
      stat_vals <- stat_lines[[stat_label]]
      if (length(stat_vals) == 1) stat_vals <- rep(stat_vals, n_cols)
      cat(stat_label)
      for (val in stat_vals) cat(sprintf(" & %s", val))
      cat("\\\\\n")
    }
  }
  cat("Observations")
  for (j in seq_along(models)) cat(sprintf(" & %s", fint(models[[j]]$N)))
  cat("\\\\\n")
  cat("Adjusted R$^2$")
  for (j in seq_along(models)) cat(sprintf(" & %.4f", summary(models[[j]])$adj.r.squared))
  cat("\\\\\n")
  cat("Number of Trials")
  for (j in seq_along(models)) cat(sprintf(" & %s", fint(n_trials_vals[j])))
  cat("\\\\\n")
  cat("\\bottomrule\n")
  if (!is.null(note_lines) && length(note_lines) > 0) {
    for (note in note_lines) {
      cat("\\multicolumn{", n_cols + 1,
          "}{l}{\\footnotesize ", note, "}\\\\\n", sep = "")
    }
  }
  cat("\\end{tabular}%\n")
  cat("}\n\\end{table}\n")
}

latex_table_custom <- function(rows, outcome_labels, caption, label,
                               stat_lines = NULL, note_lines = NULL) {
  n_cols <- length(outcome_labels)
  if (is.null(note_lines)) {
    note_lines <- c(
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  }

  cat("\\begin{table}[p]\n\\centering\n")
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  cat("\\resizebox{\\textwidth}{!}{%\n")
  cat(sprintf("\\begin{tabular}{l%s}\n", paste(rep("c", n_cols), collapse = "")))
  cat("\\toprule\n")
  cat("& \\multicolumn{", n_cols, "}{c}{Dependent variable} \\\\\n", sep = "")
  cat(sprintf("\\cmidrule(lr){2-%d}\n", n_cols + 1))
  cat(paste0("& ", paste(outcome_labels, collapse = " & "), " \\\\\n"))
  cat("\\midrule\n")

  apply(rows, 1, function(r) cat(paste(r, collapse = " & "), " \\\\\n"))

  if (!is.null(stat_lines) && length(stat_lines) > 0) {
    cat("\\midrule\n")
    for (stat_label in names(stat_lines)) {
      stat_vals <- stat_lines[[stat_label]]
      if (length(stat_vals) == 1) stat_vals <- rep(stat_vals, n_cols)
      cat(stat_label)
      for (val in stat_vals) cat(sprintf(" & %s", val))
      cat("\\\\\n")
    }
  }

  cat("\\bottomrule\n")
  if (!is.null(note_lines) && length(note_lines) > 0) {
    for (note in note_lines) {
      cat("\\multicolumn{", n_cols + 1,
          "}{l}{\\footnotesize ", note, "}\\\\\n", sep = "")
    }
  }
  cat("\\end{tabular}%\n")
  cat("}\n\\end{table}\n")
}

# ----- Table 6: Discriminatory Steering and Neighborhood Racial Composition -----
table6_outcomes <- c("w2012pc_Rec", "percent_black", "percent_hispanic", "percent_asian")
table6_labels <- c("White Share", "Black Share", "Hispanic Share", "Asian Share")
table6_data_list <- lapply(table6_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome))
})

table6_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table6_outcomes, table6_data_list)
table6_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table6_outcomes, table6_data_list)
table6_n_trials <- sapply(table6_data_list, function(df) length(unique(df$CONTROL)))
table6_white_means <- comparison_mean_white(table6_outcomes, table6_data_list)

table6_rows <- build_rows(
  table6_models_minority,
  table6_models_race,
  outcome_labels = table6_labels
)
write_table(
  file.path(tables_dir, "table6.tex"),
  latex_table_multi(
    table6_rows,
    outcome_labels = table6_labels,
    caption = "Discriminatory Steering and Neighborhood Racial Composition\\\\[0.5em]\\textit{Table 6, C\\&T 2022}",
    label = "tab:table6",
    models_minority = table6_models_minority,
    models_race = table6_models_race,
    n_trials = table6_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table6_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

# ----- Appendix Table A11: Literal advertised-home availability -----
if (
  !is.null(available_any_literal_ofcolor) &&
  !is.null(available_any_literal_races) &&
  !is.null(available_first_literal_ofcolor) &&
  !is.null(available_first_literal_races)
) {
  current_table <- "Appendix Table A11"
  progress_message("Appendix Table A11: running literal advertised-home availability models")

  appendix_a11_models_minority <- list(
    available_any_literal_ofcolor,
    available_first_literal_ofcolor
  )
  appendix_a11_models_race <- list(
    available_any_literal_races,
    available_first_literal_races
  )
  appendix_a11_labels <- c(
    "\\begin{tabular}{@{}c@{}}Ad Property Ever\\\\Literally Available\\end{tabular}",
    "\\begin{tabular}{@{}c@{}}Ad Property Literally Available\\\\First Appointment\\end{tabular}"
  )
  appendix_a11_data_list <- list(
    literal_available_data,
    literal_first_appointment_data
  )
  appendix_a11_outcomes <- c("SAVLBAD_ANY_LITERAL", "SAVLBAD_FIRST_LITERAL")
  appendix_a11_white_means <- mapply(function(outcome, df) {
    mean(df[[outcome]][as.character(df$RACE) == "1"], na.rm = TRUE)
  }, appendix_a11_outcomes, appendix_a11_data_list)
  appendix_a11_n_trials <- sapply(appendix_a11_data_list, function(df) length(unique(df$CONTROL)))
  appendix_a11_rows <- build_rows(
    appendix_a11_models_minority,
    appendix_a11_models_race,
    appendix_a11_labels
  )

  write_table(
    file.path(appendix_tables_dir, "tableA11_savlbad_literal_availability.tex"),
    latex_table_multi(
      appendix_a11_rows,
      outcome_labels = appendix_a11_labels,
      caption = "Literal Availability of Advertised Properties\\\\[0.5em]\\textit{Appendix Table A11}",
      label = "tab:appendixA11",
      models_minority = appendix_a11_models_minority,
      models_race = appendix_a11_models_race,
      n_trials = appendix_a11_n_trials,
      stat_lines = list("Comparison mean (white)" = sprintf("%.3f", appendix_a11_white_means)),
      note_lines = c(
        comparison_mean_note,
        "Literal availability treats HDS \\texttt{SAVLBAD} codes 1 and 5 as available. The main C\\&T-style Table 5 availability outcome treats only code 1 as available.",
        cluster_note,
        "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
      )
    )
  )
}

# ----- Table 7: Discriminatory Steering and Neighborhood Racial Composition by Income -----
current_table <- "Table 7"
progress_message("Table 7: running racial-composition-by-income outcome models")
table7_outcomes <- c("WhiteHI_Rec", "WhiteMI_Rec", "WhiteLI_Rec")
table7_labels <- c("White High-Income", "White Middle-Income", "White Low-Income")
table7_data_list <- lapply(table7_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome))
})

table7_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table7_outcomes, table7_data_list)
table7_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table7_outcomes, table7_data_list)
table7_n_trials <- sapply(table7_data_list, function(df) length(unique(df$CONTROL)))
table7_white_means <- comparison_mean_white(table7_outcomes, table7_data_list)

table7_rows <- build_rows(table7_models_minority, table7_models_race, table7_labels)
write_table(
  file.path(tables_dir, "table7.tex"),
  latex_table_multi(
    table7_rows,
    outcome_labels = table7_labels,
    caption = "Discriminatory Steering and Neighborhood Racial Composition by Income\\\\[0.5em]\\textit{Table 7, C\\&T 2022}",
    label = "tab:table7",
    models_minority = table7_models_minority,
    models_race = table7_models_race,
    n_trials = table7_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table7_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

# ----- Table 8: Discriminatory Steering and Neighborhood Effects -----
current_table <- "Table 8A"
progress_message("Table 8A: running school-quality and safety outcome models")
table8a_outcomes <- c("elementary_school_score", "middle_school_score", "Assault_Rec", "Elementary_School_Score_Rec")
table8a_labels <- c("SEDA Elementary", "SEDA Middle", "Assaults", "GreatSchools Elem")
table8a_data_list <- lapply(table8a_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome))
})

table8a_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table8a_outcomes, table8a_data_list)
table8a_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table8a_outcomes, table8a_data_list)
table8a_n_trials <- sapply(table8a_data_list, function(df) length(unique(df$CONTROL)))
table8a_white_means <- comparison_mean_white(table8a_outcomes, table8a_data_list)

table8a_rows <- build_rows(table8a_models_minority, table8a_models_race, table8a_labels)
write_table(
  file.path(tables_dir, "table8a.tex"),
  latex_table_multi(
    table8a_rows,
    outcome_labels = table8a_labels,
    caption = "Discriminatory Steering and Neighborhood Effects (Panel A: School Quality and Safety)\\\\[0.5em]\\textit{Table 8, C\\&T 2022}",
    label = "tab:table8a",
    models_minority = table8a_models_minority,
    models_race = table8a_models_race,
    n_trials = table8a_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table8a_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

current_table <- "Table 8B"
progress_message("Table 8B: running ACS-neighborhood-characteristic outcome models")
table8b_outcomes <- c("povrate_Rec", "skill_Rec", "college_Rec", "singlefamily_Rec", "ownerocc_Rec")
table8b_labels <- c("Poverty Rate", "High Skill", "College", "Single Family", "Ownership Rate")
table8b_data_list <- lapply(table8b_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome))
})

table8b_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table8b_outcomes, table8b_data_list)
table8b_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table8b_outcomes, table8b_data_list)
table8b_n_trials <- sapply(table8b_data_list, function(df) length(unique(df$CONTROL)))
table8b_white_means <- comparison_mean_white(table8b_outcomes, table8b_data_list)

table8b_rows <- build_rows(table8b_models_minority, table8b_models_race, table8b_labels)
write_table(
  file.path(tables_dir, "table8b.tex"),
  latex_table_multi(
    table8b_rows,
    outcome_labels = table8b_labels,
    caption = "Discriminatory Steering and Neighborhood Effects (Panel B: ACS Characteristics)\\\\[0.5em]\\textit{Table 8, C\\&T 2022}",
    label = "tab:table8b",
    models_minority = table8b_models_minority,
    models_race = table8b_models_race,
    n_trials = table8b_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table8b_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

# ----- Table 9: Discriminatory Steering and Local Pollution Exposures -----
current_table <- "Table 9A"
progress_message("Table 9A: running pollution-exposure outcome models")
table9_outcomes <- c("SFcount_Rec", "RSEI_Rec", "PM25_Rec")
table9_labels <- c("Superfund", "Toxics (RSEI)", "PM2.5")
table9_data_list <- lapply(table9_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome))
})

table9_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table9_outcomes, table9_data_list)
table9_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table9_outcomes, table9_data_list)
table9_n_trials <- sapply(table9_data_list, function(df) length(unique(df$CONTROL)))
table9_white_means <- comparison_mean_white(table9_outcomes, table9_data_list)

table9_rows <- build_rows(table9_models_minority, table9_models_race, table9_labels)
write_table(
  file.path(tables_dir, "table9a.tex"),
  latex_table_multi(
    table9_rows,
    outcome_labels = table9_labels,
    caption = "Discriminatory Steering and Local Pollution Exposures (Panel A: Full Sample)\\\\[0.5em]\\textit{Table 9, C\\&T 2022}",
    label = "tab:table9a",
    models_minority = table9_models_minority,
    models_race = table9_models_race,
    n_trials = table9_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table9_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

current_table <- "Table 9B"
progress_message("Table 9B: running pollution-exposure models for mothers")
table9_mom_data_list <- lapply(table9_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome)) %>%
    filter(mother == 1) %>%
    completed_pair_filter()
})
table9_mom_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table9_outcomes, table9_mom_data_list)
table9_mom_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table9_outcomes, table9_mom_data_list)
table9_mom_n_trials <- sapply(table9_mom_data_list, function(df) length(unique(df$CONTROL)))
table9_mom_white_means <- comparison_mean_white(table9_outcomes, table9_mom_data_list)

table9_mom_rows <- build_rows(table9_mom_models_minority, table9_mom_models_race, table9_labels)
write_table(
  file.path(tables_dir, "table9b.tex"),
  latex_table_multi(
    table9_mom_rows,
    outcome_labels = table9_labels,
    caption = "Discriminatory Steering and Local Pollution Exposures (Panel B: Mothers)\\\\[0.5em]\\textit{Table 9, C\\&T 2022}",
    label = "tab:table9b",
    models_minority = table9_mom_models_minority,
    models_race = table9_mom_models_race,
    n_trials = table9_mom_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table9_mom_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

# ----- Table 10: Discriminatory Steering and Neighborhood Effects (Mothers) -----
current_table <- "Table 10A"
progress_message("Table 10A: running school-quality and safety models for mothers")
table10a_outcomes <- table8a_outcomes
table10a_labels <- table8a_labels
table10a_data_list <- lapply(table10a_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome)) %>%
    filter(mother == 1) %>%
    completed_pair_filter()
})

table10a_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table10a_outcomes, table10a_data_list)
table10a_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table10a_outcomes, table10a_data_list)
table10a_n_trials <- sapply(table10a_data_list, function(df) length(unique(df$CONTROL)))
table10a_white_means <- comparison_mean_white(table10a_outcomes, table10a_data_list)

table10a_rows <- build_rows(table10a_models_minority, table10a_models_race, table10a_labels)
write_table(
  file.path(tables_dir, "table10a.tex"),
  latex_table_multi(
    table10a_rows,
    outcome_labels = table10a_labels,
    caption = "Discriminatory Steering and Neighborhood Effects (Mothers, Panel A)\\\\[0.5em]\\textit{Table 10, C\\&T 2022}",
    label = "tab:table10a",
    models_minority = table10a_models_minority,
    models_race = table10a_models_race,
    n_trials = table10a_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table10a_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

current_table <- "Table 10B"
progress_message("Table 10B: running ACS-neighborhood-characteristic models for mothers")
table10b_outcomes <- table8b_outcomes
table10b_labels <- table8b_labels
table10b_data_list <- lapply(table10b_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome)) %>%
    filter(mother == 1) %>%
    completed_pair_filter()
})

table10b_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table10b_outcomes, table10b_data_list)
table10b_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, table10b_outcomes, table10b_data_list)
table10b_n_trials <- sapply(table10b_data_list, function(df) length(unique(df$CONTROL)))
table10b_white_means <- comparison_mean_white(table10b_outcomes, table10b_data_list)

table10b_rows <- build_rows(table10b_models_minority, table10b_models_race, table10b_labels)
write_table(
  file.path(tables_dir, "table10b.tex"),
  latex_table_multi(
    table10b_rows,
    outcome_labels = table10b_labels,
    caption = "Discriminatory Steering and Neighborhood Effects (Mothers, Panel B)\\\\[0.5em]\\textit{Table 10, C\\&T 2022}",
    label = "tab:table10b",
    models_minority = table10b_models_minority,
    models_race = table10b_models_race,
    n_trials = table10b_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table10b_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

# ----- Table 11: Low-Poverty Neighborhoods (dropped: no within-control variation) -----

# ----- Table 12: Median Income in Neighborhood -----
current_table <- "Table 12"
progress_message("Table 12: running median-income models")
table12_data_all <- prep_table_data(cleaned_data, c("medincome_Rec")) %>%
  filter(medincome_Rec > 0)
table12_data_fam <- table12_data_all %>%
  filter(kids == 1) %>%
  completed_pair_filter()
table12_data_mom <- table12_data_all %>%
  filter(kids == 1 & TSEX_num == 0) %>%
  completed_pair_filter()
table12_n_trials <- c(
  length(unique(table12_data_all$CONTROL)),
  length(unique(table12_data_fam$CONTROL)),
  length(unique(table12_data_mom$CONTROL))
)

table12_models <- list(
  felm(as.formula(paste("log(medincome_Rec) ~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group")), data = table12_data_all),
  felm(as.formula(paste("log(medincome_Rec) ~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group")), data = table12_data_fam),
  felm(as.formula(paste("log(medincome_Rec) ~ RACE +", base_covariates_mom_fml, "| CONTROL | 0 | cluster_group")), data = table12_data_mom)
)
table12_white_means <- c(
  mean(log(table12_data_all$medincome_Rec[as.character(table12_data_all$RACE) == "1"]), na.rm = TRUE),
  mean(log(table12_data_fam$medincome_Rec[as.character(table12_data_fam$RACE) == "1"]), na.rm = TRUE),
  mean(log(table12_data_mom$medincome_Rec[as.character(table12_data_mom$RACE) == "1"]), na.rm = TRUE)
)

table12_labels <- c("All Testers", "Families", "Moms")
table12_rows <- build_race_only_rows(table12_models, table12_labels)
write_table(
  file.path(tables_dir, "table12.tex"),
  latex_table_race_only(
    table12_rows,
    outcome_labels = table12_labels,
    caption = "Discriminatory Steering: Median Income in Neighborhood\\\\[0.5em]\\textit{Table 12, C\\&T 2022}",
    label = "tab:table12",
    models = table12_models,
    n_trials = table12_n_trials,
    stat_lines = list("Comparison mean (white)" = sprintf("%.3f", table12_white_means)),
    note_lines = c(
      comparison_mean_note,
      cluster_note,
      "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
    )
  )
)

# =================================================================================================== #
# APPENDIX TABLES AND AUXILIARY OUTPUTS
# =================================================================================================== #

# ----- Appendix Table A1: Expanded Table 6 -----
current_table <- "Appendix Table A1"
progress_message("Appendix Table A1: running expanded racial-composition models")
appendix_a1_outcomes <- c("w2012pc_Rec", "percent_black", "percent_hispanic", "percent_asian")
appendix_a1_labels <- c("White Share", "Black Share", "Hispanic Share", "Asian Share")
appendix_a1_data_list <- lapply(appendix_a1_outcomes, function(outcome) {
  prep_table_data(cleaned_data, c(outcome))
})

appendix_a1_models_race <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, appendix_a1_outcomes, appendix_a1_data_list)
appendix_a1_models_minority <- Map(function(outcome, df) {
  fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "| CONTROL | 0 | cluster_group"))
  felm(fml, data = df)
}, appendix_a1_outcomes, appendix_a1_data_list)
appendix_a1_n_trials <- sapply(appendix_a1_data_list, function(df) length(unique(df$CONTROL)))

appendix_a1_rows <- build_rows(
  appendix_a1_models_minority,
  appendix_a1_models_race,
  appendix_a1_labels
)
write_table(
  file.path(appendix_tables_dir, "tableA1_expanded_racial_composition.tex"),
  latex_table_multi(
    appendix_a1_rows,
    outcome_labels = appendix_a1_labels,
    caption = "Expanded Neighborhood Racial Composition Outcomes\\\\[0.5em]\\textit{Appendix Table A1}",
    label = "tab:appendixA1",
    models_minority = appendix_a1_models_minority,
    models_race = appendix_a1_models_race,
    n_trials = appendix_a1_n_trials
  )
)

# ----- Appendix Tables A2-A5: Within-race linear models for kids and mothers -----
family_aux_outcomes <- c("w2012pc_Rec", "elementary_school_score", "middle_school_score", "Elementary_School_Score_Rec", "povrate_Rec")
family_aux_labels <- c("White Share", "SEDA Elem", "SEDA Middle", "GreatSchools Elem", "Poverty Rate")
family_aux_races <- c("1", "2", "3", "4")
family_aux_race_labels <- c("White", "African American", "Hispanic", "Asian")
family_aux_files <- c(
  file.path(appendix_tables_dir, "tableA2_white_family_status.tex"),
  file.path(appendix_tables_dir, "tableA3_black_family_status.tex"),
  file.path(appendix_tables_dir, "tableA4_hispanic_family_status.tex"),
  file.path(appendix_tables_dir, "tableA5_asian_family_status.tex")
)
family_aux_table_ids <- c("A2", "A3", "A4", "A5")

for (i in seq_along(family_aux_races)) {
  current_table <- paste("Appendix Table", family_aux_table_ids[i])
  progress_message(paste0(current_table, ": running within-race family-status models"))
  race_df <- cleaned_data %>% filter(as.character(RACE) == family_aux_races[i])
  family_data_list <- lapply(family_aux_outcomes, function(outcome) {
    prep_table_data(race_df, c(outcome), extra_vars = c("kids", "mother"), require_completed_pair = FALSE)
  })

  kids_models <- Map(function(outcome, df) {
    fml <- as.formula(paste(outcome, "~ kids +", base_covariates_fml, "| 0 | 0 | cluster_group"))
    felm(fml, data = df)
  }, family_aux_outcomes, family_data_list)
  mother_models <- Map(function(outcome, df) {
    fml <- as.formula(paste(outcome, "~ mother +", base_covariates_fml, "| 0 | 0 | cluster_group"))
    felm(fml, data = df)
  }, family_aux_outcomes, family_data_list)

  family_rows <- build_model_group_rows(
    list(kids_models, mother_models),
    outcome_labels = family_aux_labels,
    coef_names = c("kids", "mother"),
    group_labels = c("Kids", "Mother")
  )

  family_stat_lines <- list(
    "Observations" = sapply(kids_models, function(m) fint(m$N)),
    "Adjusted R$^2$ (Kids model)" = sapply(kids_models, function(m) sprintf("%.4f", summary(m)$adj.r.squared)),
    "Adjusted R$^2$ (Mother model)" = sapply(mother_models, function(m) sprintf("%.4f", summary(m)$adj.r.squared)),
    "Number of Trials" = sapply(family_data_list, function(df) fint(length(unique(df$CONTROL))))
  )

  write_table(
    family_aux_files[i],
    latex_table_custom(
      family_rows,
      outcome_labels = family_aux_labels,
      caption = paste0(
        "Within-Race Linear Models of Family Status and Steering Outcomes (",
        family_aux_race_labels[i],
        " Testers)\\\\[0.5em]\\textit{Appendix Table ",
        family_aux_table_ids[i],
        "}"
      ),
      label = paste0("tab:appendix", family_aux_table_ids[i]),
      stat_lines = family_stat_lines,
      note_lines = c(
        "Rows come from separate linear regressions on kids and mother indicators with the full baseline controls, without control fixed effects.",
        cluster_note,
        "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
      )
    )
  )
}

# ----- Appendix Tables A6-A8: Baseline racial-share controls -----
run_race_control_table <- function(outcomes, labels, file, caption, table_label) {
  data_list <- lapply(outcomes, function(outcome) {
    prep_table_data(cleaned_data, c(outcome), extra_vars = race_share_controls)
  })

  models_race <- Map(function(outcome, df) {
    fml <- as.formula(paste(outcome, "~ RACE +", base_covariates_fml, "+", race_share_controls_fml, "| CONTROL | 0 | cluster_group"))
    felm(fml, data = df)
  }, outcomes, data_list)
  models_minority <- Map(function(outcome, df) {
    fml <- as.formula(paste(outcome, "~ ofcolor +", base_covariates_fml, "+", race_share_controls_fml, "| CONTROL | 0 | cluster_group"))
    felm(fml, data = df)
  }, outcomes, data_list)
  n_trials <- sapply(data_list, function(df) length(unique(df$CONTROL)))

  rows <- build_rows(models_minority, models_race, labels)
  write_table(
    file,
    latex_table_multi(
      rows,
      outcome_labels = labels,
      caption = caption,
      label = table_label,
      models_minority = models_minority,
      models_race = models_race,
      n_trials = n_trials
    )
  )
}

current_table <- "Appendix Table A6"
progress_message("Appendix Table A6: running school/safety models with racial-share controls")
run_race_control_table(
  table8a_outcomes,
  table8a_labels,
  file.path(appendix_tables_dir, "tableA6_school_safety_race_controls.tex"),
  "Neighborhood Effects with Linear Racial-Share Controls (School Quality and Safety)\\\\[0.5em]\\textit{Appendix Table A6}",
  "tab:appendixA6"
)

current_table <- "Appendix Table A7"
progress_message("Appendix Table A7: running ACS-neighborhood models with racial-share controls")
run_race_control_table(
  table8b_outcomes,
  table8b_labels,
  file.path(appendix_tables_dir, "tableA7_acs_race_controls.tex"),
  "Neighborhood Effects with Linear Racial-Share Controls (ACS Characteristics)\\\\[0.5em]\\textit{Appendix Table A7}",
  "tab:appendixA7"
)

current_table <- "Appendix Table A8"
progress_message("Appendix Table A8: running pollution models with racial-share controls")
run_race_control_table(
  table9_outcomes,
  table9_labels,
  file.path(appendix_tables_dir, "tableA8_pollution_race_controls.tex"),
  "Neighborhood Effects with Linear Racial-Share Controls (Pollution)\\\\[0.5em]\\textit{Appendix Table A8}",
  "tab:appendixA8"
)

# ----- Appendix Table A9: Median income with racial-share controls -----
current_table <- "Appendix Table A9"
progress_message("Appendix Table A9: running median-income model with racial-share controls")
appendix_a9_data <- prep_table_data(cleaned_data, c("medincome_Rec"), extra_vars = race_share_controls) %>%
  filter(medincome_Rec > 0)
appendix_a9_model <- felm(
  as.formula(paste("log(medincome_Rec) ~ RACE +", base_covariates_fml, "+", race_share_controls_fml, "| CONTROL | 0 | cluster_group")),
  data = appendix_a9_data
)
appendix_a9_rows <- build_race_only_rows(list(appendix_a9_model), c("Log Median Income"))
write_table(
  file.path(appendix_tables_dir, "tableA9_income_race_controls.tex"),
  latex_table_race_only(
    appendix_a9_rows,
    outcome_labels = c("Log Median Income"),
    caption = "Median Income with Linear Racial-Share Controls\\\\[0.5em]\\textit{Appendix Table A9}",
    label = "tab:appendixA9",
    models = list(appendix_a9_model),
    n_trials = length(unique(appendix_a9_data$CONTROL))
  )
)

# ----- Hispanic tester subgroup exploration outputs -----
progress_message("Writing Hispanic tester subgroup diagnostic outputs")
hispanic_appearances <- cleaned_data %>%
  filter(as.character(RACE) == "3") %>%
  distinct(CONTROL, TESTERID, .keep_all = TRUE)
hispanic_unique_testers <- hispanic_appearances %>%
  distinct(TESTERID, .keep_all = TRUE)

summarise_hispanic_var <- function(df, var_name, label_map = NULL, top_n = NULL) {
  out <- df %>%
    count(value = .data[[var_name]], sort = TRUE, name = "n") %>%
    mutate(
      variable = var_name,
      value = if_else(is.na(value), "<NA>", as.character(value)),
      label = if (!is.null(label_map)) unname(label_map[value]) else value
    )
  out$label[is.na(out$label)] <- out$value[is.na(out$label)]
  if (!is.null(top_n)) out <- out %>% slice_head(n = top_n)
  out
}

aprace_labels <- c("1" = "White", "2" = "Black/African-American", "4" = "Asian/Pacific Islander", "5" = "Other (specify)")
tnatorig_labels <- c("-1" = "Missing", "1" = "Non-Hispanic", "2" = "Hispanic")
thispubg_labels <- c(
  "-1" = "Missing",
  "1" = "Mexican",
  "2" = "Cuban",
  "3" = "Dominican",
  "4" = "Puerto Rican",
  "5" = "Central American",
  "6" = "South American",
  "7" = "Spanish",
  "8" = "Other Hispanic"
)

hispanic_proxy_summary <- bind_rows(
  summarise_hispanic_var(hispanic_appearances, "APRACE", aprace_labels) %>% mutate(sample = "analytic_appearances"),
  summarise_hispanic_var(hispanic_appearances, "TNATORIG", tnatorig_labels) %>% mutate(sample = "analytic_appearances"),
  summarise_hispanic_var(hispanic_appearances, "THISPUBG", thispubg_labels) %>% mutate(sample = "analytic_appearances"),
  summarise_hispanic_var(hispanic_unique_testers, "APRACE", aprace_labels) %>% mutate(sample = "unique_testers"),
  summarise_hispanic_var(hispanic_unique_testers, "TNATORIG", tnatorig_labels) %>% mutate(sample = "unique_testers"),
  summarise_hispanic_var(hispanic_unique_testers, "THISPUBG", thispubg_labels) %>% mutate(sample = "unique_testers")
) %>%
  select(sample, variable, value, label, n)
write.csv(
  hispanic_proxy_summary,
  file.path(appendix_tables_dir, "hispanic_tester_proxy_summary.csv"),
  row.names = FALSE
)

hispanic_text_fields <- bind_rows(
  summarise_hispanic_var(hispanic_appearances %>% filter(!is.na(TRACESPY), trimws(TRACESPY) != ""), "TRACESPY", top_n = 20),
  summarise_hispanic_var(hispanic_appearances %>% filter(!is.na(TCORGI2), trimws(TCORGI2) != ""), "TCORGI2", top_n = 20)
) %>%
  mutate(sample = "analytic_appearances") %>%
  select(sample, variable, value, label, n)
write.csv(
  hispanic_text_fields,
  file.path(appendix_tables_dir, "hispanic_tester_text_fields.csv"),
  row.names = FALSE
)

aprace_appearance_counts <- hispanic_proxy_summary %>%
  filter(sample == "analytic_appearances", variable == "APRACE") %>%
  select(value, n)
aprace_white <- aprace_appearance_counts$n[aprace_appearance_counts$value == "1"]
aprace_other <- aprace_appearance_counts$n[aprace_appearance_counts$value == "5"]
if (length(aprace_white) == 0) aprace_white <- 0
if (length(aprace_other) == 0) aprace_other <- 0

explicit_nonwhite_text <- hispanic_appearances %>%
  filter(!is.na(TRACESPY)) %>%
  mutate(text_lc = tolower(TRACESPY)) %>%
  filter(grepl("black|afro|mestizo|indigenous|native", text_lc)) %>%
  count(TRACESPY, sort = TRUE, name = "n")

hispanic_note_lines <- c(
  sprintf("Analytic Hispanic tester appearances: %d; unique Hispanic testers: %d.", nrow(hispanic_appearances), nrow(hispanic_unique_testers)),
  sprintf("APRACE supports a coarse split inside the Hispanic category: %d white-coded appearances and %d other-coded appearances.", aprace_white, aprace_other),
  "TNATORIG is not useful within the Hispanic analytic sample because it is almost entirely coded Hispanic.",
  "THISPUBG provides Hispanic subgroup labels (for example Mexican, Puerto Rican, Central American, South American) but not race within Hispanic identity.",
  "TRACESPY and TCORGI2 are exploratory text fields: they contain some explicit entries such as Black Hispanic and Indigenous-Mexican, but most values are generic Hispanic/Latino or birthplace descriptions."
)
if (nrow(explicit_nonwhite_text) > 0) {
  hispanic_note_lines <- c(
    hispanic_note_lines,
    paste0(
      "Explicit non-white text examples in TRACESPY were sparse but present: ",
      paste(head(sprintf("%s (%d)", explicit_nonwhite_text$TRACESPY, explicit_nonwhite_text$n), 5), collapse = "; "),
      "."
    )
  )
}
writeLines(hispanic_note_lines, file.path(appendix_tables_dir, "hispanic_tester_proxy_note.txt"))

# ----- Market heterogeneity outputs (site-level summary plus Atlanta/Chicago highlight table) -----
# These site-specific regressions have only one market by construction, so market
# clustering is undefined. Keep them only in the standard trial-clustered run.
if (cluster_level == "trial") {
  progress_message("Writing market heterogeneity diagnostic outputs")
  site_market_lookup <- cleaned_data %>%
    mutate(site = substr(as.character(CONTROL), 1, 2)) %>%
    group_by(site, HSTATE, HCITY) %>%
    summarise(rows = n(), .groups = "drop") %>%
    group_by(site) %>%
    slice_max(rows, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(dominant_state = HSTATE, dominant_city = HCITY) %>%
    left_join(
      cleaned_data %>%
        distinct(CONTROL, site) %>%
        count(site, name = "controls"),
      by = "site"
    ) %>%
    arrange(site)
  write.csv(site_market_lookup, file.path(appendix_tables_dir, "site_market_lookup.csv"), row.names = FALSE)

  current_table <- "Appendix Table A10"
  progress_message("Appendix Table A10: running site-specific racial-composition models")
  market_sites <- site_market_lookup$site
  market_outcomes <- appendix_a1_outcomes
  market_labels <- appendix_a1_labels

  site_outcome_data <- lapply(market_sites, function(site_code) {
    race_df <- cleaned_data %>% filter(site == site_code)
    lapply(market_outcomes, function(outcome) prep_table_data(race_df, c(outcome)))
  })
  names(site_outcome_data) <- market_sites

  # In a single-market regression, some retained baseline factors can have only
  # one observed level and are therefore not estimable. Drop only those controls
  # in this exploratory site-specific appendix output.
  usable_site_covariates <- function(df, controls) {
    controls[vapply(controls, function(control) {
      vals <- df[[control]]
      vals <- vals[!is.na(vals)]
      length(unique(vals)) > 1
    }, logical(1))]
  }

  site_outcome_models <- lapply(market_sites, function(site_code) {
    Map(function(outcome, df) {
      site_rhs <- paste(c("RACE", usable_site_covariates(df, base_covariates)), collapse = " + ")
      felm(as.formula(paste(outcome, "~", site_rhs, "| CONTROL | 0 | cluster_group")), data = df)
    }, market_outcomes, site_outcome_data[[site_code]])
  })
  names(site_outcome_models) <- market_sites

  market_coef_summary <- do.call(rbind, lapply(market_sites, function(site_code) {
    site_models <- site_outcome_models[[site_code]]
    site_data <- site_outcome_data[[site_code]]
    dominant_row <- site_market_lookup %>% filter(site == site_code)
    do.call(rbind, lapply(seq_along(site_models), function(j) {
      outcome_name <- market_outcomes[j]
      outcome_label <- market_labels[j]
      do.call(rbind, lapply(
        c("RACE2", "RACE3", "RACE4"),
        function(coef_name) {
          info <- extract_coef_numeric(site_models[[j]], coef_name)
          if (is.null(info)) return(NULL)
          data.frame(
            site = site_code,
            dominant_city = dominant_row$dominant_city,
            dominant_state = dominant_row$dominant_state,
            outcome = outcome_name,
            outcome_label = outcome_label,
            coefficient = coef_name,
            coefficient_label = c(RACE2 = "African American", RACE3 = "Hispanic", RACE4 = "Asian")[coef_name],
            estimate = info$estimate,
            se = info$se,
            ci_lo = info$ci_lo,
            ci_hi = info$ci_hi,
            p = info$p,
            observations = site_models[[j]]$N,
            trials = length(unique(site_data[[j]]$CONTROL))
          )
        }
      ))
    }))
  }))
  write.csv(
    market_coef_summary,
    file.path(appendix_tables_dir, "market_racial_share_heterogeneity.csv"),
    row.names = FALSE
  )

  atl_models <- site_outcome_models[["AT"]]
  chi_models <- site_outcome_models[["CH"]]
  appendix_a10_rows <- build_model_group_rows(
    list(appendix_a1_models_race, atl_models, chi_models),
    outcome_labels = market_labels,
    coef_names = "RACE2",
    group_labels = c("Full Sample", "Atlanta (AT)", "Chicago (CH)")
  )
  write_table(
    file.path(appendix_tables_dir, "tableA10_black_market_highlights.tex"),
    latex_table_custom(
      appendix_a10_rows,
      outcome_labels = market_labels,
      caption = "Market Heterogeneity in African American Steering to Neighborhood Racial Composition\\\\[0.5em]\\textit{Appendix Table A10}",
      label = "tab:appendixA10",
      stat_lines = NULL,
      note_lines = c(
        "Rows report the African American coefficient from separate site-specific reconstructed-sample regressions with the full baseline controls and control fixed effects; controls with no within-site variation are omitted.",
        "A full site-by-race-by-outcome coefficient file is saved to Appendix\\_Tables/market\\_racial\\_share\\_heterogeneity.csv; site labels are summarized in Appendix\\_Tables/site\\_market\\_lookup.csv.",
        cluster_note,
        "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
      )
    )
  )
}

if (length(rank_warn_log) > 0) {
  progress_message("Writing rank-deficiency warning logs")
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
  warn_tables <- vapply(rank_warn_log, `[[`, "", "table")
  warn_msgs <- vapply(rank_warn_log, `[[`, "", "message")
  warn_lines <- sprintf("%s: %s", warn_tables, warn_msgs)
  writeLines(warn_lines, file.path(tables_dir, "rank_deficient_warnings.txt"))
  warn_summary <- sort(table(warn_tables), decreasing = TRUE)
  summary_lines <- sprintf("%s: %d", names(warn_summary), as.integer(warn_summary))
  writeLines(summary_lines, file.path(tables_dir, "rank_deficient_summary.txt"))
}


###########################################################################
### APPENDIX REGRESSIONS (DISABLED)
###########################################################################

if (FALSE) {
  recommended_first <- felm(STOTUNIT_FIRST ~ RACE + was_first_visitor + am_indicator_first +
                           TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                           CONTROL, data = filtered_data)

  available_first <- felm(SAVLBAD_FIRST ~ RACE + was_first_visitor + am_indicator_first +
                           TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                           CONTROL, data = filtered_data)

  summary(recommended_first)
  summary(available_first)

  # Analysis of likelihood of getting invited back for a second appointment by race
  appointment_counts <- appointments_data %>%
      group_by(CONTROL, TESTERID) %>%
      summarise(n_appointments = n(), .groups = 'drop') %>%
      left_join(appointments_data %>% 
                select(CONTROL, TESTERID, RACE, TPEGAI, THHEGAI, TSEX, age, THIGHEDU) %>%
                distinct(), 
                by = c("CONTROL", "TESTERID"))

  appointment_counts$got_second_appointment <- as.numeric(appointment_counts$n_appointments >= 2)

  callback_regression <- felm(got_second_appointment ~ RACE + TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                             CONTROL, data = callback_data)

  cat("\n--- Callback Analysis: Likelihood of Second Appointment ---\n")
  summary(callback_regression)
  cat("Observations (callback_regression):", callback_regression$N, "\n")

  callback_summary <- callback_data %>%
      group_by(RACE) %>%
      summarise(
          n_testers = n(),
          got_callback = sum(got_second_appointment),
          callback_rate = mean(got_second_appointment),
          .groups = 'drop'
      )

  cat("\n--- Callback Rates by Race ---\n")
  print(callback_summary)

  white_callback_info <- callback_data %>%
      filter(RACE == 1) %>%
      summarise(
          total_white_testers = n(),
          white_got_callback = sum(got_second_appointment),
          avg_white_callbacks = mean(got_second_appointment)
      )

  cat("\n--- White Testers (RACE=1) Callback Information ---\n")
  cat("Total white testers:", white_callback_info$total_white_testers, "\n")
  cat("Number who got second appointment:", white_callback_info$white_got_callback, "\n")
  cat("Average callback rate for white testers:", white_callback_info$avg_white_callbacks, "\n")
}

progress_message("analysis.R finished")
}, warning = rank_warning_handler)
