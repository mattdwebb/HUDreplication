# Market-level all-completed-pairs heterogeneity for key outcomes.
# This script is standalone and is also sourced by all_completed_pairs/analysis.R.

library(lfe)
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
while (!dir.exists(file.path(repo_root, "all_completed_pairs")) && repo_root != dirname(repo_root)) {
  repo_root <- dirname(repo_root)
}
setwd(repo_root)

data_dir <- file.path(repo_root, "Data")
all_completed_pairs_generated_dir <- file.path(data_dir, "Generated", "all_completed_pairs")
market_output_dir <- file.path(repo_root, "all_completed_pairs", "Market_Heterogeneity")
dir.create(market_output_dir, showWarnings = FALSE, recursive = TRUE)

target_races <- data.frame(
  race_value = c("2", "3", "4"),
  race = c("African American", "Hispanic", "Asian"),
  stringsAsFactors = FALSE
)

race_colors <- c(
  "African American" = "#1b4d89",
  "Hispanic" = "#b6531f",
  "Asian" = "#2d7d46"
)

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

restrict_to_official_pass <- function(df) {
  semi_join(df, official_pass_controls, by = "CONTROL")
}

# Reused for both outcomes and all market-race cells. Each model compares White
# testers only with one target race inside valid two-tester completed pairs.
estimate_race_pair_model <- function(data, outcome_name, outcome_label,
                                     race_value, race_label, site_code, market_label) {
  matched_controls <- data %>%
    filter(as.character(RACE) %in% c("1", race_value)) %>%
    group_by(CONTROL) %>%
    summarise(
      n_testers = n_distinct(TESTERID),
      has_white = any(as.character(RACE) == "1"),
      has_target = any(as.character(RACE) == race_value),
      .groups = "drop"
    ) %>%
    filter(n_testers == 2, has_white, has_target) %>%
    select(CONTROL)

  model_data <- data %>%
    semi_join(matched_controls, by = "CONTROL") %>%
    mutate(target_race = as.integer(as.character(RACE) == race_value))

  if (n_distinct(model_data$CONTROL) < 2 || n_distinct(model_data$target_race) < 2) {
    return(NULL)
  }

  # Market-race cells are small, so the figure uses the design-based all-completed-pairs
  # specification: trial fixed effects and clustered standard errors, without
  # tester controls that can overfit within-market cells. The input samples still
  # impose the same nonmissing covariate restrictions as Tables 5 and 6.
  model_formula <- as.formula(paste(
    outcome_name,
    "~ target_race | CONTROL | 0 | CONTROL"
  ))

  model <- tryCatch(
    suppressWarnings(felm(model_formula, data = model_data)),
    error = function(e) NULL
  )
  if (is.null(model)) return(NULL)

  model_summary <- tryCatch(
    suppressWarnings(coef(summary(model))),
    error = function(e) NULL
  )
  if (is.null(model_summary) || !"target_race" %in% rownames(model_summary)) return(NULL)

  se_col <- if ("Cluster s.e." %in% colnames(model_summary)) "Cluster s.e." else "Std. Error"
  p_col <- grep("^Pr\\(", colnames(model_summary), value = TRUE)
  if (length(p_col) == 0) p_col <- "Pr(>|t|)"

  estimate <- as.numeric(model_summary["target_race", "Estimate"])
  se <- as.numeric(model_summary["target_race", se_col])
  p <- suppressWarnings(as.numeric(model_summary["target_race", p_col[1]]))
  tcrit <- qt(0.975, df = model$df.residual)

  if (!is.finite(estimate) || !is.finite(se) || !is.finite(tcrit)) return(NULL)

  data.frame(
    model_spec = "design_based_all_completed_pairs",
    outcome = outcome_name,
    outcome_label = outcome_label,
    site = site_code,
    market_label = market_label,
    race_value = race_value,
    race = race_label,
    estimate = estimate,
    se = se,
    ci_lo = estimate - tcrit * se,
    ci_hi = estimate + tcrit * se,
    p = p,
    observations = model$N,
    pair_trials = n_distinct(model_data$CONTROL),
    race_observations = sum(as.character(model_data$RACE) == race_value, na.rm = TRUE),
    white_observations = sum(as.character(model_data$RACE) == "1", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

estimate_market_models <- function(data, outcome_name, outcome_label) {
  overall_estimates <- bind_rows(lapply(seq_len(nrow(target_races)), function(i) {
    estimate_race_pair_model(
      data,
      outcome_name,
      outcome_label,
      target_races$race_value[i],
      target_races$race[i],
      "ALL",
      "All markets"
    )
  }))

  market_estimates <- bind_rows(lapply(sort(unique(data$site)), function(site_code) {
    site_data <- data %>% filter(site == site_code)
    market_label <- unique(site_data$market_label)
    bind_rows(lapply(seq_len(nrow(target_races)), function(i) {
      estimate_race_pair_model(
        site_data,
        outcome_name,
        outcome_label,
        target_races$race_value[i],
        target_races$race[i],
        site_code,
        market_label[1]
      )
    }))
  }))

  list(overall = overall_estimates, market = market_estimates)
}

plot_market_estimates <- function(market_estimates, overall_estimates, outcome_name, outcome_label) {
  plot_data <- market_estimates %>%
    filter(!is.na(estimate), !is.na(ci_lo), !is.na(ci_hi))

  overall_plot <- overall_estimates %>%
    filter(!is.na(estimate), !is.na(ci_lo), !is.na(ci_hi))

  market_order <- plot_data %>%
    group_by(market_label) %>%
    summarise(order_stat = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
    arrange(order_stat) %>%
    pull(market_label)

  race_offsets <- c("African American" = -0.23, "Hispanic" = 0, "Asian" = 0.23)
  plot_data <- plot_data %>%
    mutate(
      market_y = as.numeric(factor(market_label, levels = market_order)) + race_offsets[race],
      market_label = factor(market_label, levels = market_order)
    )

  x_range <- range(c(plot_data$ci_lo, plot_data$ci_hi, overall_plot$ci_lo, overall_plot$ci_hi), na.rm = TRUE)
  x_pad <- diff(x_range) * 0.08
  if (!is.finite(x_pad) || x_pad == 0) x_pad <- 0.1

  p <- ggplot(plot_data, aes(x = estimate, y = market_y, color = race)) +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey35") +
    geom_vline(data = overall_plot, aes(xintercept = estimate, color = race), linetype = "dashed", linewidth = 0.45) +
    geom_vline(data = overall_plot, aes(xintercept = ci_lo, color = race), linetype = "dotted", linewidth = 0.35) +
    geom_vline(data = overall_plot, aes(xintercept = ci_hi, color = race), linetype = "dotted", linewidth = 0.35) +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0, linewidth = 0.35, alpha = 0.65) +
    geom_point(size = 1.6) +
    scale_color_manual(values = race_colors) +
    scale_y_continuous(
      breaks = seq_along(market_order),
      labels = market_order,
      expand = expansion(add = 0.65)
    ) +
    coord_cartesian(xlim = c(x_range[1] - x_pad, x_range[2] + x_pad)) +
    labs(
      title = paste("Market-Level Matched-Pair Estimates:", outcome_label),
      subtitle = "Dashed/dotted vertical lines show the race-specific overall estimate and 95% CI.",
      x = "Estimated difference relative to White tester",
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "grey20"),
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey25"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )

  safe_name <- gsub("[^A-Za-z0-9]+", "_", tolower(outcome_name))
  ggsave(file.path(market_output_dir, paste0("market_heterogeneity_", safe_name, ".pdf")), p, width = 10, height = 8, bg = "white")
  ggsave(file.path(market_output_dir, paste0("market_heterogeneity_", safe_name, ".png")), p, width = 10, height = 8, dpi = 300, bg = "white")
  p
}

site_lookup_source <- read.csv(file.path(all_completed_pairs_generated_dir, "cleaned_hds.csv")) %>%
  restrict_to_official_pass() %>%
  mutate(site = substr(as.character(CONTROL), 1, 2))

site_market_lookup <- site_lookup_source %>%
  group_by(site, HSTATE, HCITY) %>%
  summarise(rows = n(), .groups = "drop") %>%
  group_by(site) %>%
  slice_max(rows, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    dominant_city = ifelse(is.na(HCITY) | HCITY == "", site, HCITY),
    dominant_state = ifelse(is.na(HSTATE) | HSTATE == "", "", HSTATE),
    market_label = paste0(site, " - ", dominant_city, ifelse(dominant_state == "", "", paste0(", ", dominant_state)))
  ) %>%
  select(site, dominant_city, dominant_state, market_label)

# Table 5 outcome: total number of recommendations.
table5_data <- read.csv(file.path(all_completed_pairs_generated_dir, "sales_and_tester_merged.csv")) %>%
  restrict_to_official_pass() %>%
  filter(
    !is.na(STOTUNIT_TOTAL),
    !is.na(RACE),
    !is.na(was_first_visitor),
    !is.na(am_indicator_first),
    !is.na(TPEGAI),
    !is.na(THHEGAI),
    !is.na(TSEX),
    !is.na(age),
    !is.na(THIGHEDU)
  ) %>%
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%
  ungroup() %>%
  mutate(
    site = substr(as.character(CONTROL), 1, 2),
    RACE = factor(RACE, levels = c(1, 2, 3, 4)),
    CONTROL = as.factor(CONTROL),
    was_first_visitor = as.factor(was_first_visitor),
    am_indicator_first = as.factor(am_indicator_first),
    TPEGAI = as.factor(TPEGAI),
    THHEGAI = as.factor(THHEGAI),
    TSEX = as.factor(TSEX),
    age = as.numeric(age),
    THIGHEDU = as.factor(THIGHEDU)
  ) %>%
  left_join(site_market_lookup, by = "site")

# Table 6 outcome: white household share of recommended homes.
table6_data <- read.csv(file.path(all_completed_pairs_generated_dir, "cleaned_hds.csv")) %>%
  restrict_to_official_pass() %>%
  filter(
    !is.na(w2012pc_Rec),
    !is.na(RACE),
    !is.na(was_first_visitor),
    !is.na(am_indicator_first),
    !is.na(TSEX),
    !is.na(THHEGAI),
    !is.na(TPEGAI),
    !is.na(THIGHEDU),
    !is.na(TCURTENR),
    !is.na(age)
  ) %>%
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%
  ungroup() %>%
  mutate(
    site = substr(as.character(CONTROL), 1, 2),
    RACE = factor(RACE, levels = c(1, 2, 3, 4)),
    CONTROL = as.factor(CONTROL),
    was_first_visitor = as.factor(was_first_visitor),
    am_indicator_first = as.factor(am_indicator_first),
    TSEX = as.factor(TSEX),
    THHEGAI = as.factor(THHEGAI),
    TPEGAI = as.factor(TPEGAI),
    THIGHEDU = as.factor(THIGHEDU),
    TCURTENR = as.factor(TCURTENR),
    age = as.numeric(age)
  ) %>%
  left_join(site_market_lookup, by = "site")

table5_results <- estimate_market_models(
  table5_data,
  "STOTUNIT_TOTAL",
  "Total Number of Recommendations"
)

table6_results <- estimate_market_models(
  table6_data,
  "w2012pc_Rec",
  "White Household Share"
)

market_estimates <- bind_rows(table5_results$market, table6_results$market)
overall_estimates <- bind_rows(table5_results$overall, table6_results$overall)

market_counts <- bind_rows(
  table5_data %>% count(outcome = "STOTUNIT_TOTAL", site, market_label, RACE, name = "observations"),
  table6_data %>% count(outcome = "w2012pc_Rec", site, market_label, RACE, name = "observations")
) %>%
  mutate(race = recode(as.character(RACE), "1" = "White", "2" = "African American", "3" = "Hispanic", "4" = "Asian")) %>%
  select(-RACE)

write.csv(site_market_lookup, file.path(market_output_dir, "site_market_lookup.csv"), row.names = FALSE)
write.csv(market_counts, file.path(market_output_dir, "market_race_counts.csv"), row.names = FALSE)
write.csv(overall_estimates, file.path(market_output_dir, "overall_all_completed_pairs_estimates.csv"), row.names = FALSE)
write.csv(market_estimates, file.path(market_output_dir, "market_all_completed_pairs_estimates.csv"), row.names = FALSE)

plot_market_estimates(table5_results$market, table5_results$overall, "STOTUNIT_TOTAL", "Total Number of Recommendations")
plot_market_estimates(table6_results$market, table6_results$overall, "w2012pc_Rec", "White Household Share")

cat("Market heterogeneity estimates written to:", market_output_dir, "\n")
cat("Market-level estimate rows:", nrow(market_estimates), "\n")
cat("Overall race-specific estimates written to:", file.path(market_output_dir, "overall_all_completed_pairs_estimates.csv"), "\n")
