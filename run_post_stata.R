# This script runs all the R scripts required to replicate ``Fewer Homes, Similar Neighborhoods'', after ct_sample/preprocess.R is run 
# to clean and preprocess the data and ct_sample/main.do is run to generate the main results with the original and cleaned C&T sample.
# See the README for more details. 


# Usage: Rscript run_post_stata.R [options]
#
# Options:
#   --repo-root=/path/to/HUDreplication   Explicit repository root.
#   --skip-geocoding                     Reuse the canonical geocoded cache.
#   --live-geocoding                     Rebuild geocodes from live services.
#   --skip-data-cleaning                 Start at reconstructed analysis.
#   --skip-data-description              Do not regenerate data-description tables.
#   --skip-matching-diagnostics          Do not regenerate matching-diagnosis appendix tables.
#   --with-market-heterogeneity          Run standalone market heterogeneity script.
#
# By default, the runner uses --skip-geocoding when the canonical geocoded
# cache exists at Data/Generated/reconstructed_sample/sales_tester_rechomes_geocoded.csv.


# ENTER THE GLOBAL PATH TO THE REPOSITORY ROOT "HUDreplication" DIRECTORY HERE:
# This is used only as a fallback; passing --repo-root=/path/to/HUDReplication
# on the command line overrides it.
repo_root <- "ENTER/REPOSITORY/ROOT/HERE"  # Placeholder: replace with the repository root path.

args <- commandArgs(trailingOnly = TRUE)

has_arg <- function(flag) flag %in% args
get_arg_value <- function(prefix) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) return(NULL)
  sub(prefix, "", hit[1], fixed = TRUE)
}

# Prefer the --repo-root command-line value when supplied; otherwise use the
# manually entered path above.
repo_root_arg <- get_arg_value("--repo-root=")
if (!is.null(repo_root_arg) && nzchar(repo_root_arg)) {
  repo_root <- repo_root_arg
}

setwd(repo_root)
Sys.setenv(HUD_REPLICATION_ROOT = repo_root, REPO_ROOT = repo_root)

skip_geocoding <- has_arg("--skip-geocoding")
live_geocoding <- has_arg("--live-geocoding")
if (skip_geocoding && live_geocoding) {
  stop("Use only one of --skip-geocoding and --live-geocoding.")
}

canonical_geocode_cache <- file.path(
  repo_root,
  "Data",
  "Generated",
  "reconstructed_sample",
  "sales_tester_rechomes_geocoded.csv"
)
use_geocode_cache <- skip_geocoding || (!live_geocoding && file.exists(canonical_geocode_cache))

rscript <- file.path(R.home("bin"), "Rscript")

run_step <- function(label, script, step_args = character()) {
  script_path <- file.path(repo_root, script)
  if (!file.exists(script_path)) {
    stop("Missing script for step '", label, "': ", script_path)
  }

  command <- paste(c(shQuote(rscript), shQuote(script_path), shQuote(step_args)), collapse = " ")
  cat("\n== ", label, " ==\n", command, "\n", sep = "")
  flush.console()

  status <- system(command)
  if (!identical(status, 0L)) {
    stop("Step failed: ", label, call. = FALSE)
  }
  invisible(status)
}

cat("Repository root: ", repo_root, "\n", sep = "")
if (use_geocode_cache) {
  cat("Geocoding mode: reuse canonical cache (--skip-geocoding)\n")
} else {
  cat("Geocoding mode: live geocoding\n")
}

if (!has_arg("--skip-data-cleaning")) {
  data_cleaning_args <- if (use_geocode_cache) "--skip-geocoding" else character()
  run_step(
    "Clean and merge reconstructed-sample data",
    file.path("reconstructed_sample", "data_cleaning.R"),
    data_cleaning_args
  )
}

run_step("Run reconstructed-sample analysis", file.path("reconstructed_sample", "analysis.R"))
run_step("Format reconstructed-sample tables", file.path("reconstructed_sample", "format_tables.R"))
run_step("Format supplemental C&T-sample appendix tables", file.path("ct_sample", "format_supplemental_appendix_tables.R"))
run_step("Generate comparison tables", "make_comparison_tables.R")

if (!has_arg("--skip-data-description")) {
  run_step("Generate data-description and diagnostic tables", "data_description.R")
}

if (!has_arg("--skip-matching-diagnostics")) {
  run_step(
    "Generate reconstructed-sample matching diagnostics",
    file.path("reconstructed_sample", "matching_diagnosis.R"),
    paste0("--repo-root=", repo_root)
  )
}

if (has_arg("--with-market-heterogeneity")) {
  run_step(
    "Generate standalone market heterogeneity outputs",
    file.path("reconstructed_sample", "market_heterogeneity_analysis.R")
  )
}

cat("\nPost-Stata R replication steps completed.\n")
