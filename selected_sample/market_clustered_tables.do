clear all

// Parallel selected-sample run with conventional market-level clustering.
// This leaves the standard Output folder untouched by writing into
// Output/market_clustered/{comparison_tables,corrected_full,original_full}.

if "${REPO_ROOT}" == "" {
    global REPO_ROOT "/PATH/TO/HUDreplication"
}
if "${CODE}" == "" {
    global CODE "${REPO_ROOT}/selected_sample"
}
if "${DATA}" == "" {
    global DATA "${REPO_ROOT}/Data"
}
if "${FORCE_CLEAN}" == "" {
    global FORCE_CLEAN 0
}

local PKG "egenmore strgroup matchit freqindex reghdfe estout ftools"
foreach var in `PKG' {
    cap which `var'
    if _rc != 0 {
        ssc install `var'
    }
}

set more off

local OLD_SS_CLUSTER_VAR "${SELECTED_SAMPLE_CLUSTER_VAR}"
local OLD_SS_CLUSTER_LABEL "${SELECTED_SAMPLE_CLUSTER_LABEL}"
local OLD_SS_CLUSTER_DESC "${SELECTED_SAMPLE_CLUSTER_DESC}"

global SELECTED_SAMPLE_CLUSTER_VAR "market"
global SELECTED_SAMPLE_CLUSTER_LABEL "market"
global SELECTED_SAMPLE_CLUSTER_DESC "market"

cap mkdir "${CODE}/Output"
cap mkdir "${CODE}/Output/market_clustered"
cap mkdir "${CODE}/Output/market_clustered/comparison_tables"
cap mkdir "${CODE}/Output/market_clustered/corrected_full"
cap mkdir "${CODE}/Output/market_clustered/original_full"

// Reuse the current cleaned caches when FORCE_CLEAN=0. This keeps the market-
// clustered analysis focused on inference and avoids rerunning the city-cleaning
// plugin path unnecessarily.
foreach f in ///
    adsprocessed_with_duplicates_hcity_cleaned.dta ///
    adsprocessed_processed_hcity_cleaned.dta ///
    HUDprocessed_census_with_duplicates_hcityx_cleaned.dta ///
    HUDprocessed_census_processed_hcityx_cleaned.dta ///
    HUDprocessed_census_processed_hcity_ad_cleaned.dta ///
    HUDprocessed_testscores_with_duplicates_hcityx_cleaned.dta ///
    HUDprocessed_testscores_processed_hcityx_cleaned.dta ///
    HUDprocessed_testscores_processed_hcity_ad_cleaned.dta ///
    HUDprocessed_names_with_duplicates_hcityx_cleaned.dta ///
    HUDprocessed_names_processed_hcityx_cleaned.dta ///
    HUDprocessed_names_processed_hcity_ad_cleaned.dta {
    cap copy "${CODE}/Output/`f'" "${CODE}/Output/market_clustered/comparison_tables/`f'", replace
}

foreach f in ///
    adsprocessed_processed_hcity_cleaned.dta ///
    HUDprocessed_census_processed_hcity_ad_cleaned.dta ///
    HUDprocessed_testscores_processed_hcity_ad_cleaned.dta ///
    HUDprocessed_names_processed_hcity_ad_cleaned.dta {
    cap copy "${CODE}/Output/`f'" "${CODE}/Output/market_clustered/corrected_full/`f'", replace
}

foreach f in ///
    adsprocessed_with_duplicates_hcity_cleaned.dta ///
    HUDprocessed_census_with_duplicates_hcityx_cleaned.dta ///
    HUDprocessed_testscores_with_duplicates_hcityx_cleaned.dta ///
    HUDprocessed_names_with_duplicates_hcityx_cleaned.dta {
    cap copy "${CODE}/Output/`f'" "${CODE}/Output/market_clustered/original_full/`f'", replace
}

cap log close

global OUTPUT "${CODE}/Output/market_clustered/comparison_tables"
log using "${OUTPUT}/market_clustered_comparison_tables.log", text replace
do "${CODE}/comparison_tables.do"
log close

global OUTPUT "${CODE}/Output/market_clustered"
global APPENDIX_OUTPUT_ROOT "${CODE}/Output/market_clustered"
global APPENDIX_TABLE_MODES "corrected original"
log using "${OUTPUT}/market_clustered_appendix_tables.log", text replace
do "${CODE}/appendix_tables.do"
log close
macro drop APPENDIX_OUTPUT_ROOT
macro drop APPENDIX_TABLE_MODES

global SELECTED_SAMPLE_CLUSTER_VAR "`OLD_SS_CLUSTER_VAR'"
global SELECTED_SAMPLE_CLUSTER_LABEL "`OLD_SS_CLUSTER_LABEL'"
global SELECTED_SAMPLE_CLUSTER_DESC "`OLD_SS_CLUSTER_DESC'"
