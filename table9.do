clear all

do "${CODE}/table_generation_function.do"

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition_1 as the full dataset
qui gen condition_1 = 1

// Generate condition_2 as an indicator for mothers
gen condition_2 = 0
// Set condition_2 to 1 when a participant has kids and sex is female
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

run_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" /// // CONTROL_VARS
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" /// // ABS_VARS
    "sfcount_rec" /// // dependent_var_1
    "sfcount_rec" /// // dependent_var_2
    "sfcount_ad" /// // control_var_1
    "sfcount_ad" /// // control_var_2
    "9" // table_number
