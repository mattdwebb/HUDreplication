clear all

do "${CODE}/table_generation_function.do"

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition_1
gen condition_1 = 1

// Generate condition_2 
gen condition_2 = 1

run_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad" /// // CONTROL_VARS
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" /// // ABS_VARS
    "whitehi_rec" /// // dependent_var_1
    "whiteli_rec" /// // dependent_var_2
    "" /// // control_var_1
    "" /// // control_var_2
    "7" // table_number
