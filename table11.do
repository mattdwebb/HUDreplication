clear all

do "${CODE}/table_generation_function.do"

process_data "HUDprocessed_JPE_census_042021.csv" 0

clean_vars "povrate_rec povrate_ad"

gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1

gen condition_1 = 1
gen condition_2 = 1

run_regressions ///
    "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" /// // CONTROL_VARS
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" /// // ABS_VARS
    "low_povrate" /// // dependent_var_1
    "low_povrate" /// // dependent_var_2
    "" /// // control_var_1
    "" /// // control_var_2
    "11" // table_number
