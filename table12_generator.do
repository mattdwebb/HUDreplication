
clear all

do "${CODE}/table_generation_function.do"

process_data "HUDprocessed_JPE_census_042021.rds" 0

gen condition_1 = 1
gen condition_2 = 1

run_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" /// // CONTROL_VARS
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" /// // ABS_VARS
    "lnmincome_rec" /// // dependent_var_1
    "lnmincome_rec" /// // dependent_var_2
    "medincome_ad" /// // control_var_1
    "medincome_ad" /// // control_var_2
    "12" // table_number
