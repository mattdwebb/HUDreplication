clear all

do "${CODE}/table_generation_function.do"

process_data "HUDprocessed_JPE_names_042021.csv" 0

display "Data processing complete"

/*-------------------------------------*/
/*--- Data processing for Table 14 ----*/
/*-------------------------------------*/

// Convert RecordingDate_Rec to month and year
gen recordingdate_rec_date = date(recordingdate_rec, "YMD")
gen transmonth = month(recordingdate_rec_date)
label define monthlbl 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"
label values transmonth monthlbl

gen transyear = year(recordingdate_rec_date)
summarize transyear

// Create transmid11 variable
gen transmid11 = 0
replace transmid11 = 1 if recordingdate_rec_date > mdy(1, 6, 2011)
replace transmid11 = . if missing(recordingdate_rec_date)

//Create log of salespriceamount_rec

clean_vars "salespriceamount_rec"

gen salespriceamount_rec_log = log(salespriceamount_rec)

// In original R analysis, missing values of hcity were treated as their own category, to allow this in Stata, we set missing values to the string "missing"
replace hcity = "missing" if hcity == ""
replace temp_city = "missing" if temp_city == ""

// Define the data subset indicator variable for regressions 1 and 2 (corresponding to dependent_var_1 and 2 above)
gen condition_1 = 0 
replace condition_1 = 1 if salespriceamount_rec>10000 & salespriceamount_rec<10000000 & transmid11 == 1
gen condition_2 = 0 
replace condition_2 = 1 if salespriceamount_rec>10000 & salespriceamount_rec<10000000 & transmid11 == 1


// Example call to the program
// run_regressions "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad i.transmonth i.transyear logadprice" "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" "salespriceamount_rec_log" "salespriceamount_rec_log" "" "" "14"

run_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad i.transmonth i.transyear logadprice" /// // CONTROL_VARS
    "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" /// // ABS_VARS
    "salespriceamount_rec_log" /// // dependent_var_1
    "salespriceamount_rec_log" /// // dependent_var_2
    "" /// // control_var_1
    "" /// // control_var_2
    "14" // table_number
	
	