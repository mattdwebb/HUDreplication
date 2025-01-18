clear all

do "${CODE}/table_generation_function.do"

capture program drop generate_condition_var
program define generate_condition_var
    args initial_condition condition_name
    
    // Generate initial condition variable
    gen condition = `initial_condition'
    
    // Tag unique tester-control combinations
    egen tag = tag(testerid control) if condition == 1
    
    // Count tags by control
    egen cnt = total(tag), by(control)
    
    // Generate final condition variable
    gen `condition_name' = (cnt > 1 & condition == 1)
    
    // Clean up
    drop condition tag cnt
end

// TABLE 5

process_data "adsprocessed_JPE.csv" 0

qui gen show = stotunit
qui cap destring show, replace force
qui replace show = . if show < 0

qui gen home_av = savlbad
qui replace home_av = "." if home_av == "NA" | home_av == "-1"
qui cap destring home_av, replace force
qui replace home_av = 0 if home_av > 1 & home_av != .


// Define common control variables
local CONTROL_VARS ""

// Define common absorbed variables
local ABS_VARS "control sequencex month market arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown"

// Generate condition variables
forvalues i = 1/5 {
    gen condition_`i' = 1
}

// Call correct_table function for number of recommendations
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "show show home_av home_av" ///
    " " ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    " " ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"  ///
    " " ///
    " " ///
    "5" ///
    "corrected"

// TABLE 6

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables
forvalues i = 1/5 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS " "

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx market arelate2x sapptamx tsexx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx "

// Run regressions for White neighborhood percentage
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "w2012pc_rec w2012pc_rec w2012pc_rec w2012pc_rec w2012pc_rec" ///
    " " ///
    "w2012pc_ad" ///
    "w2012pc_ad logadprice" ///
    "w2012pc_ad logadprice b2012pc_ad a2012pc_ad hisp2012pc_ad" ///
    "w2012pc_ad logadprice b2012pc_ad a2012pc_ad hisp2012pc_ad povrate_ad" ///
    " " ///
    "6" ///
    "corrected" ///
    " " " " " " " " " " ///
    " "

// TABLE 7

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables
forvalues i = 1/3 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "whitehi_rec whitemi_rec whiteli_rec" ///
    " " ///
	" " ///
	" " ///
	" " ///
	" " ///
    " " ///
    "7" ///
    "corrected"

// TABLE 8A - pt. 1

process_data "HUDprocessed_JPE_testscores_042021.csv" 1

// Generate condition variables
forvalues i = 1/2 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "mn_avg_ol_elem_rec mn_avg_ol_middle_rec" ///
    "mn_avg_ol_elem_ad" ///
	"mn_avg_ol_middle_ad" ///
	" " ///
	" " ///
	" " ///
    " " ///
    "8A1" ///
	"corrected"

// TABLE 8A - pt. 2

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables
forvalues i = 1/2 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions 
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "assault_rec elementary_school_score_rec" ///
    "assault_ad" ///
	"elementary_school_score_ad" ///
	" " ///
	" " ///
	" " ///
    " " ///
    "8A2" ///
	"corrected"

// TABLE 8B

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables
forvalues i = 1/5 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "povrate_rec skill_rec college_rec singlefamily_rec ownerocc_rec" ///
    "povrate_ad" ///
	"skill_ad" ///
	"college_ad" ///
	"singlefamily_ad" ///
	"ownerocc_ad" ///
    " " ///
    "8B" ///
	"corrected"

// TABLE 9A

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables, whole dataset
forvalues i = 1/3 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "sfcount_rec rsei_rec pm25_rec" /// // dependent variable list
    "sfcount_ad" /// 
    "rsei_ad" /// 
    "pm25_ad" /// 
    " " ///
    " " ///
    " " ///
    "9A" /// // table_number
    "corrected" // original or corrected?

// TABLE 9B

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables, selecting only for mothers
forvalues i = 1/3 {
    gen condition_`i' = 0
    replace condition_`i' = 1 if kidsx == 1 & tsexxx == 0
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "sfcount_rec rsei_rec pm25_rec" /// // dependent variable list
    "sfcount_ad" /// 
    "rsei_ad" /// 
    "pm25_ad" /// 
    " " ///
    " " ///
    " " ///
    "9B" /// // table_number
    "corrected" // original or corrected?



// TABLE 10A - pt. 1

process_data "HUDprocessed_JPE_testscores_042021.csv" 0

// Generate condition variables
forvalues i = 1/2 {
    gen condition_`i' = 0
    replace condition_`i' = 1 if kidsx == 1 & tsexxx == 0
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex"

// Run regressions 
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "mn_avg_ol_elem_rec mn_avg_ol_middle_rec" ///
    "mn_avg_ol_elem_ad" ///
	"mn_avg_ol_middle_ad" ///
	" " ///
	" " ///
	" " ///
    " " ///
    "10A1" ///
	"corrected"

// TABLE 10A - pt. 2

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables
forvalues i = 1/2 {
    gen condition_`i' = 0
    replace condition_`i' = 1 if kidsx == 1 & tsexxx == 0
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions 
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "assault_rec elementary_school_score_rec" ///
    "assault_ad" ///
	"elementary_school_score_ad" ///
	" " ///
	" " ///
	" " ///
    " " ///
    "10A2" ///
	"corrected"

// TABLE 10B

process_data "HUDprocessed_JPE_census_042021.csv" 0

// Generate condition variables
forvalues i = 1/5 {
    gen condition_`i' = 0
    replace condition_`i' = 1 if kidsx == 1 & tsexxx == 0
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions 
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "povrate_rec skill_rec college_rec singlefamily_rec ownerocc_rec" ///
    "povrate_ad" ///
	"skill_ad" ///
	"college_ad" ///
	"singlefamily_ad" ///
	"ownerocc_ad" ///
    " " ///
    "10B" ///
	"corrected"

// TABLE 11

process_data "HUDprocessed_JPE_census_042021.csv" 1 "original"


clean_vars "povrate_rec povrate_ad nodad_rec nodad_ad"

gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1

gen low_povrate_high_dad = 0 
replace low_povrate_high_dad = 1 if povrate_rec < 0.1 & nodad_rec <0.5




generate_condition_var "povrate_ad < 0.1" "condition_1"
generate_condition_var "povrate_ad < 0.1 & kidsx == 1" "condition_2"
generate_condition_var "povrate_ad < 0.1 & kidsx == 1 & tsexxx == 0 " "condition_3"
generate_condition_var "povrate_ad < 0.1 & nodad_ad < 0.5" "condition_4"
generate_condition_var "povrate_ad < 0.1 & nodad_ad < 0.5 & kidsx == 1" "condition_5"
generate_condition_var "povrate_ad < 0.1 & nodad_ad < 0.5 & kidsx == 1 & tsexxx == 0 " "condition_6"

// Define common control variables
local CONTROL_VARS "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

// Run regressions 
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "low_povrate low_povrate low_povrate low_povrate_high_dad low_povrate_high_dad low_povrate_high_dad" ///
    " " ///
	" " ///
	" " ///
	" " ///
	" " ///
    " " ///
    "11" ///
	"corrected"


// TABLE 12

process_data "HUDprocessed_JPE_census_042021.csv" 0

clean_vars "medincome_rec"
qui gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
generate_condition_var "kidsx == 1" "condition_2"
generate_condition_var "kidsx == 1 & tsexxx == 0" "condition_3"

// Define common control variables
local CONTROL_VARS "medincome_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

// Run regressions 
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "lnmincome_rec lnmincome_rec lnmincome_rec" ///
    " " ///
	" " ///
	" " ///
	" " ///
	" " ///
    " " ///
    "12" ///
	"corrected"

// TABLE 13 - pt. 1

process_data "HUDprocessed_JPE_testscores_042021.csv" 1 

clean_vars "mn_avg_ol_elem_rec mn_avg_ol_elem_ad mn_avg_ol_middle_rec mn_avg_ol_middle_ad"

gen dif_ed_elem = mn_avg_ol_elem_rec - mn_avg_ol_elem_ad
gen dif_ed_middle4 = mn_avg_ol_middle_rec - mn_avg_ol_middle_ad

// Generate condition variables
forvalues i = 1/2 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "dif_ed_elem dif_ed_middle4" ///
    " " ///
	" " ///
	" " ///
	" " ///
	" " ///
    " " ///
    "13A1" ///
	"corrected"


// TABLE 13 - pt. 2

process_data "HUDprocessed_JPE_census_042021.csv" 0 

clean_vars "assault_rec assault_ad elementary_school_score_rec elementary_school_score_ad"

gen dif_asadrace = assault_rec - assault_ad
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad

// Generate condition variables
forvalues i = 1/2 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "dif_asadrace dif_esadrace" ///
    " " ///
	" " ///
	" " ///
	" " ///
	" " ///
    " " ///
    "13A2" ///
	"corrected"


// TABLE 13B

process_data "HUDprocessed_JPE_census_042021.csv" 0

clean_vars "povrate_rec povrate_ad skill_rec skill_ad college_rec college_ad singlefamily_rec singlefamily_ad ownerocc_rec ownerocc_ad sfcount_rec sfcount_ad rsei_rec rsei_ad pm25_rec pm25_ad"

gen dif_pradrace = povrate_rec - povrate_ad
gen dif_skadrace = skill_rec - skill_ad
gen dif_coladrace = college_rec - college_ad
gen dif_sf4 = singlefamily_rec - singlefamily_ad
gen dif_own4 = ownerocc_rec - ownerocc_ad

// Generate condition variables
forvalues i = 1/5 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex market algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "dif_pradrace dif_skadrace dif_coladrace dif_sf4 dif_own4" ///
    " " ///
    " " ///
    " " ///
    " " ///
    " " ///
    " " ///
    "13B" ///
	"corrected"


// TABLE 14A

process_data "HUDprocessed_JPE_names_042021.csv" 1

// construct indicators for race groups
clean_vars "apracex"
gen whitetester = 0
replace whitetester = 1 if apracex == 1

gen blacktester = 0
replace blacktester = 1 if apracex == 2

gen hisptester = 0
replace hisptester = 1 if apracex == 3

gen asiantester = 0
replace asiantester = 1 if apracex == 4

// construct race of buyer
clean_vars "buyer_pred_race_rec"
gen buyer_white_rec = 0
replace buyer_white_rec = 1 if buyer_pred_race_rec == "white"
replace buyer_white_rec = . if buyer_pred_race_rec == "."

gen buyer_black_rec = 0
replace buyer_black_rec = 1 if buyer_pred_race_rec == "black"
replace buyer_black_rec = . if buyer_pred_race_rec == "."

gen buyer_hisp_rec = 0
replace buyer_hisp_rec = 1 if buyer_pred_race_rec == "hispanic"
replace buyer_hisp_rec = . if buyer_pred_race_rec == "."

gen buyer_asian_rec = 0
replace buyer_asian_rec = 1 if buyer_pred_race_rec == "asian"
replace buyer_asian_rec = . if buyer_pred_race_rec == "."

// Generate condition variables
forvalues i = 1/4 {
    gen condition_`i' = 1
}

// Define common control variables
local CONTROL_VARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"

// Define common absorbed variables
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex algncurx aelng1x dpmtexpx amoversx aleasetpx acarownx"

// Run regressions for White High, Middle, and Low Income neighborhoods
correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "buyer_white_rec buyer_black_rec buyer_hisp_rec buyer_asian_rec " ///
    "whitetester" ///
    "blacktester" ///
    "hisptester" ///
    "asiantester" ///
	" " ///
    " " ///
    "14A" ///
	"corrected" ///
    " " " " " " " " " " " " "override"


// TABLE 14B

process_data "HUDprocessed_JPE_names_042021.csv" 0 

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


// Define the data subset indicator variable for regressions
gen condition_1 = 0 
replace condition_1 = 1 if salespriceamount_rec>10000 & salespriceamount_rec<10000000 & transmid11 == 1
gen condition_2 = condition_1
gen condition_3 = condition_1
gen condition_4 = condition_1
gen condition_5 = condition_1

local CONTROL_VARS "i.transmonth i.transyear "
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"


correct_table "`CONTROL_VARS'" "`ABS_VARS'" ///
    "salespriceamount_rec_log salespriceamount_rec_log salespriceamount_rec_log salespriceamount_rec_log salespriceamount_rec_log" ///
    " " ///
    "w2012pc_ad " ///
    "w2012pc_ad logadprice" ///
    "w2012pc_ad logadprice b2012pc_ad a2012pc_ad hisp2012pc_ad" ///
	" " ///
    " " ///
    "14B" ///
	"corrected"

