// This file generates the six-column comparison tables for the C&T-sample replication
// using the cleaned `_processed` and `_with_duplicates` generated inputs.

clear all

set more off
if "${FORCE_CLEAN}" == "" {
    global FORCE_CLEAN 0
}

do "${CODE}/table_generation_function.do"

local ADS_PROCESSED "adsprocessed_correct_cities_processed.csv"
local ADS_WITH_DUPLICATES "adsprocessed_correct_cities_with_duplicates.csv"
local HUD_CENSUS_PROCESSED "HUDprocessed_census_correct_cities_processed.csv"
local HUD_CENSUS_WITH_DUPLICATES "HUDprocessed_census_correct_cities_with_duplicates.csv"
local HUD_TESTSCORES_PROCESSED "HUDprocessed_testscores_correct_cities_processed.csv"
local HUD_TESTSCORES_WITH_DUPLICATES "HUDprocessed_testscores_correct_cities_with_duplicates.csv"
local HUD_NAMES_PROCESSED "HUDprocessed_names_correct_cities_processed.csv"
local HUD_NAMES_WITH_DUPLICATES "HUDprocessed_names_correct_cities_with_duplicates.csv"

local FORCE_CLEAN_LOCAL "${FORCE_CLEAN}"

tempfile data_col1 data_col23 data_col456

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 5 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`ADS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcity" "adsprocessed_with_duplicates_hcity"

gen show = stotunit
gen home_av = savlbad
capture confirm string variable home_av
if !_rc {
    replace home_av = "." if home_av == "NA" | home_av == "-1"
}
else {
    replace home_av = . if home_av == -1
}
clean_vars "show home_av"
replace show = . if show < 0
replace home_av = 0 if home_av > 1 & home_av != .

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`ADS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity" "adsprocessed_processed_hcity"

gen show = stotunit
gen home_av = savlbad
capture confirm string variable home_av
if !_rc {
    replace home_av = "." if home_av == "NA" | home_av == "-1"
}
else {
    replace home_av = . if home_av == -1
}
clean_vars "show home_av"
replace show = . if show < 0
replace home_av = 0 if home_av > 1 & home_av != .

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace
save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex month market arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown" ///
    "show" ///
    "home_av" ///
    "" ///
    "" ///
    "5" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 6 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "logadprice w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad povrate_ad" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "w2012pc_rec" ///
    "w2012pc_rec" ///
    "" ///
    "" ///
    "6" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 7 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "whitehi_rec" ///
    "whiteli_rec" ///
    "" ///
    "" ///
    "7" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 8 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "elementary_school_score_rec" ///
    "povrate_rec" ///
    "elementary_school_score_ad" ///
    "povrate_ad" ///
    "8" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 9 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

gen condition_1 = 1
gen condition_2 = 0
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

gen condition_1 = 1
gen condition_2 = 0
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

gen condition_1 = 1
gen condition_2 = 0
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "sfcount_rec" ///
    "sfcount_rec" ///
    "sfcount_ad" ///
    "sfcount_ad" ///
    "9" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 10A ============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_TESTSCORES_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_testscores_with_duplicates_hcityx"

capture confirm variable kidsx
if !_rc {
    capture confirm variable tsexxx
    if !_rc {
        keep if kidsx == 1 & tsexxx == 0
    }
    else {
        keep if kidsx == 1
    }
}

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_TESTSCORES_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_testscores_processed_hcityx"

capture confirm variable kidsx
if !_rc {
    capture confirm variable tsexxx
    if !_rc {
        keep if kidsx == 1 & tsexxx == 0
    }
    else {
        keep if kidsx == 1
    }
}

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_TESTSCORES_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_testscores_processed_hcity_ad"

capture confirm variable kidsx
if !_rc {
    capture confirm variable tsexxx
    if !_rc {
        keep if kidsx == 1 & tsexxx == 0
    }
    else {
        keep if kidsx == 1
    }
}

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex" ///
    "mn_avg_ol_elem_rec" ///
    "mn_avg_ol_middle_rec" ///
    "mn_avg_ol_elem_ad" ///
    "mn_avg_ol_middle_ad" ///
    "10A" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 10B ============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

keep if kidsx == 1 & tsexxx == 0

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

keep if kidsx == 1 & tsexxx == 0

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

keep if kidsx == 1 & tsexxx == 0

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "skill_rec" ///
    "singlefamily_rec" ///
    "skill_ad" ///
    "singlefamily_ad" ///
    "10B" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 11 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

clean_vars "povrate_rec povrate_ad"
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1
drop tag cnt

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

clean_vars "povrate_rec povrate_ad"
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1
drop tag cnt

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

clean_vars "povrate_rec povrate_ad"
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1
drop tag cnt

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "low_povrate" ///
    "low_povrate" ///
    "" ///
    "" ///
    "11" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 12 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

clean_vars "medincome_rec"
gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

clean_vars "medincome_rec"
gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

clean_vars "medincome_rec"
gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "lnmincome_rec" ///
    "lnmincome_rec" ///
    "medincome_ad" ///
    "medincome_ad" ///
    "12" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 13 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_CENSUS_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_with_duplicates_hcityx"

clean_vars "elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec"
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

gen condition_1 = 1
gen condition_2 = 1

save "`data_col1'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_census_processed_hcityx"

clean_vars "elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec"
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

gen condition_1 = 1
gen condition_2 = 1

save "`data_col23'", replace

process_data "`HUD_CENSUS_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_census_processed_hcity_ad"

clean_vars "elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec"
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

gen condition_1 = 1
gen condition_2 = 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "dif_esadrace" ///
    "dif_skadrace" ///
    "" ///
    "" ///
    "13" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 14 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

process_data "`HUD_NAMES_WITH_DUPLICATES'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_names_with_duplicates_hcityx"

gen recordingdate_rec_date = date(recordingdate_rec, "YMD")
gen transmonth = month(recordingdate_rec_date)
label define monthlbl 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"
label values transmonth monthlbl

gen transyear = year(recordingdate_rec_date)

gen transmid11 = 0
replace transmid11 = 1 if recordingdate_rec_date > mdy(1, 6, 2011)
replace transmid11 = . if missing(recordingdate_rec_date)

clean_vars "salespriceamount_rec"
gen salespriceamount_rec_log = log(salespriceamount_rec)

replace hcity = "missing" if hcity == ""
replace temp_city = "missing" if temp_city == ""

gen condition_1 = 0
replace condition_1 = 1 if salespriceamount_rec > 10000 & salespriceamount_rec < 10000000 & transmid11 == 1
gen condition_2 = 0
replace condition_2 = 1 if salespriceamount_rec > 10000 & salespriceamount_rec < 10000000 & transmid11 == 1

save "`data_col1'", replace

process_data "`HUD_NAMES_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcityx" "HUDprocessed_names_processed_hcityx"

gen recordingdate_rec_date = date(recordingdate_rec, "YMD")
gen transmonth = month(recordingdate_rec_date)
label define monthlbl 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"
label values transmonth monthlbl

gen transyear = year(recordingdate_rec_date)

gen transmid11 = 0
replace transmid11 = 1 if recordingdate_rec_date > mdy(1, 6, 2011)
replace transmid11 = . if missing(recordingdate_rec_date)

clean_vars "salespriceamount_rec"
gen salespriceamount_rec_log = log(salespriceamount_rec)

replace hcity = "missing" if hcity == ""
replace temp_city = "missing" if temp_city == ""

gen condition_1 = 0
replace condition_1 = 1 if salespriceamount_rec > 10000 & salespriceamount_rec < 10000000 & transmid11 == 1
gen condition_2 = 0
replace condition_2 = 1 if salespriceamount_rec > 10000 & salespriceamount_rec < 10000000 & transmid11 == 1

save "`data_col23'", replace

process_data "`HUD_NAMES_PROCESSED'" "`FORCE_CLEAN_LOCAL'" "" "hcity_ad" "HUDprocessed_names_processed_hcity_ad"

gen recordingdate_rec_date = date(recordingdate_rec, "YMD")
gen transmonth = month(recordingdate_rec_date)
label define monthlbl 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"
label values transmonth monthlbl

gen transyear = year(recordingdate_rec_date)

gen transmid11 = 0
replace transmid11 = 1 if recordingdate_rec_date > mdy(1, 6, 2011)
replace transmid11 = . if missing(recordingdate_rec_date)

clean_vars "salespriceamount_rec"
gen salespriceamount_rec_log = log(salespriceamount_rec)

replace hcity = "missing" if hcity == ""
replace temp_city = "missing" if temp_city == ""

gen condition_1 = 0
replace condition_1 = 1 if salespriceamount_rec > 10000 & salespriceamount_rec < 10000000 & transmid11 == 1
gen condition_2 = 0
replace condition_2 = 1 if salespriceamount_rec > 10000 & salespriceamount_rec < 10000000 & transmid11 == 1

save "`data_col456'", replace

run_comparison_regressions ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad i.transmonth i.transyear logadprice" ///
    "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "salespriceamount_rec_log" ///
    "salespriceamount_rec_log" ///
    "" ///
    "" ///
    "14" ///
    "`data_col1'" ///
    "`data_col23'" ///
    "`data_col456'"
