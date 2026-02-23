// This file generates the six-column comparison table for HUD replication
// Written by Shi Chen and Anthony McCanny
// Feb 22, 2026


/* Unified comparison tables driver */
/* Uses table_generation_function.do to generate tables with consistent columns */


/*

	===== NOTE TO USER =======================================================
	
	[1] ===== data ===========================================================
	 | two versions of datasets required.
	 | the first version can be obtained by running the pre-processing 
	 | R script, without envoking the de-duplication programs. save them
	 | to the path exactly as:
	 |	./Data/Generated/nodedup/
	 | the second set can be obtained by running the pre-processing file
	 | without any adjustments. outputs will be automatically saved to
	 |	./Data/Generated/
	==========================================================================

	[2] ===== Stata program overview =========================================
	 | this Stata program re-creates the six-column comparison table as
	 | provided in the comment. the estimations are first performed on the
	 | appropriate dataset, with the regression results saved, then expoted
	 | to the LaTaX format at the very end of the program
	==========================================================================

*/


clear all

set more off
global FORCE_CLEAN 0

do "${CODE}/table_generation_function.do"

capture program drop ensure_race_vars
program define ensure_race_vars
    local allvars ""
    capture qui ds
    if !_rc local allvars "`r(varlist)'"
    local has_aprace = strpos(" `allvars' ", " aprace ") > 0
    local has_apracex = strpos(" `allvars' ", " apracex ") > 0
    local has_aprace_x = strpos(" `allvars' ", " aprace_x ") > 0
    if !`has_aprace' {
        if `has_apracex' gen aprace = apracex
        else if `has_aprace_x' gen aprace = aprace_x
    }
    if !`has_apracex' {
        if `has_aprace' gen apracex = aprace
        else if `has_aprace_x' gen apracex = aprace_x
    }
    capture confirm variable ofcolor
    if _rc {
        gen ofcolor = 0
        replace ofcolor = 1 if aprace == 2 | aprace == 3 | aprace == 4
    }
    capture confirm variable othrace
    if _rc {
        gen othrace = 0
        replace othrace = 1 if aprace == 5
    }
end

capture program drop ensure_zip_city_vars
program define ensure_zip_city_vars
    capture confirm variable hcity_ad
    if _rc {
        capture confirm variable hcityad
        if !_rc gen hcity_ad = hcityad
    }
    capture confirm variable hcity
    if _rc {
        capture confirm variable hcity_ad
        if !_rc gen hcity = hcity_ad
        else {
            capture confirm variable hcity_rec
            if !_rc gen hcity = hcity_rec
            else {
                capture confirm variable hcity_x
                if !_rc gen hcity = hcity_x
            }
        }
    }
    else {
        capture confirm variable hcity_ad
        if !_rc replace hcity = hcity_ad if hcity_ad != ""
    }
    capture confirm variable hzip
    if _rc {
        capture confirm variable hzip_ad
        if !_rc gen hzip = hzip_ad
        else {
            capture confirm variable zip_ad
            if !_rc gen hzip = zip_ad
            else {
                capture confirm variable hzip_rec
                if !_rc gen hzip = hzip_rec
            }
        }
    }
    capture confirm string variable hcity
    if _rc {
        capture tostring hcity, replace
    }
    capture confirm string variable hzip
    if _rc {
        capture tostring hzip, replace
    }
end

capture program drop ensure_market
program define ensure_market
    capture confirm variable market
    if _rc gen market = substr(control,1,2)
end

capture program drop ensure_sequence_vars
program define ensure_sequence_vars
    capture confirm variable sequence_x
    if _rc {
        capture confirm variable sequencex
        if !_rc gen sequence_x = sequencex
        else {
            capture confirm variable sequence
            if !_rc gen sequence_x = sequence
            else {
                capture confirm variable sequence_x_x
                if !_rc gen sequence_x = sequence_x_x
            }
        }
    }
    capture confirm variable sequence_x_x
    if _rc {
        capture confirm variable sequencexx
        if !_rc gen sequence_x_x = sequencexx
        else {
            capture confirm variable sequence_x
            if !_rc gen sequence_x_x = sequence_x
        }
    }
end

capture program drop ensure_legacy_vars
program define ensure_legacy_vars
    // Create legacy x/xx variable names used in original table scripts
    capture confirm variable sequencex
    if _rc {
        capture confirm variable sequence_x
        if !_rc gen sequencex = sequence_x
        else {
            capture confirm variable sequence_x_x
            if !_rc gen sequencex = sequence_x_x
        }
    }
    capture confirm variable sequencexx
    if _rc {
        capture confirm variable sequence_x_x
        if !_rc gen sequencexx = sequence_x_x
        else {
            capture confirm variable sequencex
            if !_rc gen sequencexx = sequencex
        }
    }
    capture confirm variable monthx
    if _rc {
        capture confirm variable month_x
        if !_rc gen monthx = month_x
        else {
            capture confirm variable month
            if !_rc gen monthx = month
        }
    }
    capture confirm variable arelate2x
    if _rc {
        capture confirm variable arelate2_x
        if !_rc gen arelate2x = arelate2_x
        else {
            capture confirm variable arelate2
            if !_rc gen arelate2x = arelate2
        }
    }
    capture confirm variable hhmtypex
    if _rc {
        capture confirm variable hhmtype_x
        if !_rc gen hhmtypex = hhmtype_x
        else {
            capture confirm variable hhmtype
            if !_rc gen hhmtypex = hhmtype
        }
    }
    capture confirm variable savlbadx
    if _rc {
        capture confirm variable savlbad_x
        if !_rc gen savlbadx = savlbad_x
        else {
            capture confirm variable savlbad
            if !_rc gen savlbadx = savlbad
        }
    }
    capture confirm variable sapptamx
    if _rc {
        capture confirm variable sapptam_x
        if !_rc gen sapptamx = sapptam_x
        else {
            capture confirm variable sapptam
            if !_rc gen sapptamx = sapptam
        }
    }
    capture confirm variable tsexx
    if _rc {
        capture confirm variable tsex_x
        if !_rc gen tsexx = tsex_x
        else {
            capture confirm variable tsex
            if !_rc gen tsexx = tsex
        }
    }
    capture confirm variable tsexxx
    if _rc {
        capture confirm variable tsex_x_x
        if !_rc gen tsexxx = tsex_x_x
        else {
            capture confirm variable tsexx
            if !_rc gen tsexxx = tsexx
        }
    }
    capture confirm variable thhegaix
    if _rc {
        capture confirm variable thhegai_x
        if !_rc gen thhegaix = thhegai_x
        else {
            capture confirm variable thhegai
            if !_rc gen thhegaix = thhegai
        }
    }
    capture confirm variable tpegaix
    if _rc {
        capture confirm variable tpegai_x
        if !_rc gen tpegaix = tpegai_x
        else {
            capture confirm variable tpegai
            if !_rc gen tpegaix = tpegai
        }
    }
    capture confirm variable thighedux
    if _rc {
        capture confirm variable thighedu_x
        if !_rc gen thighedux = thighedu_x
        else {
            capture confirm variable thighedu
            if !_rc gen thighedux = thighedu
        }
    }
    capture confirm variable tcurtenrx
    if _rc {
        capture confirm variable tcurtenr_x
        if !_rc gen tcurtenrx = tcurtenr_x
        else {
            capture confirm variable tcurtenr
            if !_rc gen tcurtenrx = tcurtenr
        }
    }
    capture confirm variable algncurx
    if _rc {
        capture confirm variable algncur_x
        if !_rc gen algncurx = algncur_x
        else {
            capture confirm variable algncur
            if !_rc gen algncurx = algncur
        }
    }
    capture confirm variable aelng1x
    if _rc {
        capture confirm variable aelng1_x
        if !_rc gen aelng1x = aelng1_x
        else {
            capture confirm variable aelng1
            if !_rc gen aelng1x = aelng1
        }
    }
    capture confirm variable dpmtexpx
    if _rc {
        capture confirm variable dpmtexp_x
        if !_rc gen dpmtexpx = dpmtexp_x
        else {
            capture confirm variable dpmtexp
            if !_rc gen dpmtexpx = dpmtexp
        }
    }
    capture confirm variable amoversx
    if _rc {
        capture confirm variable amovers_x
        if !_rc gen amoversx = amovers_x
        else {
            capture confirm variable amovers
            if !_rc gen amoversx = amovers
        }
    }
    capture confirm variable agex
    if _rc {
        capture confirm variable age_x
        if !_rc gen agex = age_x
        else {
            capture confirm variable age
            if !_rc gen agex = age
        }
    }
    capture confirm variable aleasetpx
    if _rc {
        capture confirm variable aleasetp_x
        if !_rc gen aleasetpx = aleasetp_x
        else {
            capture confirm variable aleasetp
            if !_rc gen aleasetpx = aleasetp
        }
    }
    capture confirm variable acarownx
    if _rc {
        capture confirm variable acarown_x
        if !_rc gen acarownx = acarown_x
        else {
            capture confirm variable acarown
            if !_rc gen acarownx = acarown
        }
    }
end

capture program drop load_cleaned_data
program define load_cleaned_data
    args data_file cleaned_tag force_clean
    if "`force_clean'" == "" local force_clean "${FORCE_CLEAN}"
    local cleaned_file = "${OUTPUT}/`cleaned_tag'_cleaned.dta"
    if "`force_clean'" != "1" {
        capture confirm file "`cleaned_file'"
        if !_rc {
            use "`cleaned_file'", clear
            rename *, lower
            ensure_market
            ensure_race_vars
            ensure_zip_city_vars
            ensure_sequence_vars
            ensure_legacy_vars
            exit
        }
    }
    use "${DATA}/Generated/`data_file'", clear
    rename *, lower
    ensure_market
    ensure_race_vars
    ensure_zip_city_vars
    ensure_sequence_vars
    ensure_legacy_vars
    do "${CODE}/data_cleaner.do"
    save "`cleaned_file'", replace
end

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 5 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// +++ Column 1 +++
// must force clean to avoid interference from local cache from previous runs
load_cleaned_data "/nodedup/adsprocessed_correct_cities.dta" "adsprocessed_correct_cities" 1

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


run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex month market arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown" ///
    "show" ///
    "home_av" ///
    "" ///
    "" ///
    "5"
	

// +++ Column 2-3 +++
load_cleaned_data "adsprocessed_correct_cities.dta" "adsprocessed_correct_cities" 1

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

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex month market arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown" ///
    "show" ///
    "home_av" ///
    "" ///
    "" ///
    "5"

	
// +++ Column 4-6 +++

load_cleaned_data "adsprocessed_correct_cities.dta" "adsprocessed_correct_cities" 1

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

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex month market arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown" ///
    "show" ///
    "home_av" ///
    "" ///
    "" ///
    "5"

// +++ export column 1-6 to LaTeX

run_regressions_export_tables "5" "show" "home_av"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 6 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "logadprice w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad povrate_ad" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "w2012pc_rec" ///
    "w2012pc_rec" ///
    "" ///
    "" ///
    "6"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


gen condition_1 = 1
gen condition_2 = 1

run_regressions_col23_only ///
    "logadprice w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad povrate_ad" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "w2012pc_rec" ///
    "w2012pc_rec" ///
    "" ///
    "" ///
    "6"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col456_only ///
    "logadprice w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad povrate_ad" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "w2012pc_rec" ///
    "w2012pc_rec" ///
    "" ///
    "" ///
    "6"

run_regressions_export_tables "6" "w2012pc_rec" "w2012pc_rec"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 7 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "whitehi_rec" ///
    "whiteli_rec" ///
    "" ///
    "" ///
    "7"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"
	
gen condition_1 = 1
gen condition_2 = 1	

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "whitehi_rec" ///
    "whiteli_rec" ///
    "" ///
    "" ///
    "7"
	
load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"
	
gen condition_1 = 1
gen condition_2 = 1	

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "whitehi_rec" ///
    "whiteli_rec" ///
    "" ///
    "" ///
    "7"
	
run_regressions_export_tables "7" "whitehi_rec" "whiteli_rec"


// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 8 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "elementary_school_score_rec" ///
    "povrate_rec" ///
    "elementary_school_score_ad" ///
    "povrate_ad" ///
    "8"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "elementary_school_score_rec" ///
    "povrate_rec" ///
    "elementary_school_score_ad" ///
    "povrate_ad" ///
    "8"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "elementary_school_score_rec" ///
    "povrate_rec" ///
    "elementary_school_score_ad" ///
    "povrate_ad" ///
    "8"

run_regressions_export_tables "8" "elementary_school_score_rec" "povrate_rec"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 9 ==============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

gen condition_1 = 1
gen condition_2 = 0
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "sfcount_rec" ///
    "sfcount_rec" ///
    "sfcount_ad" ///
    "sfcount_ad" ///
    "9"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

gen condition_1 = 1
gen condition_2 = 0
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "sfcount_rec" ///
    "sfcount_rec" ///
    "sfcount_ad" ///
    "sfcount_ad" ///
    "9"
	
	
load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"

gen condition_1 = 1
gen condition_2 = 0
replace condition_2 = 1 if kidsx == 1 & tsexxx == 0

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "sfcount_rec" ///
    "sfcount_rec" ///
    "sfcount_ad" ///
    "sfcount_ad" ///
    "9"

run_regressions_export_tables "9" "sfcount_rec" "sfcount_rec"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 10A =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_testscores_correct_cities.dta" "HUDprocessed_testscores_correct_cities" 1

process_data_nodedup "HUDprocessed_testscores_correct_cities.csv" 1 "" "hcityx"

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


run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex" ///
    "mn_avg_ol_elem_rec" ///
    "mn_avg_ol_middle_rec" ///
    "mn_avg_ol_elem_ad" ///
    "mn_avg_ol_middle_ad" ///
    "10"


load_cleaned_data "HUDprocessed_testscores_correct_cities.dta" "HUDprocessed_testscores_correct_cities" 1

process_data "HUDprocessed_testscores_correct_cities.csv" 1 "" "hcityx"

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

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex" ///
    "mn_avg_ol_elem_rec" ///
    "mn_avg_ol_middle_rec" ///
    "mn_avg_ol_elem_ad" ///
    "mn_avg_ol_middle_ad" ///
    "10"


load_cleaned_data "HUDprocessed_testscores_correct_cities.dta" "HUDprocessed_testscores_correct_cities" 1

process_data "HUDprocessed_testscores_correct_cities.csv" 1 "" "hcity_ad"

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

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencex monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex" ///
    "mn_avg_ol_elem_rec" ///
    "mn_avg_ol_middle_rec" ///
    "mn_avg_ol_elem_ad" ///
    "mn_avg_ol_middle_ad" ///
    "10"

run_regressions_export_tables "10A" "mn_avg_ol_elem_rec" "mn_avg_ol_middle_rec"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 10B ============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

keep if kidsx == 1 & tsexxx == 0

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "skill_rec" ///
    "singlefamily_rec" ///
    "skill_ad" ///
    "singlefamily_ad" ///
    "10"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

keep if kidsx == 1 & tsexxx == 0

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "skill_rec" ///
    "singlefamily_rec" ///
    "skill_ad" ///
    "singlefamily_ad" ///
    "10"
	
load_cleaned_data "nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"

keep if kidsx == 1 & tsexxx == 0

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "skill_rec" ///
    "singlefamily_rec" ///
    "skill_ad" ///
    "singlefamily_ad" ///
    "10"

run_regressions_export_tables "10B" "skill_rec" "singlefamily_rec"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 11 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


clean_vars "povrate_rec povrate_ad"
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "low_povrate" ///
    "low_povrate" ///
    "" ///
    "" ///
    "11"

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

clean_vars "povrate_rec povrate_ad"
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col23_only ///
    "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "low_povrate" ///
    "low_povrate" ///
    "" ///
    "" ///
    "11"

	
load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"

clean_vars "povrate_rec povrate_ad"
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col456_only ///
    "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "low_povrate" ///
    "low_povrate" ///
    "" ///
    "" ///
    "11"

run_regressions_export_tables "11" "low_povrate" "low_povrate"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 12 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


clean_vars "medincome_rec"
qui gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "lnmincome_rec" ///
    "lnmincome_rec" ///
    "medincome_ad" ///
    "medincome_ad" ///
    "12"
	
	
load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"

clean_vars "medincome_rec"
qui gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "lnmincome_rec" ///
    "lnmincome_rec" ///
    "medincome_ad" ///
    "medincome_ad" ///
    "12"
	
	
load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"

clean_vars "medincome_rec"
qui gen lnmincome_rec = log(medincome_rec)

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "lnmincome_rec" ///
    "lnmincome_rec" ///
    "medincome_ad" ///
    "medincome_ad" ///
    "12"
	
run_regressions_export_tables "12" "lnmincome_rec" "lnmincome_rec"

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 13 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data_nodedup "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


clean_vars "elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec"
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "dif_esadrace" ///
    "dif_skadrace" ///
    "" ///
    "" ///
    "13"

	
load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcityx"


clean_vars "elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec"
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "dif_esadrace" ///
    "dif_skadrace" ///
    "" ///
    "" ///
    "13"
	

load_cleaned_data "HUDprocessed_census_correct_cities.dta" "HUDprocessed_census_correct_cities" 1

process_data "HUDprocessed_census_correct_cities.csv" 1 "" "hcity_ad"


clean_vars "elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec"
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

gen condition_1 = 1
gen condition_2 = 1

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice" ///
    "sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "dif_esadrace" ///
    "dif_skadrace" ///
    "" ///
    "" ///
    "13"
	
run_regressions_export_tables "13" "dif_esadrace" "dif_skadrace"


// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ===== Table 14 =============================================================
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


load_cleaned_data "/nodedup/HUDprocessed_names_correct_cities.dta" "HUDprocessed_names_correct_cities" 1

process_data_nodedup "HUDprocessed_names_correct_cities.csv" 1 "" "hcityx"


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

run_regressions_col1_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad i.transmonth i.transyear logadprice" ///
    "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "salespriceamount_rec_log" ///
    "salespriceamount_rec_log" ///
    "" ///
    "" ///
    "14"

load_cleaned_data "HUDprocessed_names_correct_cities.dta" "HUDprocessed_names_correct_cities" 1

process_data "HUDprocessed_names_correct_cities.csv" 1 "" "hcityx"

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

run_regressions_col23_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad i.transmonth i.transyear logadprice" ///
    "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "salespriceamount_rec_log" ///
    "salespriceamount_rec_log" ///
    "" ///
    "" ///
    "14"

load_cleaned_data "HUDprocessed_names_correct_cities.dta" "HUDprocessed_names_correct_cities" 1

process_data "HUDprocessed_names_correct_cities.csv" 1 "" "hcity_ad"

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

run_regressions_col456_only ///
    "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad i.transmonth i.transyear logadprice" ///
    "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx" ///
    "salespriceamount_rec_log" ///
    "salespriceamount_rec_log" ///
    "" ///
    "" ///
    "14"

run_regressions_export_tables "14" "salespriceamount_rec_log" "salespriceamount_rec_log"
