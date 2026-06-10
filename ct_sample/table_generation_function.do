if "${SELECTED_SAMPLE_CLUSTER_VAR}" == "" {
    global SELECTED_SAMPLE_CLUSTER_VAR "control"
}
if "${SELECTED_SAMPLE_CLUSTER_LABEL}" == "" {
    global SELECTED_SAMPLE_CLUSTER_LABEL "trial"
}
if "${SELECTED_SAMPLE_CLUSTER_DESC}" == "" {
    global SELECTED_SAMPLE_CLUSTER_DESC "${SELECTED_SAMPLE_CLUSTER_VAR}"
}

capture program drop clean_vars
program define clean_vars
	args all_vars
	foreach var in `all_vars' {
		qui cap replace `var' = "." if `var' == "NA" | `var' == ""
		
		// Check if the variable is a string
		capture confirm string variable `var'
		if !_rc {
			// If the variable is a string, check for non-numeric values
			qui {
				count if missing(real(`var')) & `var' != "."
				local non_numeric = r(N)
			}
			if `non_numeric' == 0 {
				qui destring `var', replace
			}
		}
	}
end

capture program drop set_hcity_source
program define set_hcity_source
    args source_var
    local source = lower("`source_var'")
    if "`source'" == "hcityad" local source "hcity_ad"

    capture confirm variable `source'
    if _rc {
        display as error "Requested hcity source variable not found: `source'"
        error 111
    }

    capture confirm string variable `source'
    if _rc {
        capture tostring `source', replace
    }

    if "`source'" == "hcity" {
        exit
    }

    capture drop hcity
    gen hcity = `source'
end


capture program drop process_data
program define process_data
    // hcity_source should be passed explicitly per file (e.g., hcity_ad for HUD corrected files)
    args data_file force_clean corrected hcity_source cleaned_tag

    if "`hcity_source'" == "" {
        local hcity_source "hcity"
    }
	
	local source_tag = lower("`hcity_source'")
    local cleaned_file = subinstr("`data_file'", ".csv", "_`source_tag'_cleaned.dta", .)
    if "`cleaned_tag'" != "" {
        local cleaned_file = "`cleaned_tag'_cleaned.dta"
    }

    if "`force_clean'" != "1" {
        capture confirm file "${OUTPUT}/`cleaned_file'"
        if !_rc {
            use "${OUTPUT}/`cleaned_file'", clear
            display "Using existing cleaned data file: ${OUTPUT}/`cleaned_file'"
            exit
        }
    }
    // import data
    import delimited "${DATA}/Generated/ct_sample/`data_file'", bindquote(strict) clear
    rename *, lower
	gen id = _n
	
	display "imported ${DATA}/`data_file'"

    /*-------------------------------------*/
    /*---- Cleaning, labelling variables --*/
    /*-------------------------------------*/

    qui gen market = substr(control,1,2)

    // Generate ofcolor as originally generated
    qui gen ofcolor = 0
    qui replace ofcolor = 1 if aprace == 2 | aprace == 3 | aprace == 4
    qui label variable ofcolor "Racial Minority"

    // Generate a dummy variable for 'other' individuals
    qui gen othrace = 0
    qui replace othrace = 1 if aprace == 5
    qui label variable othrace "Other Race"

    // Define labels for aprace
    qui label define race 1 "White" 2 "African American" 3 "Hispanic" 4 "Asian" 5 "Other Race"
    qui label values aprace race

    /*-------------------------------------*/
    /*---- Getting correct city names -----*/
    /*-------------------------------------*/

    // Standardize alias before explicit source selection
    if lower("`hcity_source'") == "hcity_ad" {
        capture confirm variable hcity_ad
        if _rc {
            capture confirm variable hcityad
            if !_rc gen hcity_ad = hcityad
        }
    }

    display "Using hcity source variable: `hcity_source'"
    set_hcity_source "`hcity_source'"

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

    // Keep in empty city name strings

    // Replace empty strings with NA for hcity or hcityx if corrected == "original"
    if "`corrected'" == "original" {
        capture confirm variable hcity
        if !_rc {
            qui replace hcity = "NA" if hcity == ""
        }
        else {
            capture confirm variable hcityx
            if !_rc {
                qui replace hcityx = "NA" if hcityx == ""
            }
        }
    }
	
    do "${CODE}/data_cleaner.do"
    
    // Save the cleaned data to be reloaded later
    save "${OUTPUT}/`cleaned_file'", replace
end

capture program drop normalize_aprace_for_regressions
program define normalize_aprace_for_regressions
    local allvars ""
    capture qui ds
    if !_rc local allvars "`r(varlist)'"
    local has_aprace = strpos(" `allvars' ", " aprace ") > 0
    local has_apracex = strpos(" `allvars' ", " apracex ") > 0
    if `has_apracex' {
        if `has_aprace' {
            drop apracex
            display "Variable apracex dropped (aprace already exists)."
        }
        else {
            rename apracex aprace
            display "Variable apracex has been renamed to aprace."
        }
    }
    else if !`has_aprace' {
        display "Neither apracex nor aprace exists in the dataset."
    }
end

capture program drop apply_hds_fixed_race_spec
program define apply_hds_fixed_race_spec
    args source_var

    local source = lower("`source_var'")
    capture confirm variable `source'
    if _rc {
        display as error "Requested HDS race source variable not found: `source'"
        error 111
    }

    tempvar race_code
    gen double `race_code' = .

    capture confirm string variable `source'
    if !_rc {
        replace `race_code' = real(`source')
    }
    else {
        replace `race_code' = `source'
    }

    replace aprace = .
    replace aprace = mod(`race_code', 10) if inlist(mod(`race_code', 10), 1, 2, 3, 4)
    count if !missing(`race_code') & missing(aprace)
    if r(N) > 0 {
        display as error "HDS race source contains values outside supported race codes 1-4: `source'"
        tab `source' if !missing(`race_code') & missing(aprace), missing
        error 459
    }

    label define race 1 "White" 2 "African American" 3 "Hispanic" 4 "Asian", replace
    label values aprace race

    replace ofcolor = .
    replace ofcolor = 0 if aprace == 1
    replace ofcolor = 1 if inlist(aprace, 2, 3, 4)

	capture drop othrace
end

capture program drop mark_complete_case
program define mark_complete_case
	args markvar varlist_text

	gen byte `markvar' = 1
	foreach raw_token in `varlist_text' {
		local token "`raw_token'"
		local token : subinstr local token "i." "", all
		local token : subinstr local token "c." "", all
		local token : subinstr local token "#" " ", all

		foreach var in `token' {
			capture confirm variable `var'
			if !_rc {
				qui replace `markvar' = 0 if missing(`var')
			}
		}
	}
end

capture program drop add_trial_count
program define add_trial_count
    capture confirm variable control
    if _rc {
        qui estadd scalar num_trials = .
        exit
    }

    capture confirm variable testerid
    if _rc {
        qui estadd scalar num_trials = .
        exit
    }

    tempvar tester_tag tester_count trial_tag
    qui egen byte `tester_tag' = tag(control testerid) if e(sample)
    qui egen int `tester_count' = total(`tester_tag'), by(control)
    qui egen byte `trial_tag' = tag(control) if e(sample) & `tester_count' >= 2
    qui count if `trial_tag'
    qui estadd scalar num_trials = r(N)
end

capture program drop add_white_outcome_stats
program define add_white_outcome_stats
    args dependent_var

    capture confirm variable aprace
    if _rc {
        qui estadd scalar white_sd = .
        qui estadd scalar white_n = .
        exit
    }

    qui summarize `dependent_var' if e(sample) & aprace == 1
    qui estadd scalar white_sd = r(sd)
    qui estadd scalar white_n = r(N)
end

capture program drop run_comparison_regressions
program define run_comparison_regressions, rclass
    args CONTROL_VARS ABS_VARS dependent_var_1 dependent_var_2 control_var_1 control_var_2 table_number data_col1 data_col23 data_col456

    local cols_for_depvar_1_minority " "
    local cols_for_depvar_1_categories " "
    local cols_for_depvar_2_minority " "
    local cols_for_depvar_2_categories " "

    forvalues d = 1/2 {
        forvalues cols = 1/6 {
            if `cols' == 1 {
                use "`data_col1'", clear
            }
            else if inrange(`cols', 2, 3) {
                use "`data_col23'", clear
            }
            else {
                use "`data_col456'", clear
            }

            local all_vars "`CONTROL_VARS' `ABS_VARS' `dependent_var_`d'' `control_var_`d''"
            clean_vars "`all_vars'"
            normalize_aprace_for_regressions

	            if `cols' > 1 {
	                if "`table_number'" == "5" {
	                    apply_hds_fixed_race_spec "race_ad"
	                    keep if !inlist(hcity, "", ".", "NA") & !inlist(place_name, "", ".", "NA") & !inlist(county_name, "", ".", "NA")
                }
                else {
                    apply_hds_fixed_race_spec "race_rec"
	                    keep if !inlist(hcityx, "", ".", "NA") & !inlist(hcity_ad, "", ".", "NA") & !inlist(place_name, "", ".", "NA") & !inlist(county_name, "", ".", "NA")
	                }
	            }

	            if inrange(`cols', 2, 5) {
	                tempvar comparison_complete_case
	                local comparison_sample_vars "`CONTROL_VARS' `ABS_VARS' `dependent_var_`d'' `control_var_`d'' aprace ofcolor"
	                if "`table_number'" == "5" {
	                    local comparison_sample_vars "`comparison_sample_vars' hcity temp_city place_name"
	                }
	                else {
	                    local comparison_sample_vars "`comparison_sample_vars' hcityx hcity_ad temp_city place_name"
	                }
	                mark_complete_case `comparison_complete_case' "`comparison_sample_vars'"
	                keep if `comparison_complete_case'
	            }

	            if `cols' == 1 {
	                local racial_minority "ofcolor"
                if "`table_number'" == "5" {
                    local geofe "hcity"
                }
                else {
                    local geofe "hcityx"
                }
            }
            else if `cols' == 2 {
                local racial_minority "ofcolor"
                if "`table_number'" == "5" {
                    local geofe "hcity"
                }
                else {
                    local geofe "hcityx"
                }
            }
            else if `cols' == 3 {
                local racial_minority "ofcolor"
                local geofe "temp_city"
            }
            else if `cols' == 4 {
                local racial_minority "ofcolor"
                local geofe "temp_city"
            }
            else if `cols' == 5 {
                local racial_minority "ofcolor"
                local geofe "place_name"
            }
            else {
                local racial_minority "ofcolor"
                local geofe "county_name"
            }

            disp as text "Dep. Var. is: " as result "`dependent_var_`d''"
            disp as text "Racial Minority specification is: " as result "`racial_minority'"
            disp as text "City Fixed Effect is: " as result "`geofe'"
            disp as text "Clustered by: ${SELECTED_SAMPLE_CLUSTER_DESC}"

            if "`table_number'" == "13" & `cols' == 1 & "${SELECTED_SAMPLE_CLUSTER_VAR}" == "control" {
                reghdfe `dependent_var_`d'' `racial_minority' `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle
            }
            else {
                reghdfe `dependent_var_`d'' `racial_minority' `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(${SELECTED_SAMPLE_CLUSTER_VAR})
            }

            matrix hdfe = e(dof_table)
            local num_levels_geofe = hdfe[rowsof(hdfe),1]
            qui estadd scalar num_cities  =  `num_levels_geofe'
            add_trial_count

            qui eststo dep_var_`d'_col_`cols'_minority
            local cols_for_depvar_`d'_minority = " `cols_for_depvar_`d'_minority' dep_var_`d'_col_`cols'_minority "

            if "`table_number'" == "13" & `cols' == 1 & "${SELECTED_SAMPLE_CLUSTER_VAR}" == "control" {
                reghdfe `dependent_var_`d'' i.aprace `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle
            }
            else {
                reghdfe `dependent_var_`d'' i.aprace `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(${SELECTED_SAMPLE_CLUSTER_VAR})
            }

            matrix hdfe = e(dof_table)
            local num_levels_geofe = hdfe[rowsof(hdfe),1]
            qui estadd scalar num_cities  =  `num_levels_geofe'
            add_trial_count

            qui eststo dep_var_`d'_col_`cols'_categories
            local cols_for_depvar_`d'_categories = " `cols_for_depvar_`d'_categories' dep_var_`d'_col_`cols'_categories "

            disp as text "*******************************************************"
        }
    }

    local racial_minority "ofcolor"
    forvalues d = 1/2 {
        esttab ///
        dep_var_`d'_col_1_minority ///
        dep_var_`d'_col_2_minority ///
        dep_var_`d'_col_3_minority ///
        dep_var_`d'_col_4_minority ///
        dep_var_`d'_col_5_minority ///
        dep_var_`d'_col_6_minority ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_minority.tex", ///
        replace booktabs label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name \& Correct Race" "Proper City Name \& Correct Race" "Place Name \& Correct Race" "County Name \& Correct Race",pattern(1 1 1 1 1 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Neighbourhood Attributes as `dependent_var_`d'', Clustered at ${SELECTED_SAMPLE_CLUSTER_LABEL}) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities" "Number of Trials")) ///
        keep(`racial_minority')

        esttab ///
        dep_var_`d'_col_1_minority ///
        dep_var_`d'_col_2_minority ///
        dep_var_`d'_col_3_minority ///
        dep_var_`d'_col_4_minority ///
        dep_var_`d'_col_5_minority ///
        dep_var_`d'_col_6_minority ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_minority.csv", ///
        replace csv label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name \& Correct Race" "Proper City Name \& Correct Race" "Place Name \& Correct Race" "County Name \& Correct Race",pattern(1 1 1 1 1 1)) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities" "Number of Trials")) ///
        keep(`racial_minority')

        esttab ///
        dep_var_`d'_col_1_categories ///
        dep_var_`d'_col_2_categories ///
        dep_var_`d'_col_3_categories ///
        dep_var_`d'_col_4_categories ///
        dep_var_`d'_col_5_categories ///
        dep_var_`d'_col_6_categories ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_categories.tex", ///
        replace booktabs label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name \& Correct Race" "Proper City Name \& Correct Race" "Place Name \& Correct Race" "County Name \& Correct Race",pattern(1 1 1 1 1 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Neighbourhood Attributes as `dependent_var_`d'', Clustered at ${SELECTED_SAMPLE_CLUSTER_LABEL}) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities" "Number of Trials")) ///
        keep(2.aprace 3.aprace 4.aprace 5.aprace)

        esttab ///
        dep_var_`d'_col_1_categories ///
        dep_var_`d'_col_2_categories ///
        dep_var_`d'_col_3_categories ///
        dep_var_`d'_col_4_categories ///
        dep_var_`d'_col_5_categories ///
        dep_var_`d'_col_6_categories ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_categories.csv", ///
        replace csv label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name \& Correct Race" "Proper City Name \& Correct Race" "Place Name \& Correct Race" "County Name \& Correct Race",pattern(1 1 1 1 1 1)) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities" "Number of Trials")) ///
        keep(2.aprace 3.aprace 4.aprace 5.aprace)
    }
end

capture program drop correct_table
program define correct_table, rclass
	args CONTROL_VARS ABS_VARS dependent_vars control_vars_1 control_vars_2 control_vars_3 control_vars_4 control_vars_5 control_vars_6 table_number analysis_type abs_vars_1 abs_vars_2 abs_vars_3 abs_vars_4 abs_vars_5 abs_vars_6 override

	local full_sample_CONTROL_VARS "`CONTROL_VARS'"
	local full_sample_ABS_VARS "`ABS_VARS'"
	forvalues full_sample_i = 1/6 {
		local full_sample_control_vars_`full_sample_i' "`control_vars_`full_sample_i''"
		local full_sample_abs_vars_`full_sample_i' "`abs_vars_`full_sample_i''"
	}

    local drop_trial_invariant 0
    if "`analysis_type'" == "corrected" {
        local drop_trial_invariant 1
    }

	if `drop_trial_invariant' {
		// Corrected specification: keep the trial fixed effect and the
		// tester/appointment controls used in the reconstructed-sample design. In this
        // mode, use the same retained baseline controls across all tables
        // rather than inheriting smaller table-specific C&T control sets.
        local within_trial_abs_vars_ads "control sequencex month arelate2 sapptam thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown"
        local within_trial_abs_vars_hud "control sequencexx monthx arelate2x sapptamx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"
        local within_trial_reg_vars "i.transmonth i.transyear transmonth transyear"

        if substr("`table_number'", 1, 2) != "13" {
            if "`table_number'" == "5" {
                local ABS_VARS "`within_trial_abs_vars_ads'"
            }
            else {
                local ABS_VARS "`within_trial_abs_vars_hud'"
            }
        }
        local CONTROL_VARS : list CONTROL_VARS & within_trial_reg_vars

        forvalues filter_i = 1/6 {
            if "`override'" != "override" {
                local control_vars_`filter_i' : list control_vars_`filter_i' & within_trial_reg_vars
            }
            if substr("`table_number'", 1, 2) != "13" {
                local abs_vars_`filter_i' ""
            }
        }

        display as text "Corrected drop-trial-invariant-controls specification is active."
    }

    local all_vars "`CONTROL_VARS' `ABS_VARS'"
    foreach dv in `dependent_vars' {
        local all_vars "`all_vars' `dv'"
    }
    
	// Extract all unique control variables
	local unique_controls "`control_vars_1' `control_vars_2' `control_vars_3' `control_vars_4' `control_vars_5' `control_vars_6'"
	local all_vars "`all_vars' `unique_controls'"
	if `drop_trial_invariant' {
		local all_vars "`all_vars' `full_sample_CONTROL_VARS' `full_sample_ABS_VARS'"
		forvalues full_sample_i = 1/6 {
			local all_vars "`all_vars' `full_sample_control_vars_`full_sample_i'' `full_sample_abs_vars_`full_sample_i''"
		}
	}

	clean_vars "`all_vars'"
    normalize_aprace_for_regressions

    if "`analysis_type'" == "corrected" {
        if "`table_number'" == "5" {
            apply_hds_fixed_race_spec "race_ad"
            keep if !inlist(hcity, "", ".", "NA") & !inlist(place_name, "", ".", "NA") & !inlist(county_name, "", ".", "NA")
        }
        else {
            apply_hds_fixed_race_spec "race_rec"
            keep if !inlist(hcityx, "", ".", "NA") & !inlist(hcity_ad, "", ".", "NA") & !inlist(place_name, "", ".", "NA") & !inlist(county_name, "", ".", "NA")
        }
    }

    // Create empty placeholder to store column names
    local cols_for_all_regressions = " "

    local num_regressions : word count `dependent_vars'
    
    local cols_for_minority ""
    local cols_for_categories ""

    forvalues i = 1/`num_regressions' {
        local dependent_var : word `i' of `dependent_vars'
        disp as text "`dependent_var'"

        // Extract the i-th set of control variables
        local control_vars = "`control_vars_`i''"
        disp as text "`control_vars'"

        // Extract the i-th set of absorbed variables
        local abs_vars = "`abs_vars_`i''"
        disp as text "`abs_vars'"

        disp as text "Analysis type is: " as result "`analysis_type'" 

        // SET RACIAL MINORITY VARIABLE AND CITY FIXED EFFECT BASED ON ANALYSIS TYPE
        if "`analysis_type'" == "original" {
            local racial_minority = "ofcolor"
            local geofe = "hcity"
        }
        else if "`analysis_type'" == "corrected" {
            local racial_minority = "ofcolor"
            local geofe = "place_name"
        }
        if `drop_trial_invariant' {
            local geofe ""
        }

        // Print the current specification of the model
        disp as text "Dep. Var. is: " as result "`dependent_var'" 
        disp as text "Control Vars. are: " as result "`CONTROL_VARS' `control_vars'"
        disp as text "Absorbed Vars. are: " as result "`ABS_VARS' `abs_vars'"
        disp as text "City Fixed Effect is: " as result "`geofe'"
	        disp as text "Clustered by: ${SELECTED_SAMPLE_CLUSTER_DESC}"

	        if `drop_trial_invariant' {
	            tempvar full_control_complete_case
	            local fs_same_ctrl ""
	            local fs_same_abs ""
	            forvalues fs_j = 1/`num_regressions' {
	                local fs_dep_j : word `fs_j' of `dependent_vars'
	                if "`fs_dep_j'" == "`dependent_var'" {
	                    local fs_same_ctrl "`fs_same_ctrl' `full_sample_control_vars_`fs_j''"
	                    local fs_same_abs "`fs_same_abs' `full_sample_abs_vars_`fs_j''"
	                }
	            }
	            local full_control_sample_vars "`full_sample_CONTROL_VARS' `full_sample_ABS_VARS' `fs_same_ctrl' `fs_same_abs' `dependent_var' aprace ofcolor"
	            if "`table_number'" == "5" {
	                local full_control_sample_vars "`full_control_sample_vars' hcity temp_city place_name county_name"
	            }
	            else {
	                local full_control_sample_vars "`full_control_sample_vars' hcityx hcity_ad temp_city place_name county_name"
	            }
	            mark_complete_case `full_control_complete_case' "`full_control_sample_vars'"
	        }
	        else {
	            tempvar full_control_complete_case
	            gen byte `full_control_complete_case' = 1
	        }

	        // ESTIMATE MODELS
	        if "`override'" != "override" {
	            disp as text "Racial Minority specification is: " as result "`racial_minority'"
	            reghdfe `dependent_var' `racial_minority' `CONTROL_VARS' `control_vars' if condition_`i' & `full_control_complete_case', absorb(`ABS_VARS' `abs_vars' `geofe') keepsingle cluster(${SELECTED_SAMPLE_CLUSTER_VAR})
            
            // Extract number of levels of city variable
            if "`geofe'" != "" {
                matrix hdfe = e(dof_table)
                local num_levels_geofe = hdfe[rowsof(hdfe),1]
                qui estadd scalar num_cities = `num_levels_geofe'
            }
            else {
                qui estadd scalar num_cities = .
            }
            add_trial_count
            add_white_outcome_stats "`dependent_var'"

            qui eststo regression_`i'_minority
            local cols_for_minority = "`cols_for_minority' regression_`i'_minority"

	            reghdfe `dependent_var' i.aprace `CONTROL_VARS' `control_vars' if condition_`i' & `full_control_complete_case', absorb(`ABS_VARS' `abs_vars' `geofe') keepsingle cluster(${SELECTED_SAMPLE_CLUSTER_VAR})
            
            // Extract number of levels of city variable
            if "`geofe'" != "" {
                matrix hdfe = e(dof_table)
                local num_levels_geofe = hdfe[rowsof(hdfe),1]
                qui estadd scalar num_cities = `num_levels_geofe'
            }
            else {
                qui estadd scalar num_cities = .
            }
            add_trial_count
            add_white_outcome_stats "`dependent_var'"

            qui eststo regression_`i'_categories
            local cols_for_categories = "`cols_for_categories' regression_`i'_categories"
        }
	        else {
	            disp as text "Override option selected. Estimating model with control variables as primary variables of interest."
	            reghdfe `dependent_var' `control_vars' `CONTROL_VARS' if condition_`i' & `full_control_complete_case', absorb(`ABS_VARS' `abs_vars' `geofe') keepsingle cluster(${SELECTED_SAMPLE_CLUSTER_VAR})
            
            // Extract number of levels of city variable
            if "`geofe'" != "" {
                matrix hdfe = e(dof_table)
                local num_levels_geofe = hdfe[rowsof(hdfe),1]
                qui estadd scalar num_cities = `num_levels_geofe'
            }
            else {
                qui estadd scalar num_cities = .
            }
            add_trial_count
            add_white_outcome_stats "`dependent_var'"

            qui eststo regression_`i'_override
            local cols_for_override = "`cols_for_override' regression_`i'_override"
        }

        disp as text "*******************************************************"
    }

    /*-------------------------------------*/
    /*- Export Results to LaTeX and CSV ---*/
    /*-------------------------------------*/

    // Dynamically generate the number of mgroups and columns
    local num_regressions = wordcount("`cols_for_minority'")
    local mgroups_pattern = ""
    local mgroups_titles = ""
    forvalues i = 1/`num_regressions' {
        local mgroups_pattern = "`mgroups_pattern' 1"
        local mgroups_titles = `"`mgroups_titles' "Regression `i'""'
    }

    if "`override'" != "override" {
        // Determine the keep list based on the analysis type
        local keep_list_minority "ofcolor"
        local keep_list_categories "2.aprace 3.aprace 4.aprace"
        if "`analysis_type'" != "corrected" {
            local keep_list_categories "`keep_list_categories' 5.aprace"
        }
        
        // Output the Latex table for the racial minority analyses
        esttab `cols_for_minority' ///
        using "${OUTPUT}/table`table_number'_minority_`analysis_type'.tex", ///
        replace booktabs label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern') ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Multiple Regressions Results - Racial Minority, Clustered at ${SELECTED_SAMPLE_CLUSTER_LABEL}) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities" "Number of Trials")) ///
        keep(`keep_list_minority')

        // Output the CSV file for the racial minority analyses
        esttab `cols_for_minority' ///
        using "${OUTPUT}/table`table_number'_minority_`analysis_type'.csv", ///
        replace csv label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern')) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials white_sd white_n, fmt(0 4 0 0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities" "Number of Trials" "White SD" "White N")) ///
        keep(`keep_list_minority')

        // Output the Latex table for the racial categories analyses
        esttab `cols_for_categories' ///
        using "${OUTPUT}/table`table_number'_categories_`analysis_type'.tex", ///
        replace booktabs label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern') ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Multiple Regressions Results - Racial Categories, Clustered at ${SELECTED_SAMPLE_CLUSTER_LABEL}) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities" "Number of Trials")) ///
        keep(`keep_list_categories')

        // Output the CSV file for the racial categories analyses
        esttab `cols_for_categories' ///
        using "${OUTPUT}/table`table_number'_categories_`analysis_type'.csv", ///
        replace csv label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern')) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials white_sd white_n, fmt(0 4 0 0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities" "Number of Trials" "White SD" "White N")) ///
        keep(`keep_list_categories')
    }
    else {
        // Output the Latex table for the override analyses
        esttab `cols_for_override' ///
        using "${OUTPUT}/table`table_number'_override_`analysis_type'.tex", ///
        replace booktabs label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern') ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Multiple Regressions Results - Override, Clustered at ${SELECTED_SAMPLE_CLUSTER_LABEL}) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials, fmt(0 4 0 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities" "Number of Trials")) ///
        keep(`unique_controls')

        // Output the CSV file for the override analyses
        esttab `cols_for_override' ///
        using "${OUTPUT}/table`table_number'_override_`analysis_type'.csv", ///
        replace csv label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern')) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities num_trials white_sd white_n, fmt(0 4 0 0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities" "Number of Trials" "White SD" "White N")) ///
        keep(`unique_controls')
    }
end
