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


capture program drop process_data
program define process_data
    args data_file force_clean corrected
    local cleaned_file = subinstr("`data_file'", ".csv", "_cleaned.dta", .)

    if "`force_clean'" != "1" {
        capture confirm file "${OUTPUT}/`cleaned_file'"
        if !_rc {
            use "${OUTPUT}/`cleaned_file'", clear
            display "Using existing cleaned data file: ${OUTPUT}/`cleaned_file'"
            exit
        }
    }
    // import data
    import delimited "${DATA}/`data_file'", bindquote(strict) clear
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

capture program drop run_regressions
program define run_regressions, rclass
    args CONTROL_VARS ABS_VARS dependent_var_1 dependent_var_2 control_var_1 control_var_2 table_number

    local all_vars "`CONTROL_VARS' `ABS_VARS' `dependent_var_1' `dependent_var_2' `control_var_1' `control_var_2'"

    clean_vars "`all_vars'"

    // Create empty placeholders to store column names
    local cols_for_depvar_1_minority = " "
    local cols_for_depvar_1_categories = " "
    local cols_for_depvar_2_minority = " "
    local cols_for_depvar_2_categories = " "

    save "temp_data_table`table_number'_formatted.dta", replace

    forvalues d = 1/2 {
        forvalues cols = 1/3 {
            // SET RACIAL MINORITY VARIABLE FOR THIS COLUMN
            if `cols' == 1 {
                local racial_minority = "ofcolor"
                local geofe = "hcity"
            }
            else if `cols' == 2 {
                local racial_minority = "ofcolor othrace"
                local geofe = "hcity"
            }
            else if `cols' == 3 {
                local racial_minority = "ofcolor othrace"
                local geofe = "temp_city"
            }

            // Print the current specification of the model
            disp as text "Dep. Var. is: " as result "`dependent_var_`d''" 
            disp as text "Racial Minority specification is: " as result "`racial_minority'"
            disp as text "City Fixed Effect is: " as result "`geofe'"
            disp as text "Clustered by: control (a variable representing the trial)"


            // ESTIMATE MODELS
            reghdfe `dependent_var_`d'' `racial_minority' `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(control)

            // Extract number of levels of city variable
			matrix hdfe = e(dof_table)
			local num_levels_geofe = hdfe[rowsof(hdfe),1]
			qui estadd scalar num_cities  =  `num_levels_geofe'

            qui eststo dep_var_`d'_col_`cols'_minority
            local cols_for_depvar_`d'_minority = " `cols_for_depvar_`d'_minority' dep_var_`d'_col_`cols'_minority "


            reghdfe `dependent_var_`d'' i.aprace `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(control)

            // Extract number of levels of city variable
			matrix hdfe = e(dof_table)
			local num_levels_geofe = hdfe[rowsof(hdfe),1]
			qui estadd scalar num_cities  =  `num_levels_geofe'

            qui eststo dep_var_`d'_col_`cols'_categories
            local cols_for_depvar_`d'_categories = " `cols_for_depvar_`d'_categories' dep_var_`d'_col_`cols'_categories "
        }
        disp as text "*******************************************************"
    }

    /*-------------------------------------*/
    /*- Export Results to LaTeX and CSV ---*/
    /*-------------------------------------*/

    forvalues d = 1/2 {
        // Output the Latex table for the racial minority analyses
        esttab `cols_for_depvar_`d'_minority' ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_minority.tex", ///
        replace booktabs label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name & Correct Race",pattern(1 1 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Neighbourhood Attributes as `dependent_var_`d'', Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities")) ///
        keep(`racial_minority')

        // Output the csv file for the racial minority analyses
        esttab `cols_for_depvar_`d'_minority' ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_minority.csv", ///
        replace csv label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name & Correct Race", pattern(1 1 1)) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities")) ///
        keep(`racial_minority')

        // Output the Latex table for the racial categories analyses
        esttab `cols_for_depvar_`d'_categories' ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_categories.tex", ///
        replace booktabs label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name & Correct Race",pattern(1 1 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Neighbourhood Attributes as `dependent_var_`d'', Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities")) ///
        keep(2.apracex 3.apracex 4.apracex 5.apracex)

        // Output the CSV file for the racial categories analyses
        esttab `cols_for_depvar_`d'_categories' ///
        using "${OUTPUT}/table`table_number'_dep_var_`d'_categories.csv", ///
        replace csv label ///
        mgroups("Original Data" "Correct Race Only" "Updated City Name & Correct Race", pattern(1 1 1)) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities")) ///
        keep(2.apracex 3.apracex 4.apracex 5.apracex)
    }
end



capture program drop correct_table
program define correct_table, rclass
    args CONTROL_VARS ABS_VARS dependent_vars control_vars_1 control_vars_2 control_vars_3 control_vars_4 control_vars_5 control_vars_6 table_number analysis_type abs_vars_1 abs_vars_2 abs_vars_3 abs_vars_4 abs_vars_5 abs_vars_6 override

    local all_vars "`CONTROL_VARS' `ABS_VARS'"
    foreach dv in `dependent_vars' {
        local all_vars "`all_vars' `dv'"
    }
    
    // Extract all unique control variables
    local unique_controls "`control_vars_1' `control_vars_2' `control_vars_3' `control_vars_4' `control_vars_5' `control_vars_6'"
    local all_vars "`all_vars' `unique_controls'"

    clean_vars "`all_vars'"

    // Create empty placeholder to store column names
    local cols_for_all_regressions = " "

    save "temp_data_table`table_number'_formatted.dta", replace

    local num_regressions : word count `dependent_vars'
    
    local cols_for_minority ""
    local cols_for_categories ""

    // Check for apracex and rename it to aprace if present
    capture confirm variable apracex
    if !_rc {
        rename apracex aprace
        display "Variable apracex has been renamed to aprace."
    } 
    else {
        // If apracex doesn't exist, check if aprace already exists
        capture confirm variable aprace
        if _rc {
            display "Neither apracex nor aprace exists in the dataset."
        }
        else {
            display "Variable aprace already exists. No renaming needed."
        }
    }

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
            local racial_minority = "ofcolor othrace"
            local geofe = "temp_city"
        }

        // Print the current specification of the model
        disp as text "Dep. Var. is: " as result "`dependent_var'" 
        disp as text "Control Vars. are: " as result "`CONTROL_VARS' `control_vars'"
        disp as text "Absorbed Vars. are: " as result "`ABS_VARS' `abs_vars'"
        disp as text "City Fixed Effect is: " as result "`geofe'"
        disp as text "Clustered by: control (a variable representing the trial)"

        // ESTIMATE MODELS
        if "`override'" != "override" {
            disp as text "Racial Minority specification is: " as result "`racial_minority'"
            reghdfe `dependent_var' `racial_minority' `CONTROL_VARS' `control_vars' if condition_`i', absorb(`ABS_VARS' `abs_vars' `geofe') keepsingle cluster(control)
            
            // Extract number of levels of city variable
            matrix hdfe = e(dof_table)
            local num_levels_geofe = hdfe[rowsof(hdfe),1]
            qui estadd scalar num_cities = `num_levels_geofe'

            qui eststo regression_`i'_minority
            local cols_for_minority = "`cols_for_minority' regression_`i'_minority"

            reghdfe `dependent_var' i.aprace `CONTROL_VARS' `control_vars' if condition_`i', absorb(`ABS_VARS' `abs_vars' `geofe') keepsingle cluster(control)
			
			if inlist(`i',2,4) {
					cap drop in_sample
					generate in_sample = e(sample)
					export delimited if in_sample==1 using "${OUTPUT}/sampleAnthony_`i'.csv", replace
			}
            
            // Extract number of levels of city variable
            matrix hdfe = e(dof_table)
            local num_levels_geofe = hdfe[rowsof(hdfe),1]
            qui estadd scalar num_cities = `num_levels_geofe'

            qui eststo regression_`i'_categories
            local cols_for_categories = "`cols_for_categories' regression_`i'_categories"
        }
        else {
            disp as text "Override option selected. Estimating model with control variables as primary variables of interest."
            reghdfe `dependent_var' `control_vars' `CONTROL_VARS' if condition_`i', absorb(`ABS_VARS' `abs_vars' `geofe') keepsingle cluster(control)
            
            // Extract number of levels of city variable
            matrix hdfe = e(dof_table)
            local num_levels_geofe = hdfe[rowsof(hdfe),1]
            qui estadd scalar num_cities = `num_levels_geofe'

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
        local keep_list_minority = cond("`analysis_type'" == "corrected", "ofcolor othrace", "ofcolor")
        
        // Output the Latex table for the racial minority analyses
        esttab `cols_for_minority' ///
        using "${OUTPUT}/table`table_number'_minority_`analysis_type'.tex", ///
        replace booktabs label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern') ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Multiple Regressions Results - Racial Minority, Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities")) ///
        keep(`keep_list_minority')

        // Output the CSV file for the racial minority analyses
        esttab `cols_for_minority' ///
        using "${OUTPUT}/table`table_number'_minority_`analysis_type'.csv", ///
        replace csv label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern')) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities")) ///
        keep(`keep_list_minority')

        // Output the Latex table for the racial categories analyses
        esttab `cols_for_categories' ///
        using "${OUTPUT}/table`table_number'_categories_`analysis_type'.tex", ///
        replace booktabs label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern') ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Multiple Regressions Results - Racial Categories, Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities")) ///
        keep(2.aprace 3.aprace 4.aprace 5.aprace)

        // Output the CSV file for the racial categories analyses
        esttab `cols_for_categories' ///
        using "${OUTPUT}/table`table_number'_categories_`analysis_type'.csv", ///
        replace csv label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern')) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities")) ///
        keep(2.aprace 3.aprace 4.aprace 5.aprace)
    }
    else {
        // Output the Latex table for the override analyses
        esttab `cols_for_override' ///
        using "${OUTPUT}/table`table_number'_override_`analysis_type'.tex", ///
        replace booktabs label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern') ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Multiple Regressions Results - Override, Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        label("Observations" "Adjusted R$^2$" "Number of Cities")) ///
        keep(`unique_controls')

        // Output the CSV file for the override analyses
        esttab `cols_for_override' ///
        using "${OUTPUT}/table`table_number'_override_`analysis_type'.csv", ///
        replace csv label ///
        mgroups(`mgroups_titles', pattern(`mgroups_pattern')) ///
        cells("b(star fmt(4))" se(par fmt(4)) ci(fmt(4) par)) ///
        starlevels(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a num_cities, fmt(0 4 0) ///
        labels("Observations" "Adjusted R^2" "Number of Cities")) ///
        keep(`unique_controls')
    }
end
