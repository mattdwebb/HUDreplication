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
    args data_file force_clean
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
    import delimited "${DATA}/`data_file'", bindquote(strict)
	
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
        forvalues cols = 1/4 {
            // SET RACIAL MINORITY VARIABLE FOR THIS COLUMN
            if inlist(`cols',1,2) {
                local racial_minority = "ofcolor"
            }
            else if inlist(`cols',3,4) {
                local racial_minority = "ofcolor othrace"
            }

            // SET CITY FIXED EFFECT FOR THIS COLUMN
            if inlist(`cols',1,3) {
                local geofe = "hcity"
            }
            else {
                local geofe = "temp_city"
            }

            // Print the current specification of the model
            disp as text "Dep. Var. is: " as result "`dependent_var_`d''" 
            disp as text "Racial Minority specification is: " as result "`racial_minority'"
            disp as text "City Fixed Effect is: " as result "`geofe'"
            disp as text "Clustered by: control (a variable representing the trial)"

            // ESTIMATE MODELS
            reghdfe `dependent_var_`d'' `racial_minority' `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(control)
            qui eststo dep_var_`d'_col_`cols'_minority
            local cols_for_depvar_`d'_minority = " `cols_for_depvar_`d'_minority' dep_var_`d'_col_`cols'_minority "

            reghdfe `dependent_var_`d'' i.aprace `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(control)
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
        using "${OUTPUT}/table`table_number`_dep_var_`d'_minority.tex", ///
        replace booktabs label ///
        mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name \& Correct Race",pattern(1 1 1 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Neighbourhood Attributes as `dependent_var_`d'', Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        se star(* 0.10 ** 0.05 *** 0.01) ///
        s(N r2_a, ///
        label("Observations" "Adjusted R$^2$")) ///
        keep(`racial_minority')

        // Output the csv file for the racial minority analyses
        esttab `cols_for_depvar_`d'_minority' ///
        using "${OUTPUT}/table`table_number`_dep_var_`d'_minority.csv", ///
        replace csv label ///
        mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name & Correct Race", pattern(1 1 1 1)) ///
        se star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a, ///
        labels("Observations" "Adjusted R^2")) ///
        keep(`racial_minority')

        // Output the Latex table for the racial categories analyses
        esttab `cols_for_depvar_`d'_categories' ///
        using "${OUTPUT}/table`table_number`_dep_var_`d'_categories.tex", ///
        replace booktabs label ///
        mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name \& Correct Race",pattern(1 1 1 1) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
        title(Neighbourhood Attributes as `dependent_var_`d'', Clustered at trial) ///
        alignment(c) page(dcolumn) nomtitle ///
        se star(* 0.10 ** 0.05 *** 0.01) ///
        s(N r2_a, ///
        label("Observations" "Adjusted R$^2$")) ///
        keep(2.apracex 3.apracex 4.apracex 5.apracex)

        // Output the CSV file for the racial categories analyses
        esttab `cols_for_depvar_`d'_categories' ///
        using "${OUTPUT}/table`table_number`_dep_var_`d'_categories.csv", ///
        replace csv label ///
        mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name & Correct Race", pattern(1 1 1 1)) ///
        se star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N r2_a, ///
        labels("Observations" "Adjusted R^2")) ///
        keep(2.apracex 3.apracex 4.apracex 5.apracex)
    }
end
