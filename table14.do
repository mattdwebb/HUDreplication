/* Stata Do File for Table 9 */
/* Written by: Anthony McCanny */
/* Based on code by: Shi Chen */
/* Date: May 14, 2024 */

clear all
// import data
import delimited "${DATA}/HUDprocessed_JPE_names_042021.csv", bindquote(strict)

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


//global DESVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad sequencex monthx sapptamx algncurx aelng1x dpmtexpx agex aleasetpx acarownx elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec logadprice sf_count_rec sf_count_ad kidsx tsexxx "
	
//foreach var in $DESVARS {
//	qui cap replace `var' = "." if `var' == "NA" | `var' == ""
//	qui cap destring `var', replace force
//}

/*-------------------------------------*/
/*--------- Subsetting Data -----------*/
/*-------------------------------------*/

// Generate an indicator for the full dataset
qui gen full_dataset = 1

// Generate an indicator for mothers
gen mother = 0
// Set mother to 1 when a participants has kids and sex is female
replace mother = 1 if kidsx == 1 & tsexxx == 0



/*-------------------------------------*/
/*---- Getting correct city names -----*/
/*-------------------------------------*/

//do "${CODE}/data_cleaner.do"

// Save the cleaned data to be reloaded later
//save "temp_data_table14.dta", replace

use "temp_data_table14.dta", clear

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


// In original R analysis, missing values of hcity were treated as their own category, to allow this in Stata, we set missing values to the string "missing"
replace hcity = "missing" if hcity == ""
replace temp_city = "missing" if temp_city == ""

/*-------------------------------------*/
/*---- Regressions --------------------*/
/*-------------------------------------*/

// Define the general control variables for the regression
local CONTROL_VARS "transmonth transyear logadprice"

// Define the general fixed effects to be absorbed in the estimation process
local ABS_VARS "control sequencexx monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

// Define the dependent variables in the two sets of regressions
local dependent_var_1 = "salespriceamount_rec"
local dependent_var_2 = "salespriceamount_rec"

// Define the regression specific control variables to be introduced to regressions 1 and 2 (corresponding to dependent_var_1 and 2 above)
local control_var_1 = ""
local control_var_2 = ""

// List any variables used in the conditions that aren't listed elsewhere, so that they can be formatted properly
local condition_vars = ""


local all_vars "`CONTROL_VARS' `ABS_VARS' `dependent_var_1' `dependent_var_2' `control_var_1' `control_var_2' `condition_vars'"

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

save "temp_data_table14_formatted.dta", replace

// Output the exact type of salespriceamount_rec
describe salespriceamount_rec
local var_type = r(type)
display as text "The type of salespriceamount_rec is: " as result "`var_type'"



// New testing code for salespriceamount_rec

    // Check if the variable is a string
    capture confirm numeric variable salespriceamount_rec
    if _rc {
        display as text "salespriceamount_rec is not numeric."
    } 
    else {
        display as text "salespriceamount_rec is numeric."
    }

    // Check for missing values
    count if missing(salespriceamount_rec)
    local missing_values = r(N)
    disp as text "Number of missing values in salespriceamount_rec: " as result "`missing_values'"
    
    // Check for outliers (values outside the range 10000 to 10000000)
    count if salespriceamount_rec < 10000 | salespriceamount_rec > 10000000
    local outliers = r(N)
    disp as text "Number of outliers in salespriceamount_rec: " as result "`outliers'"
    
    // Summary statistics
    summarize salespriceamount_rec
    disp as text "Summary statistics for salespriceamount_rec:"
    disp as text "Mean: " as result `r(mean)'
    disp as text "Min: " as result `r(min)'
    disp as text "Max: " as result `r(max)'



// Define the data subset indicator variable for regressions 1 and 2 (corresponding to dependent_var_1 and 2 above)
gen condition_1 = 0 
replace condition_1 = 1 if salespriceamount_rec>10000 & salespriceamount_rec<10000000 & transmid11 == 1
gen condition_2 = 0 
replace condition_2 = 1 if salespriceamount_rec>10000 & salespriceamount_rec<10000000 & transmid11 == 1

// Create empty placeholders to store column names
local cols_for_depvar_1_minority = " "
local cols_for_depvar_1_categories = " "
local cols_for_depvar_2_minority = " "
local cols_for_depvar_2_categories = " "


save "temp_data_table14_formatted.dta", replace

forvalues d = 1/2 {

    forvalues cols = 1/4 {
        // SET RACIAL MINORITY VARIABLE FOR THIS COLUMN
        // In columns 1 and 2 we use the original specification for racial minority which takes as its reference group all 'white' and 'other' participants in the study
        if inlist(`cols',1,2) {
            local racial_minority = "ofcolor"
        }
        // In columns 3 and 4 we use our fixed specification for racial minority which takes only 'white' participants as its reference group, includes 'other' as a separate category
        else if inlist(`cols',3,4) {
            local racial_minority = "ofcolor othrace"
        }

        // SET CITY FIXED EFFECT FOR THIS COLUMN
        // In columns 1 and 3 we use the original city name column which includes many duplications and misspellings of single cities, representing them as multiple fixed effects
        if inlist(`cols',1,3) {
            local geofe = "hcity"
        }
        // In columns 2 and 4 we use our corrected city names from our string matching algorithm in data_cleaner.do
        // Forgive the variable name, temp_city is not very intuitive, but is in fact the final corrected city name to be generated by data_cleaner.do
        else {
            local geofe = "temp_city"
        }

        // Print the current specification of the model
        disp as text "Dep. Var. is: " as result "`dependent_var_`d''" 
        disp as text "Racial Minority specification is: " as result "`racial_minority'"
        disp as text "City Fixed Effect is: " as result "`geofe'"
        disp as text "Clustered by: control (a variable representing the trial)"

        // ESTIMATE MODELS
		
        // Estimate the 'racial minority' regression for this column
        reghdfe `dependent_var_`d'' `racial_minority' `CONTROL_VARS' `control_var_`d'' if condition_`d', absorb(`ABS_VARS' `geofe') keepsingle cluster(control)
        qui eststo dep_var_`d'_col_`cols'_minority
		
        // Make list of column object names to combine into one plot later
        local cols_for_depvar_`d'_minority = " `cols_for_depvar_`d'_minority' dep_var_`d'_col_`cols'_minority "
    
        // Estimate the 'racial category' regression for this column
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
    using "${OUTPUT}/table14_dep_var_`d'_minority.tex", ///
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
    using "${OUTPUT}/table14_dep_var_`d'_minority.csv", ///
    replace csv label ///
    mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name & Correct Race", pattern(1 1 1 1)) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2_a, ///
    labels("Observations" "Adjusted R^2")) ///
    keep(`racial_minority')
	
	
    // Output the Latex table for the racial categories analyses
    esttab `cols_for_depvar_`d'_categories' ///
    using "${OUTPUT}/table14_dep_var_`d'_categories.tex", ///
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
    using "${OUTPUT}/table14_dep_var_`d'_categories.csv", ///
    replace csv label ///
    mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name & Correct Race", pattern(1 1 1 1)) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2_a, ///
    labels("Observations" "Adjusted R^2")) ///
    keep(2.apracex 3.apracex 4.apracex 5.apracex)
}
