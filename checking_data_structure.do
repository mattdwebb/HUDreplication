clear all

// Set paths, open log and install packages

    /* Set path to the parent folder of the local location of the git repository */
    global PATH "/Users/anthony/Library/CloudStorage/OneDrive-UniversityofToronto/Research/Replication Games"

    global CODE "${PATH}/cities-from-geoid" //set the file path to the main code directory
    global DATA "${CODE}/Data" // set the file path to the data subdirectory

    global OUTPUT "${CODE}/Data/Generated" // set the output file path
    
    cap log close
    log using "${OUTPUT}/checking_data_structure_log.txt", text replace
    
    local PKG "egenmore strgroup matchit freqindex reghdfe estout ftools"
    foreach var in `PKG' {
        cap which `var'
        if _rc!=0 {
            ssc install `var'
        }
    }
    
    set more off


// This is a variant of the process_data function in the table_generation_function.do file
// Besides from OUTPUT being set differently above; it also saves as a csv
// There are no other differences between the two functions
capture program drop process_data_csv
program define process_data_csv
    args data_file force_clean corrected
    local cleaned_file = subinstr("`data_file'", ".csv", "_cleaned.csv", .)

    if "`force_clean'" != "1" {
        capture confirm file "${OUTPUT}/`cleaned_file'"
        if !_rc {
            import delimited "${OUTPUT}/`cleaned_file'", clear
            display "Using existing cleaned data file: ${OUTPUT}/`cleaned_file'"
            exit
        }
    }
    // import data
    import delimited "${DATA}/Generated/`data_file'", clear
    gen id = _n
    
    display "imported ${DATA}/Generated/`data_file'"

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
    
    // Save as CSV
    export delimited using "${OUTPUT}/`cleaned_file'", replace
end

// Process HUD data files
process_data_csv "HUDprocessed_census_correct_cities.csv", 1, "corrected"
display "Processed census data"

process_data_csv "HUDprocessed_names_correct_cities.csv", 1, "corrected"
display "Processed names data"

process_data_csv "HUDprocessed_testscores_correct_cities.csv", 1, "corrected"
display "Processed testscores data"

// Close the log file
log close
