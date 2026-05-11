clear all

// Set paths, open log and install packages

	/* Set path to the local location of the HUDreplication repository */
	global REPO_ROOT "/PATH/TO/HUDreplication"

	global CODE "${REPO_ROOT}/selected_sample" // set the file path to the selected-sample analysis directory
	global DATA "${REPO_ROOT}/Data" // set the file path to the data subdirectory

	cap mkdir "${CODE}/Output" // make an Output folder if it doesn't already exist
	global OUTPUT "${CODE}/Output" // set the output file path

	// Rebuild all cleaned Stata inputs from the current generated CSV files.
	global FORCE_CLEAN 1
	
	cap log close
	log using "${OUTPUT}/HUDreplication_log.txt", text replace
	
	local PKG "egenmore strgroup matchit freqindex reghdfe estout ftools"
	foreach var in `PKG' {
		cap which `var'
		if _rc!=0 {
			ssc install `var'
		}
	}
	
	set more off

// Run unified comparison tables driver
	do "${CODE}/comparison_tables.do"


// generate full replication tables as features in Appendix B
	do "${CODE}/appendix_tables.do"

// create meta analysis figures
//	do "${CODE}/meta_analysis.do"
