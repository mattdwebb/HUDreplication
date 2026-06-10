clear all

// Set paths, open log and install packages

	/* Set path to the local location of the HUDreplication repository */
	global REPO_ROOT "/PATH/TO/HUDreplication"

	global CODE "${REPO_ROOT}/ct_sample" // set the file path to the C&T-sample analysis directory
	global DATA "${REPO_ROOT}/Data" // set the file path to the data subdirectory

	cap mkdir "${CODE}/Output" // make an Output folder if it doesn't already exist
	global BASE_OUTPUT "${CODE}/Output" // set the base output file path
	cap mkdir "${BASE_OUTPUT}/comparison_table_estimates"
	cap mkdir "${BASE_OUTPUT}/corrected"
	cap mkdir "${BASE_OUTPUT}/original"
	global OUTPUT "${BASE_OUTPUT}"

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
	global OUTPUT "${BASE_OUTPUT}/comparison_table_estimates"
	do "${CODE}/comparison_tables.do"


// generate full replication tables as features in Appendix B
	global OUTPUT "${BASE_OUTPUT}"
	global APPENDIX_OUTPUT_ROOT "${BASE_OUTPUT}"
	do "${CODE}/appendix_tables.do"
	macro drop APPENDIX_OUTPUT_ROOT

// create meta analysis figures
//	do "${CODE}/meta_analysis.do"
