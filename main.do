clear all

// Set paths, open log and install packages

	/* Set path to the parent folder of the local location of the git repository */
	global PATH "PATH/TO/PARENT/FOLDER/FOR/THIS/REPOSITORY"

	global CODE "${PATH}/HUDreplication" //set the file path to the main code directory
	global DATA "${CODE}/Data" // set the file path to the data subdirectory

	cap mkdir "${CODE}/Output" // make an Output folder if it doesn't already exist
	global OUTPUT "${CODE}/Output" // set the output file path
	
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

// Run files for generating tables 5, 7 and 10
	do "${CODE}/table5.do"
	do "${CODE}/table6.do"
	do "${CODE}/table7.do"
	do "${CODE}/table10.do"
	do "${CODE}/table11.do"
	do "${CODE}/table13.do"
