/* City Name Cleaner File HUD Replication */
/* Written by: Matthew D. Webb, Areez Gangji and Anthony McCanny  */
/* Date: September 4, 2023  */

// Section 1: Clean up zip codes and merge data
	// Clean up zip codes
	gen lenzip = strlen(hzip) // Generate length of zip code
	gen zip = substr(hzip,1,5) // Clean up 9/10 digit zips
	destring zip, replace force // Convert string to numeric to match other file

	// Merge city data
	merge m:1 zip using "${DATA}/zipinfo.dta" // Merge with zip data
	drop if _merge==2 // Drop unmatched entries from zip file
	drop _merge
	drop allaccept17-allaccept31 // Drop unneeded alternatives

	// Merge county data
	merge m:1 zip using "${DATA}/zipinfo-county.dta" // Merge with county data matched to zip code
	drop if _merge==2 // Drop unmatched entries from zip file
	drop _merge

	// Rename hcityx column to hcity to standardize across different data files
	capture rename hcityx hcity

	// Generate city name flag
	gen good_city = 0 // Initialize flag for whether city name is good

// Section 2: Clean up the strings
	// Generate uppercase values of hcity and primary_city
	// These will be used later to match on subsections of the string
	gen upper_hcity = upper(hcity)
    gen upper_primary = upper(primary_city)

	// Create a new function to clean city names
	// Drop the existing program 'clean_city' if it exists to avoid conflicts
	cap program drop clean_city
	
	// Define a new program 'clean_city'
	program define clean_city
		args varname newvarname
        gen `newvarname' = `varname'
		
		// Loop over a set of characters
		foreach char in " " "." "-" "'" "," "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "\\" "," ":" "#" "$" "%" "[" "]" "(" ")" char(34) {
			// For each character, replace its occurrence in 'varname' with an empty string
			// This effectively removes the character from 'varname'
			replace `newvarname' = subinstr(`newvarname', "`char'", "", .)
		}
        // Set 'newvarname' to all upper case
        replace `newvarname' = upper(`newvarname')
	end
    
    clean_city hcity clean_hcity // Clean city name
    clean_city primary_city clean_primary // Clean primary city name
    clean_city officialuspscityname clean_usps // Clean USPS city name

    replace good_city = 1 if clean_hcity == clean_primary // Flag city name as good if it matches primary city
	replace good_city = 1 if clean_hcity == clean_usps // Flag city name as good if it matches USPS city name

	// Loop over all the acceptable cities
	forvalues i = 1 /16 {
		clean_city allaccept`i' clean_allaccept`i'
		replace good_city = 1 if clean_hcity == clean_allaccept`i'
	}

	// Loop over all the acceptable counties
	forvalues i = 1 /6 {
		clean_city allcounty`i' clean_allcounty`i'
		replace good_city = 1 if clean_hcity == clean_allcounty`i'
	}

	// If two hcities share a name, and one is flagged good, then all should be
	bysort clean_hcity: egen mean_good = mean(good_city)
	replace good_city = 1 if mean_good != 0

	// Create the final city
	gen final_city = "."
	replace final_city = clean_hcity if good_city==1

	// Tries to match on splits of the typed city name and the primary city name
	split upper_hcity, parse(" ") generate(split_hcity)

	forvalues i = 1/4 {
		capture confirm variable split_hcity`i'
		if _rc == 0 {
			replace final_city =clean_primary if (split_hcity`i' == clean_primary) & (good_city == 0) & (upper_primary != "")
			replace good_city = 1 if (split_hcity`i' == clean_primary) & (upper_primary != "")
		}
	}

// Section 3: Fuzzy matching approach

	// Unique items
	matchit clean_hcity clean_primary, similmethod(soundex_ext) weights(log)
	// Replace final_city with clean_primary if similscore is greater than 0 and final_city is "."
	replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
	// Set good_city to 1 if a fuzzy match has been found
	replace good_city = 1 if similscore > 0
	drop similscore

	// Find the modal string for each string group

	preserve
	bysort clean_hcity: gen count_city = _n
	// Keep only the first instance of each city name
	keep if count_city ==1
	sort market
	// Group clean_hcity by market with a threshold of 0.25
	by market: strgroup clean_hcity, gen(grouped_hcity) threshold(0.25) first 

	// Generate frequency of the groups
	bysort grouped_hcity: egen group_freq = count(grouped_hcity)
	keep clean_hcity grouped_hcity group_freq
	// Save the data to a temporary file
	tempfile temp
	save "`temp'", replace 
	restore
	// Merge by clean_hcity using the temporary file
	merge m:1 clean_hcity using "`temp'"
	drop _merge
		
	gen grouping_hcity = .
	// Replace grouping_hcity with grouped_hcity if group_freq is greater than or equal to 2
	replace grouping_hcity = grouped_hcity if group_freq>=2
	// Find the mode of clean_hcity by grouping_hcity
	bysort grouping_hcity: egen group_mode = mode(clean_hcity) 
	// Replace final_city with group_mode if group_freq is greater than or equal to 2 and final_city is "."
	replace final_city = group_mode if (group_freq >=2) & (final_city == ".")
	// Set good_city to 1 if group_freq is greater than or equal to 2
	replace good_city=1 if group_freq >=2
		
	// Loop matchit through other accepts
	forvalues i = 1 /4{
		matchit clean_hcity clean_allaccept`i', similmethod(soundex_ext) weights(log)
		// Replace final_city with clean_primary if similscore is greater than 0 and final_city is "."
		replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
		// Set good_city to 1 if similscore is greater than 0
		replace good_city = 1 if similscore > 0
		// Drop similscore
		drop similscore
	}


// Section 4: Manual changes
    // Create a list of pairs of strings to replace with the format "ORIGINAL_VALUE>NEW_VALUE"
	local manual_changes "CAROLLTON>CARROLLTON " ///
	"SMRYNA>SMYRNA " ///
	"ANN>ANNAPOLIS " ///
	"COLUMBIAMD>COLUMBIA " ///
	"NBELAIR>BELAIR " ///
	"MIDDLEBURGHTS>MIDDLEBURGHEIGHTS " ///
	"MORTHRIDGEVILLE>NORTHRIDGEVILLE " ///
	"WOODBRIGE>WOODBRIDGE " ///
	"LEWSIVILLE>LEWISVILLE " ///
	"SACSHE>SACHSE " ///
	"MACOMBTWP>MACOMB " ///
	"MACOMBTOWNSHIP>MACOMB " ///
	"SANANTONIOTX>SANANTONIO " ///
	"NORTHRICHLANDS>NORTHRICHLANDHILLS " ///
	"NRICHLANDHILLS>NORTHRICHLANDHILLS " ///
	"HOUS>HOUSTON " ///
	"HUOSTON>HOUSTON " ///
	"BLETON>BELTON " ///
	"OVERLAND>OVERLANDPARK " ///
	"NORTHLONGBEACH>LONGBEACH " ///
	"EASTAMWELL>EASTAMWELLTWP " ///
	"GLENBROOKE>GREENBROOK " ///
	"MONJUNCTION>MONMOUTHJUNCTION " ///
	"NORTHEDISON>EDISON " ///
	"SOUTHEDISON>EDISON " ///
	"WOODBRIDGEPROPER>WOODBRIDGE " ///
	"BLOOMFIELDTWP>BLOOMFIELD " ///
	"BOONTONTOWN>BOONTON " ///
	"CALDWELLBOROTWP>CALDWELL " ///
	"CHATHAMBOROUGH>CHATHAM " ///
	"CHATHAMTOWNSHIP>CHATHAM " ///
	"HANOVERTWP>HANOVERTOWNSHIP " ///
	"MADISONBOROUGH>MADISON " ///
	"MORRISTOWNSHIP>MORRISTOWN " ///
	"MORRISTWP>MORRISTOWN " ///
	"MOUNTOLIVETWP>MOUNTOLIVE " ///
	"MTOLIVE>MOUNTOLIVE " ///
	"MTOLIVETWP>MOUNTOLIVE " ///
	"PARSIPPANYTROYHILLS>PARSIPPANY " ///
	"BAYRIDGEBROOKLYN>BROOKLYN " ///
	"BKLYN>BROOKLYN " ///
	"FLUSHINGQUEENS>QUEENS " ///
	"ARDMOOR>ARDMORE " ///
	"HAVERTOWNTOWNSHIP>HAVERTOWN " ///
	"PROSPECTPARKBORO>PROSPECTPARK " ///
	"WARRINGTONLANE>WARRINGTON " ///
	"WARRINGTONTWP>WARRINGTON " ///
	"CHESTEFILED>CHESTERFIELD " ///
	"DELMARHEIGHTS>DELMAR " ///
	"SOUTHSANJOSE>SANJOSE " ///
	"CLEARWATERBEACH>CLEARWATER " ///
	"PALMHUDSON>PALMHARBOR " ///
	"REDINGTONSHORES>REDINGTONBEACH " ///
	"HILLSBORO>HILLSBOROUGH " ///
	"THORTON>THORNTON " ///
	"NBELAIR>BELAIR " ///
	"NARBETH>NARBERTH " ///
	"MILBURY>MILLBURY " ///
	"CARROLTON>CARROLLTON " ///
	"MTAIRY>MOUNTAIRY " ///
	"NORTHEAST>WASHINGTON " ///
	"GREENBELY>GREENBELT " ///
	"CORNITH>CORINTH " ///
	"SLEEPHOLLOW>SLEEPYHOLLOW " ///
	"BRETENAHL>BRATENAHL " ///
	"WOODLANDS>THEWOODLANDS " ///
	"VALLEY>FOUNTAINVALLEY " ///
	"BEACONHILL>BOSTON " ///
	"TANEYTOWN>WESTMINSTER " ///
	"UNIVERSITYPARK>DALLAS " ///
	"NORTHKANSASCITY>KANSASCITY " ///
	"ROXBURRY>ROXBURY " ///
	"RAMBLINGHILL>EASTBRUNSWICK " ///
	"BERNARDSTWP>BASKINGRIDGE " ///
	"MTTABOR>TABOR " ///
	"IVESESTATES>MIAMI " ///
	"KENDALL>MIAMI " ///
	"NOLMSTEAD>NORTHOLMSTED " ///
	"BUMPASS>BUMPASS " ///
	"ROXBURY>ROXBURY " ///
	"REINTERSTOWN>REISTERSTOWN " ///
	"GROSSPOINTEWOODS>GROSSEPOINTEWOODS " ///
	"MELBANE>MEBANE " ///
	"RANDELMAN>RANDLEMAN " ///
	"PARSIPPANYTROYHILLSTWP>PARSIPPANY " ///
	"AUDOBON>AUDUBON " ///
	"ROYERSFORDPA>ROYERSFORD"
	


    // Loop over each pair in the list and manually change the city name
	foreach pair in `manual_changes' {
		local pair = subinstr("`pair'", ">", " ", .)
		local old_city = word("`pair'", 1)
		local new_city = word("`pair'", 2)
		replace final_city = "`new_city'" if clean_hcity == "`old_city'"
		replace good_city = 1 if clean_hcity == "`old_city'"
	}

	// Declare the list of manual changes to city names due to zip codes
	local zip_manual_changes "87111>ALBUQUERQUE " ///
	"92284>YUCCAVALLEY " ///
	"33614>TAMPA " ///
	"33511>BRANDON " ///
	"92344>HESPERIA " ///
	"33777>SEMINOLE " ///
	"10003>MANHATTAN " ///
	"92660>NEWPORTBEACH"


	disp "`zip_manual_changes'"
	
	// Loop over each pair in the list and manually change the city name based on the zip code
	foreach pair in `zip_manual_changes' {
		local pair = subinstr("`pair'", ">", " ", .)
		local old_zip = word("`pair'", 1)
		local new_city = word("`pair'", 2)
		replace final_city = "`new_city'" if (hzip == "`old_zip'")
		replace good_city = 1 if hzip == "`old_zip'"
	}
	
	// Change "NEWYORK" to "MANHATTAN", as by manual inspection all "New York" entries come from Manhattan
	// We narrow the city to the borough for consistency with the rest of the code and 
	// because the size of the boroughs and the difference in housing markets suggests they are 
	// appropriate units for analysis
	replace final_city = "MANHATTAN" if clean_hcity == "NEWYORK"
	replace final_city = "MANHATTAN" if clean_hcity == "NEWYORKCITY"
	replace final_city = "MANHATTAN" if final_city == "NEWYORK"
	replace good_city = 1 if clean_hcity == "NEWYORK"
	replace good_city = 1 if clean_hcity == "NEWYORKCITY"

// Generate city names to serve as fixed effects in the regression, final_city if good_city==1, otherwise clean_hcity
	gen temp_city = ""
	replace temp_city = final_city if good_city==1
	replace temp_city = clean_hcity if good_city==0

// Drop new variables
	capture drop zip decommissioned primary_city acceptable_cities unacceptable_cities county timezone area_codes world_region country irs_estimated_population allaccept1 allaccept2 allaccept3 allaccept4 allaccept5 allaccept6 allaccept7 allaccept8 allaccept9 allaccept10 allaccept11 allaccept12 allaccept13 allaccept14 allaccept15 allaccept16 officialuspscityname officialuspsstatecode officialstatename officialcountyname allcounty1 allcounty2 allcounty3 allcounty4 allcounty5 allcounty6 upper_hcity upper_primary clean_primary clean_usps clean_allaccept1 clean_allaccept2 clean_allaccept3 clean_allaccept4 clean_allaccept5 clean_allaccept6 clean_allaccept7 clean_allaccept8 clean_allaccept9 clean_allaccept10 clean_allaccept11 clean_allaccept12 clean_allaccept13 clean_allaccept14 clean_allaccept15 clean_allaccept16 clean_allaccept14 clean_allcounty1 clean_allcounty2 clean_allcounty3 clean_allcounty4 clean_allcounty5 clean_allcounty6 split_hcity1 split_hcity2 split_hcity3 split_hcity4 grouped_hcity group_freq grouping_hcity group_mode
	compress

