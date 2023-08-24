clear

/*set path here*/

global PATH "C:\Users\sunny\OneDrive\Desktop\Replication Games"

*import delimited "${PATH}csv_cleaned\table6.csv"


import delimited "${PATH}\csvs\table10_2_mom.csv", bindquote(strict)

/*turn log on*/
	cap log close
	//log using "${PATH}log\cleaner_t10_2.txt", text replace


/*want to match on hzip_rec*/
/*we need to clean hcity*/

/*requires four packages*/

*ssc install egenmore
*ssc install strgroup
*ssc install matchit
*ssc install freqindex
/*----------------------------------*/
/*starting point*/
/*
 there are 1,912 zip codes in the estimation sample
 there are 1688 cities in the raw data
*/

/*run the regression, keep the estimation sample only*/
	*reghdfe w2012pc_rec ofcolor, absorb(control sequencexx monthx hcity market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx) keepsingle cluster(control)
	
	/*commented out for now, for matching purposes*/
	*keep if e(sample)==1

/*clean up zip*/

	gen lenzip = strlen(hzip)

	*there are some 9/10 digit zips, clean those up
	gen zip = substr(hzip,1,5)
	
	*make the string numeric to match other file

	destring zip, replace force

/*merging city*/
	merge m:1 zip using "${PATH}\Cleaner\zipinfo.dta"
	
	*drop the not matched from zip file
	
	drop if _merge==2
	
	drop _merge
	
	/*drop the unneeded alternatives*/
	
	drop allaccept17-allaccept31
	

/*merging county*/
	merge m:1 zip using "${PATH}\Cleaner\zipinfo-county.dta"
	
	*drop the not matched from zip file
	
	drop if _merge==2
	
	drop _merge
	

/*generate a flag for whether or not the name is good*/

	gen good_city = 0
	
/*does the city match the acceptable city name*/
	replace good_city = 1 if hcityx == primary_city
	replace good_city = 1 if hcityx == officialuspscityname
	
	/*15,427  changes made*/
	
/*loop over all the acceptable cities and counties*/
	
	forvalues i = 1 /16{
		
		replace good_city = 1 if hcityx == 	allaccept`i'
	}

	forvalues i = 1 /6{
		
		replace good_city = 1 if hcityx == 	allcounty`i'
	}

	/*only the first made any changes, there were 692*/
	/*county matches another 219 */
	/*5,760 obs have a flag value of 0 */
	
	
/*now let's try to clean the strings*/

	gen upper_hcity = upper(hcityx)
	gen upper_primary = upper(primary_city)
	gen upper_usps = upper(officialuspscityname)

	replace good_city = 1 if upper_hcity == upper_primary
	replace good_city = 1 if upper_hcity == upper_usps
	
	/*this matched another 2,123 cities*/
	/*counties did not match any*/
		/*down to 1,323 unique cities*/
	
	/*loop over all the acceptable cities*/
		forvalues i = 1 /16{
			gen upper_allaccept`i' = upper(allaccept`i')
			replace good_city = 1 if upper_hcity == 	upper_allaccept`i'
		}
		
		forvalues i = 1 /6{
			gen upper_allcounty`i' = upper(allcounty`i')
			replace good_city = 1 if upper_hcity == 	upper_allcounty`i'
		}
		
		/*the first was the only one that matched*/
		
		
	/*delete spaces and periods*/
	
	gen clean_hcity = subinstr(upper_hcity," ","",100000)
	replace clean_hcity = subinstr(clean_hcity, ".","",10000)
	replace clean_hcity = subinstr(clean_hcity, "-","",10000)
	replace clean_hcity = subinstr(clean_hcity, "'","",10000)
	replace clean_hcity = subinstr(clean_hcity, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_hcity = subinstr(clean_hcity,"`i'", "", 10000)
	}
	
	gen clean_primary = subinstr(upper_primary," ","",100000)
	replace clean_primary = subinstr(clean_primary, ".","",10000)
	replace clean_primary = subinstr(clean_primary, "-","",10000)
	replace clean_primary = subinstr(clean_primary, "'","",10000)
	replace clean_primary = subinstr(clean_primary, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_primary = subinstr(clean_primary,"`i'", "", 10000)
	}
	
	gen clean_usps = subinstr(upper_usps," ","",100000)
	replace clean_usps = subinstr(clean_usps, ".","",10000)
	replace clean_usps = subinstr(clean_usps, "-","",10000)
	replace clean_usps = subinstr(clean_usps, "'","",10000)
	replace clean_usps = subinstr(clean_usps, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_usps = subinstr(clean_usps,"`i'", "", 10000)
	}
	
	replace good_city = 1 if clean_hcity == clean_primary
	replace good_city = 1 if clean_hcity == clean_usps
	
	/*this matched just 96 cities*/
	/* county did not match any extras */
		
	/*loop over all the acceptable cities*/
		forvalues i = 1 /16{
		
			gen clean_allaccept`i' = subinstr(upper_allaccept`i'," ", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',".", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',"-", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',"'", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',",", "", 10000)
			
			forvalues j = 0/9 {
				replace clean_allaccept`i' = subinstr(clean_allaccept`i',"`j'", "", 10000)
			}

			replace good_city = 1 if clean_hcity == clean_allaccept`i'
		
		}
		
		forvalues i = 1 /6{
		
			gen clean_allcounty`i' = subinstr(upper_allcounty`i'," ", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',".", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',"-", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',"'", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',",", "", 10000)
			
			forvalues j = 0/9 {
				replace clean_allcounty`i' = subinstr(clean_allcounty`i',"`j'", "", 10000)
			}

			replace good_city = 1 if clean_hcity == clean_allcounty`i'
		}
		
		
		/*this matched 892 more obs, down to 2,250 with a bad flag*/
			/*unique cities at 1,280*/
	
	/*clean up missing zip code ones*/
		/*if two hcities share a name, and one is flagged good, then all should be*/
		
	bysort clean_hcity: egen mean_good = mean(good_city)
	replace good_city = 1 if mean_good != 0
		/*1,267 more with good flag*/

	

	
/*create the final city*/

	gen final_city = "."
	
	replace final_city = clean_hcity if good_city==1
	
	

/*try some more string groups using zip codes*/	

	gen strzip = zip
	tostring strzip, replace force
	
	gen ziplength = strlen(strzip)
	
	replace strzip = "0" + strzip if ziplength==4
	
	gen zipfour = substr(strzip,1,4)
	/*
		sort zipfour
	
		by zipfour: strgroup clean_hcity, gen(group_zip) threshold(0.75) first
	
		bysort group_zip: egen group_freq_zip = count(group_zip)

	*/

/* tries to match on splits of the typed city name and the primary city name */
	
	split upper_hcity, parse(" ") generate(split_hcity)
	
	replace final_city = clean_primary if (split_hcity1 == clean_primary) & (good_city == 0)
	replace good_city = 1 if split_hcity1 == (clean_primary)
	/* fixes 196 more, 787 left */
	

	forvalues i = 1/3 {
		replace final_city =clean_primary if (split_hcity`i' == clean_primary) & (good_city == 0) & (upper_primary != "")
		replace good_city = 1 if (split_hcity`i' == clean_primary) & (upper_primary != "")
	}
/* fixes 20 more, 767 to go, 1,036 unique cities */
/*now let's try the fuzzy matching approach*/
/*unique items*/
	matchit clean_hcity clean_primary, similmethod(soundex_ext) weights(log)
	replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
	replace good_city = 1 if similscore > 0
	drop similscore
	

	/*find the modal string for each string group*/
	
	/*339 real changes, 428 left,*/
	
	preserve
	
	bysort clean_hcity: gen count_city = _n
	
	keep if count_city ==1
	
	sort market
	
	by market: strgroup clean_hcity, gen(grouped_hcity) threshold(0.25) first 
	
	/*generate frequency of the groups*/
	bysort grouped_hcity: egen group_freq = count(grouped_hcity)
	
	keep clean_hcity grouped_hcity group_freq
	
	save "${PATH}\Cleaner\uniquehcity_t10_2.dta", replace 
	
	restore
	
	merge m:1 clean_hcity using  "${PATH}\Cleaner\uniquehcity_t10_2.dta"
	
	drop _merge

	
	/*find the modal string for each string group*/
	gen grouping_hcity = .
	replace grouping_hcity = grouped_hcity if group_freq>=2
	bysort grouping_hcity: egen group_mode = mode(clean_hcity) 
	
	replace final_city = group_mode if (group_freq >=2) & (final_city == ".")
	replace good_city=1 if group_freq >=2
	
	/* 97 changes, 331 left */
	
	/* loop matchit through other accepts */
	forvalues i = 1 /3{
			matchit clean_hcity clean_allaccept`i', similmethod(soundex_ext) weights(log)
			replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
			replace good_city = 1 if similscore > 0
			drop similscore
		}
	
	/* 47 changes,  284 left */

	
/*manual inspections by market*/

	levelsof market, local(mklist)
	
	foreach mkt in `mklist' {
		
		disp "market is `mkt'"
		tab clean_hcity good_city if market == "`mkt'" 
	}

/* tries to match on splits of the typed city name and the primary city name */
	
	/*manual changes*/
	
	replace final_city = "ALBUQUERQUE" if  clean_hcity == "87111"
	replace good_city = 1 if  clean_hcity == "87111"
	
	replace final_city = "CARROLLTON" if  clean_hcity == "CAROLLTON"
	replace good_city = 1 if  clean_hcity == "CAROLLTON" 
	
	replace final_city = "SMYRNA" if  clean_hcity == "SMRYNA"
	replace good_city = 1 if  clean_hcity == "SMRYNA" 
	
	replace final_city = "ANNAPOLIS" if  clean_hcity == "ANN"
	replace good_city = 1 if  clean_hcity == "ANN" 
	
	 replace final_city = "COLUMBIA" if  clean_hcity == "COLUMBIAMD"
	replace good_city = 1 if  clean_hcity == "COLUMBIAMD" 
	
	replace final_city = "BELAIR " if  clean_hcity == " NBELAIR"
	replace good_city = 1 if  clean_hcity == " NBELAIR" 
	
	replace final_city = "MIDDLEBURGHEIGHTS" if  clean_hcity == "MIDDLEBURGHTS"
	replace good_city = 1 if  clean_hcity == "MIDDLEBURGHTS" 
	
	replace final_city = "NORTHRIDGEVILLE" if  clean_hcity == "MORTHRIDGEVILLE"
	replace good_city = 1 if  clean_hcity == "MORTHRIDGEVILLE"
	
	replace final_city = "WOODBRIDGE" if  clean_hcity == "WOODBRIGE"
	replace good_city = 1 if  clean_hcity == "WOODBRIGE" 
	
	replace final_city = "LEWISVILLE" if  clean_hcity == "LEWSIVILLE"
	replace good_city = 1 if  clean_hcity == "LEWSIVILLE" 
	
	replace final_city = "SACHSE" if  clean_hcity == "SACSHE"
	replace good_city = 1 if  clean_hcity == "SACSHE" 
	
	replace final_city = "MACOMB" if  clean_hcity == "MACOMBTWP"
	replace good_city = 1 if  clean_hcity == "MACOMBTWP" 
	
	replace final_city = "MACOMB" if  clean_hcity == "MACOMBTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "MACOMBTOWNSHIP" 
	
	replace final_city = "NORTHRICHLANDHILLS" if  clean_hcity == "NORTHRICHLANDS"
	replace good_city = 1 if  clean_hcity == "NORTHRICHLANDS" 

	replace final_city = "NORTHRICHLANDHILLS" if  clean_hcity == "NRICHLANDHILLS"
	replace good_city = 1 if  clean_hcity == "NRICHLANDHILLS" 

	replace final_city = "HOUSTON" if  clean_hcity == "HOUS"
	replace good_city = 1 if  clean_hcity == "HOUS"
	
	replace final_city = "HOUSTON" if  clean_hcity == "HUOSTON"
	replace good_city = 1 if  clean_hcity == "HUOSTON"
	
	replace final_city = "BELTON" if  clean_hcity == "BLETON"
	replace good_city = 1 if  clean_hcity == "BLETON" 
	
	replace final_city = "OVERLANDPARK" if  clean_hcity == "OVERLAND"
	replace good_city = 1 if  clean_hcity == "OVERLAND" 
	
	replace final_city = "LONGBEACH" if  clean_hcity == "NORTHLONGBEACH"
	replace good_city = 1 if  clean_hcity == "NORTHLONGBEACH" 
	
	replace final_city = "EASTAMWELLTWP" if  clean_hcity == "EASTAMWELL"
	replace good_city = 1 if  clean_hcity == "EASTAMWELL" 
	
	replace final_city = "GREENBROOK" if  clean_hcity == "GLENBROOKE"
	replace good_city = 1 if  clean_hcity == "GLENBROOKE" 
	
	replace final_city = "MONMOUTHJUNCTION" if  clean_hcity == "MONJUNCTION"
	replace good_city = 1 if  clean_hcity == "MONJUNCTION" 
	
	replace final_city = "EDISON" if  clean_hcity == "NORTHEDISON"
	replace good_city = 1 if  clean_hcity == "NORTHEDISON" 
	
	replace final_city = "EDISON" if  clean_hcity == "SOUTHEDISON"
	replace good_city = 1 if  clean_hcity == "SOUTHEDISON" 
	
	replace final_city = "WOODBRIDGE" if  clean_hcity == "WOODBRIDGEPROPER"
	replace good_city = 1 if  clean_hcity == "WOODBRIDGEPROPER"
	
	replace final_city = "BLOOMFIELD" if  clean_hcity == "BLOOMFIELDTWP"
	replace good_city = 1 if  clean_hcity == "BLOOMFIELDTWP"
 	
	replace final_city = "BOONTON" if  clean_hcity == "BOONTONTOWN"
	replace good_city = 1 if  clean_hcity == "BOONTONTOWN" 	
	
	replace final_city = "CALDWELL" if  clean_hcity == "CALDWELLBOROTWP"
	replace good_city = 1 if  clean_hcity == "CALDWELLBOROTWP" 
	
	replace final_city = "CHATHAM" if  clean_hcity == "CHATHAMBOROUGH"
	replace good_city = 1 if  clean_hcity == "CHATHAMBOROUGH"
	
	replace final_city = "CHATHAM" if  clean_hcity == "CHATHAMTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "CHATHAMTOWNSHIP"
	
	replace final_city = "HANOVERTOWNSHIP" if  clean_hcity == "HANOVERTWP"
	replace good_city = 1 if  clean_hcity == "HANOVERTWP"
	
	replace final_city = "MADISON" if  clean_hcity == "MADISONBOROUGH"
	replace good_city = 1 if  clean_hcity == "MADISONBOROUGH" 
	
	replace final_city = "MORRISTOWN" if  clean_hcity == "MORRISTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "MORRISTOWNSHIP" 
	
	replace final_city = "MORRISTOWN" if  clean_hcity == "MORRISTWP"
	
	replace final_city = "MOUNTOLIVE" if  clean_hcity == "MOUNTOLIVETWP"
	replace good_city = 1 if  clean_hcity == "MOUNTOLIVETWP" 
	
	replace final_city = "MOUNTOLIVE" if  clean_hcity == "MTOLIVE"
	replace good_city = 1 if  clean_hcity == "MTOLIVE" 
	
	replace final_city = "MOUNTOLIVE" if  clean_hcity == "MTOLIVETWP"
	replace good_city = 1 if  clean_hcity == "MTOLIVETWP" 
	
	replace final_city = "PARSIPPANY" if  clean_hcity == "PARSIPPANY-TROYHILLS"
	replace good_city = 1 if  clean_hcity == "PARSIPPANYTROYHILLS" 
	
	replace final_city = "PARSIPPANY" if  clean_hcity == "PARSIPPANY-TROYHILLS"
	
	replace final_city = "BROOKLYN" if  clean_hcity == "BAYRIDGE,BROOKLYN "
	replace good_city = 1 if  clean_hcity == "BAYRIDGE,BROOKLYN " 	
	
	replace final_city = "BROOKLYN" if  clean_hcity == "BKLYN"
	replace good_city = 1 if clean_hcity == "BKLYN" 	
	
	replace final_city = "FLUSHING" if  clean_hcity == "FLUSHINGQUEENS"
	replace good_city = 1 if  clean_hcity == "FLUSHINGQUEENS" 

	replace final_city = "ARDMORE" if  clean_hcity == "ARDMOOR"
	replace good_city = 1 if  clean_hcity == "ARDMOOR"
	
	replace final_city = "HAVERTOWN" if  clean_hcity == "HAVERTOWNTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "HAVERTOWNTOWNSHIP" 
	
	replace final_city = "PROSPECTPARK" if  clean_hcity == "PROSPECTPARKBORO"
	replace good_city = 1 if  clean_hcity == "PROSPECTPARKBORO" 
	
	replace final_city = "WARRINGTON" if  clean_hcity == "WARRINGTONLANE"
	replace good_city = 1 if  clean_hcity == "WARRINGTONLANE" 	
	
	replace final_city = "WARRINGTON" if  clean_hcity == "WARRINGTONTWP"
	replace good_city = 1 if  clean_hcity == "WARRINGTONTWP"

	replace final_city = "CHESTERFIELD" if  clean_hcity == "CHESTEFILED"
	replace good_city = 1 if  clean_hcity == "CHESTEFILED" 
	
	replace final_city = "YUCCAVALLEY" if  clean_hcity == "92284"
	replace good_city = 1 if  clean_hcity == "92284" 
	 	
	replace final_city = "SANANTONIO" if  clean_hcity == "SANANTONIO,TX"
	replace good_city = 1 if  clean_hcity == "SANANTONIO,TX" 
	
	replace final_city = "DELMAR" if  clean_hcity == "DELMARHEIGHTS"
	replace good_city = 1 if  clean_hcity == "DELMARHEIGHTS"  
		
	replace final_city = "SANJOSE" if  clean_hcity == "SOUTHSANJOSE"
	replace good_city = 1 if  clean_hcity == "SOUTHSANJOSE" 
	
	replace final_city = "TAMPA" if  clean_hcity == "33614"
	replace good_city = 1 if  clean_hcity == "33614" 
	
	replace final_city = "BRANDON" if  clean_hcity == "33511"
	replace good_city = 1 if  clean_hcity == "33511" 
	
	replace final_city = "CLEARWATER" if  clean_hcity == "CLEARWATERBEACH"
	replace good_city = 1 if  clean_hcity == "CLEARWATERBEACH" 
	
	replace final_city = "PALMHARBOR" if  clean_hcity == "PALM/HUDSON"
	replace good_city = 1 if  clean_hcity == "PALM/HUDSON" 	
	
	replace final_city = "REDINGTONBEACH" if  clean_hcity == "REDINGTONSHORES"
	replace good_city = 1 if  clean_hcity == "REDINGTONSHORES"
	
	replace final_city = "BEACHWOOD" if  clean_hcity == "BEACHWOODOH44122"
	replace good_city = 1 if  clean_hcity == "BEACHWOODOH44122"
	
	replace final_city = "HILLSBOROUGH" if clean_hcity == "HILLSBORO"
	replace good_city = 1 if  clean_hcity == "HILLSBORO"
	
	replace final_city = "THORNTON" if clean_hcity == "THORTON"
	replace good_city = 1 if  clean_hcity == "THORTON"
	
	replace final_city = "BELAIR" if clean_hcity == "NBELAIR"
	replace good_city = 1 if  clean_hcity == "NBELAIR"
	
	replace final_city = "NARBERTH" if clean_hcity == "NARBETH"
	replace good_city = 1 if clean_hcity == "NARBETH"
	
	replace final_city = "MILLBURY" if clean_hcity == "MILBURY"
	replace good_city = 1 if clean_hcity == "MILBURY"
	
	replace final_city = "CARROLLTON" if clean_hcity == "CARROLTON"
	replace good_city = 1 if clean_hcity == "CARROLTON"
	
	replace final_city = "MOUNT AIRY" if clean_hcity == "MTAIRY"
	replace good_city = 1 if clean_hcity == "MTAIRY"
	
	replace final_city = "WASHINGTON" if clean_hcity == "NORTHEAST"
	replace good_city = 1 if clean_hcity == "NORTHEAST"
	
	replace final_city = "GREENBELT" if clean_hcity == "GREENBELY"
	replace good_city = 1 if clean_hcity == "GREENBELY"
	
	replace final_city = "CORINTH" if clean_hcity == "CORNITH"
	replace good_city = 1 if clean_hcity == "CORNITH"
	
	replace final_city = "SLEEPYHOLLOW" if clean_hcity == "SLEEPHOLLOW"
	replace good_city = 1 if clean_hcity == "SLEEPHOLLOW"

	replace final_city = "BRATENAHL" if clean_hcity == "BRETENAHL"
	replace good_city = 1 if clean_hcity == "BRETENAHL"
	
	replace final_city = "THEWOODLANDS" if clean_hcity == "WOODLANDS"
	replace good_city = 1 if clean_hcity == "WOODLANDS"
	
	replace final_city = "FOUNTAINVALLEY" if clean_hcity == "VALLEY"
	replace good_city = 1 if clean_hcity == "VALLEY"
	
	replace final_city = "BOSTON" if clean_hcity == "BEACONHILL"
	replace good_city = 1 if clean_hcity == "BEACONHILL"
	
	replace final_city = "WESTMINSTER" if clean_hcity == "TANEYTOWN"
	replace good_city = 1 if clean_hcity == "TANEYTOWN"
	
	replace final_city = "DALLAS" if clean_hcity == "UNIVERSITYPARK"
	replace good_city = 1 if clean_hcity == "UNIVERSITYPARK"
	
	replace final_city = "KANSASCITY" if clean_hcity == "NORTHKANSASCITY"
	replace good_city = 1 if clean_hcity == "NORTHKANSASCITY"
	
	replace final_city = "ROXBURY" if clean_hcity == "ROXBURRY"
	replace good_city = 1 if clean_hcity == "ROXBURRY"
	
	replace final_city = "EASTBRUNSWICK" if clean_hcity == "RAMBLINGHILL"
	replace good_city = 1 if clean_hcity == "RAMBLINGHILL"
	
	replace final_city = "BASKINGRIDGE" if clean_hcity == "BERNARDSTWP"
	replace good_city = 1 if clean_hcity == "BERNARDSTWP"
	
	replace final_city = "TABOR" if clean_hcity == "MTTABOR"
	replace good_city = 1 if clean_hcity == "MTTABOR"
	
	replace final_city = "MIAMI" if clean_hcity == "IVESESTATES"
	replace good_city = 1 if clean_hcity == "IVESESTATES"
	
	replace final_city = "MIAMI" if clean_hcity == "KENDALL"
	replace good_city = 1 if clean_hcity == "KENDALL"
	
	replace final_city = "NORTHOLMSTED" if clean_hcity == "NOLMSTEAD"
	replace good_city = 1 if clean_hcity == "NOLMSTEAD"
	
	replace final_city = "HESPERIA" if (hzip == "92344")  & (good_city == 0)
	replace good_city = 1 if final_city == "HESPERIA"

	
	/*179 not classified, 1,043 unique cities*/
	replace final_city = "BUMPASS" if clean_hcity == "BUMPASS"
	replace good_city = 1 if clean_hcity == "BUMPASS"
	
	replace final_city = "ROXBURY" if clean_hcity == "ROXBURY"
	replace good_city = 1 if clean_hcity == "ROXBURY"
	
	/*consider anything with a frequency of 3 as good, but don't count duplicate addresses */
	preserve
	
	keep if good_city == 0
	
	keep clean_hcity hsitead market
	
	duplicates drop
	
	bysort market clean_hcity: egen bad_count = count(clean_hcity)
	
	save "${PATH}\Cleaner\uniquehlistings_t10_2.dta", replace 
	
	restore
	
	merge m:1 clean_hcity market hsitead using  "${PATH}\Cleaner\uniquehlistings_t10_2.dta"
	
	drop _merge

	replace final_city = clean_hcity if (bad_count >=3) & (final_city == ".")
	
	replace good_city = 1 if bad_count >=3
	
	/*this corrected 144, 80 left, 1,034 unique cities*/
	
*save "${PATH}data\tab6addzip.dta", replace

	
/*city names for regression, final city if good_city=1, original otherwise */
	
	gen temp_city = ""
	replace temp_city = final_city if good_city==1
	replace temp_city = clean_hcity if good_city==0
	
	
save "${PATH}\Cleaner\tab10_2addzip.dta", replace

/*drop new variables*/
drop zip decommissioned primary_city acceptable_cities unacceptable_cities county timezone area_codes world_region country irs_estimated_population allaccept1 allaccept2 allaccept3 allaccept4 allaccept5 allaccept6 allaccept7 allaccept8 allaccept9 allaccept10 allaccept11 allaccept12 allaccept13 allaccept14 allaccept15 allaccept16 officialuspscityname officialuspsstatecode officialstatename officialcountyname allcounty1 allcounty2 allcounty3 allcounty4 allcounty5 allcounty6 upper_hcity upper_primary upper_usps upper_allaccept1 upper_allaccept2 upper_allaccept3 upper_allaccept4 upper_allaccept5 upper_allaccept6 upper_allaccept7 upper_allaccept8 upper_allaccept9 upper_allaccept10 upper_allaccept11 upper_allaccept12 upper_allaccept13 upper_allaccept14 upper_allaccept15 upper_allaccept16 upper_allcounty1 upper_allcounty2 upper_allcounty3 upper_allcounty4 upper_allcounty5 upper_allcounty6 clean_primary clean_usps clean_allaccept1 clean_allaccept2 clean_allaccept3 clean_allaccept4 clean_allaccept5 clean_allaccept6 clean_allaccept7 clean_allaccept8 clean_allaccept9 clean_allaccept10 clean_allaccept11 clean_allaccept12 clean_allaccept13 clean_allaccept14 clean_allaccept15 clean_allaccept16 clean_allaccept14 clean_allcounty1 clean_allcounty2 clean_allcounty3 clean_allcounty4 clean_allcounty5 clean_allcounty6 split_hcity1 split_hcity2 split_hcity3 grouped_hcity group_freq grouping_hcity group_mode

compress

save "${PATH}\Cleaned Datasets\Table10_2_cityadjusted.dta", replace



////////////////////////////////////////////////////
// TABLE 10
////////////////////////////////////////////////////

clear

/*set path here*/

global PATH "C:\Users\sunny\OneDrive\Desktop\Replication Games"

*global PATH "C:\Users\a_gan\Dropbox\Public\Replication Games\HuD_Replication\"

*import delimited "${PATH}csv_cleaned\table6.csv"


import delimited "${PATH}\csvs\table10_mom.csv", bindquote(strict)

/*turn log on*/
	cap log close
	//log using "${PATH}log\cleaner_t10_2.txt", text replace


/*want to match on hzip_rec*/
/*we need to clean hcity*/

/*requires four packages*/

*ssc install egenmore
*ssc install strgroup
*ssc install matchit
*ssc install freqindex
/*----------------------------------*/
/*starting point*/
/*
 there are 1,912 zip codes in the estimation sample
 there are 1688 cities in the raw data
*/

/*run the regression, keep the estimation sample only*/
	*reghdfe w2012pc_rec ofcolor, absorb(control sequencexx monthx hcity market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx) keepsingle cluster(control)
	
	/*commented out for now, for matching purposes*/
	*keep if e(sample)==1

/*clean up zip*/

	gen lenzip = strlen(hzip)

	*there are some 9/10 digit zips, clean those up
	gen zip = substr(hzip,1,5)
	
	*make the string numeric to match other file

	destring zip, replace force

/*merging city*/
	merge m:1 zip using "${PATH}\Cleaner\zipinfo.dta"
	
	*drop the not matched from zip file
	
	drop if _merge==2
	
	drop _merge
	
	/*drop the unneeded alternatives*/
	
	drop allaccept17-allaccept31
	

/*merging county*/
	merge m:1 zip using "${PATH}\Cleaner\zipinfo-county.dta"
	
	*drop the not matched from zip file
	
	drop if _merge==2
	
	drop _merge
	

/*generate a flag for whether or not the name is good*/

	gen good_city = 0
	
/*does the city match the acceptable city name*/
	replace good_city = 1 if hcityx == primary_city
	replace good_city = 1 if hcityx == officialuspscityname
	
	/*15,427  changes made*/
	
/*loop over all the acceptable cities and counties*/
	
	forvalues i = 1 /16{
		
		replace good_city = 1 if hcityx == 	allaccept`i'
	}

	forvalues i = 1 /6{
		
		replace good_city = 1 if hcityx == 	allcounty`i'
	}

	/*only the first made any changes, there were 692*/
	/*county matches another 219 */
	/*5,760 obs have a flag value of 0 */
	
	
/*now let's try to clean the strings*/

	gen upper_hcity = upper(hcityx)
	gen upper_primary = upper(primary_city)
	gen upper_usps = upper(officialuspscityname)

	replace good_city = 1 if upper_hcity == upper_primary
	replace good_city = 1 if upper_hcity == upper_usps
	
	/*this matched another 2,123 cities*/
	/*counties did not match any*/
		/*down to 1,323 unique cities*/
	
	/*loop over all the acceptable cities*/
		forvalues i = 1 /16{
			gen upper_allaccept`i' = upper(allaccept`i')
			replace good_city = 1 if upper_hcity == 	upper_allaccept`i'
		}
		
		forvalues i = 1 /6{
			gen upper_allcounty`i' = upper(allcounty`i')
			replace good_city = 1 if upper_hcity == 	upper_allcounty`i'
		}
		
		/*the first was the only one that matched*/
		
		
	/*delete spaces and periods*/
	
	gen clean_hcity = subinstr(upper_hcity," ","",100000)
	replace clean_hcity = subinstr(clean_hcity, ".","",10000)
	replace clean_hcity = subinstr(clean_hcity, "-","",10000)
	replace clean_hcity = subinstr(clean_hcity, "'","",10000)
	replace clean_hcity = subinstr(clean_hcity, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_hcity = subinstr(clean_hcity,"`i'", "", 10000)
	}
	
	gen clean_primary = subinstr(upper_primary," ","",100000)
	replace clean_primary = subinstr(clean_primary, ".","",10000)
	replace clean_primary = subinstr(clean_primary, "-","",10000)
	replace clean_primary = subinstr(clean_primary, "'","",10000)
	replace clean_primary = subinstr(clean_primary, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_primary = subinstr(clean_primary,"`i'", "", 10000)
	}
	
	gen clean_usps = subinstr(upper_usps," ","",100000)
	replace clean_usps = subinstr(clean_usps, ".","",10000)
	replace clean_usps = subinstr(clean_usps, "-","",10000)
	replace clean_usps = subinstr(clean_usps, "'","",10000)
	replace clean_usps = subinstr(clean_usps, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_usps = subinstr(clean_usps,"`i'", "", 10000)
	}
	
	replace good_city = 1 if clean_hcity == clean_primary
	replace good_city = 1 if clean_hcity == clean_usps
	
	/*this matched just 96 cities*/
	/* county did not match any extras */
		
	/*loop over all the acceptable cities*/
		forvalues i = 1 /16{
		
			gen clean_allaccept`i' = subinstr(upper_allaccept`i'," ", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',".", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',"-", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',"'", "", 10000)
			replace clean_allaccept`i' = subinstr(clean_allaccept`i',",", "", 10000)
			
			forvalues j = 0/9 {
				replace clean_allaccept`i' = subinstr(clean_allaccept`i',"`j'", "", 10000)
			}

			replace good_city = 1 if clean_hcity == clean_allaccept`i'
		
		}
		
		forvalues i = 1 /6{
		
			gen clean_allcounty`i' = subinstr(upper_allcounty`i'," ", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',".", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',"-", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',"'", "", 10000)
			replace clean_allcounty`i' = subinstr(clean_allcounty`i',",", "", 10000)
			
			forvalues j = 0/9 {
				replace clean_allcounty`i' = subinstr(clean_allcounty`i',"`j'", "", 10000)
			}

			replace good_city = 1 if clean_hcity == clean_allcounty`i'
		}
		
		
		/*this matched 892 more obs, down to 2,250 with a bad flag*/
			/*unique cities at 1,280*/
	
	/*clean up missing zip code ones*/
		/*if two hcities share a name, and one is flagged good, then all should be*/
		
	bysort clean_hcity: egen mean_good = mean(good_city)
	replace good_city = 1 if mean_good != 0
		/*1,267 more with good flag*/

	

	
/*create the final city*/

	gen final_city = "."
	
	replace final_city = clean_hcity if good_city==1
	
	

/*try some more string groups using zip codes*/	

	gen strzip = zip
	tostring strzip, replace force
	
	gen ziplength = strlen(strzip)
	
	replace strzip = "0" + strzip if ziplength==4
	
	gen zipfour = substr(strzip,1,4)
	/*
		sort zipfour
	
		by zipfour: strgroup clean_hcity, gen(group_zip) threshold(0.75) first
	
		bysort group_zip: egen group_freq_zip = count(group_zip)

	*/

/* tries to match on splits of the typed city name and the primary city name */
	
	split upper_hcity, parse(" ") generate(split_hcity)
	
	replace final_city = clean_primary if (split_hcity1 == clean_primary) & (good_city == 0)
	replace good_city = 1 if split_hcity1 == (clean_primary)
	/* fixes 196 more, 787 left */
	

	forvalues i = 1/3 {
		replace final_city =clean_primary if (split_hcity`i' == clean_primary) & (good_city == 0) & (upper_primary != "")
		replace good_city = 1 if (split_hcity`i' == clean_primary) & (upper_primary != "")
	}
/* fixes 20 more, 767 to go, 1,036 unique cities */
/*now let's try the fuzzy matching approach*/
/*unique items*/
	matchit clean_hcity clean_primary, similmethod(soundex_ext) weights(log)
	replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
	replace good_city = 1 if similscore > 0
	drop similscore
	

	/*find the modal string for each string group*/
	
	/*339 real changes, 428 left,*/
	
	preserve
	
	bysort clean_hcity: gen count_city = _n
	
	keep if count_city ==1
	
	sort market
	
	by market: strgroup clean_hcity, gen(grouped_hcity) threshold(0.25) first 
	
	/*generate frequency of the groups*/
	bysort grouped_hcity: egen group_freq = count(grouped_hcity)
	
	keep clean_hcity grouped_hcity group_freq
	
	save "${PATH}\Cleaner\uniquehcity_t10.dta", replace 
	
	restore
	
	merge m:1 clean_hcity using  "${PATH}\Cleaner\uniquehcity_t10.dta"
	
	drop _merge

	
	/*find the modal string for each string group*/
	gen grouping_hcity = .
	replace grouping_hcity = grouped_hcity if group_freq>=2
	bysort grouping_hcity: egen group_mode = mode(clean_hcity) 
	
	replace final_city = group_mode if (group_freq >=2) & (final_city == ".")
	replace good_city=1 if group_freq >=2
	
	/* 97 changes, 331 left */
	
	/* loop matchit through other accepts */
	forvalues i = 1 /3{
			matchit clean_hcity clean_allaccept`i', similmethod(soundex_ext) weights(log)
			replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
			replace good_city = 1 if similscore > 0
			drop similscore
		}
	
	/* 47 changes,  284 left */

	
/*manual inspections by market*/

	levelsof market, local(mklist)
	
	foreach mkt in `mklist' {
		
		disp "market is `mkt'"
		tab clean_hcity good_city if market == "`mkt'" 
	}

/* tries to match on splits of the typed city name and the primary city name */
	
	/*manual changes*/
	
	replace final_city = "ALBUQUERQUE" if  clean_hcity == "87111"
	replace good_city = 1 if  clean_hcity == "87111"
	
	replace final_city = "CARROLLTON" if  clean_hcity == "CAROLLTON"
	replace good_city = 1 if  clean_hcity == "CAROLLTON" 
	
	replace final_city = "SMYRNA" if  clean_hcity == "SMRYNA"
	replace good_city = 1 if  clean_hcity == "SMRYNA" 
	
	replace final_city = "ANNAPOLIS" if  clean_hcity == "ANN"
	replace good_city = 1 if  clean_hcity == "ANN" 
	
	 replace final_city = "COLUMBIA" if  clean_hcity == "COLUMBIAMD"
	replace good_city = 1 if  clean_hcity == "COLUMBIAMD" 
	
	replace final_city = "BELAIR " if  clean_hcity == " NBELAIR"
	replace good_city = 1 if  clean_hcity == " NBELAIR" 
	
	replace final_city = "MIDDLEBURGHEIGHTS" if  clean_hcity == "MIDDLEBURGHTS"
	replace good_city = 1 if  clean_hcity == "MIDDLEBURGHTS" 
	
	replace final_city = "NORTHRIDGEVILLE" if  clean_hcity == "MORTHRIDGEVILLE"
	replace good_city = 1 if  clean_hcity == "MORTHRIDGEVILLE"
	
	replace final_city = "WOODBRIDGE" if  clean_hcity == "WOODBRIGE"
	replace good_city = 1 if  clean_hcity == "WOODBRIGE" 
	
	replace final_city = "LEWISVILLE" if  clean_hcity == "LEWSIVILLE"
	replace good_city = 1 if  clean_hcity == "LEWSIVILLE" 
	
	replace final_city = "SACHSE" if  clean_hcity == "SACSHE"
	replace good_city = 1 if  clean_hcity == "SACSHE" 
	
	replace final_city = "MACOMB" if  clean_hcity == "MACOMBTWP"
	replace good_city = 1 if  clean_hcity == "MACOMBTWP" 
	
	replace final_city = "MACOMB" if  clean_hcity == "MACOMBTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "MACOMBTOWNSHIP" 
	
	replace final_city = "NORTHRICHLANDHILLS" if  clean_hcity == "NORTHRICHLANDS"
	replace good_city = 1 if  clean_hcity == "NORTHRICHLANDS" 

	replace final_city = "NORTHRICHLANDHILLS" if  clean_hcity == "NRICHLANDHILLS"
	replace good_city = 1 if  clean_hcity == "NRICHLANDHILLS" 

	replace final_city = "HOUSTON" if  clean_hcity == "HOUS"
	replace good_city = 1 if  clean_hcity == "HOUS"
	
	replace final_city = "HOUSTON" if  clean_hcity == "HUOSTON"
	replace good_city = 1 if  clean_hcity == "HUOSTON"
	
	replace final_city = "BELTON" if  clean_hcity == "BLETON"
	replace good_city = 1 if  clean_hcity == "BLETON" 
	
	replace final_city = "OVERLANDPARK" if  clean_hcity == "OVERLAND"
	replace good_city = 1 if  clean_hcity == "OVERLAND" 
	
	replace final_city = "LONGBEACH" if  clean_hcity == "NORTHLONGBEACH"
	replace good_city = 1 if  clean_hcity == "NORTHLONGBEACH" 
	
	replace final_city = "EASTAMWELLTWP" if  clean_hcity == "EASTAMWELL"
	replace good_city = 1 if  clean_hcity == "EASTAMWELL" 
	
	replace final_city = "GREENBROOK" if  clean_hcity == "GLENBROOKE"
	replace good_city = 1 if  clean_hcity == "GLENBROOKE" 
	
	replace final_city = "MONMOUTHJUNCTION" if  clean_hcity == "MONJUNCTION"
	replace good_city = 1 if  clean_hcity == "MONJUNCTION" 
	
	replace final_city = "EDISON" if  clean_hcity == "NORTHEDISON"
	replace good_city = 1 if  clean_hcity == "NORTHEDISON" 
	
	replace final_city = "EDISON" if  clean_hcity == "SOUTHEDISON"
	replace good_city = 1 if  clean_hcity == "SOUTHEDISON" 
	
	replace final_city = "WOODBRIDGE" if  clean_hcity == "WOODBRIDGEPROPER"
	replace good_city = 1 if  clean_hcity == "WOODBRIDGEPROPER"
	
	replace final_city = "BLOOMFIELD" if  clean_hcity == "BLOOMFIELDTWP"
	replace good_city = 1 if  clean_hcity == "BLOOMFIELDTWP"
 	
	replace final_city = "BOONTON" if  clean_hcity == "BOONTONTOWN"
	replace good_city = 1 if  clean_hcity == "BOONTONTOWN" 	
	
	replace final_city = "CALDWELL" if  clean_hcity == "CALDWELLBOROTWP"
	replace good_city = 1 if  clean_hcity == "CALDWELLBOROTWP" 
	
	replace final_city = "CHATHAM" if  clean_hcity == "CHATHAMBOROUGH"
	replace good_city = 1 if  clean_hcity == "CHATHAMBOROUGH"
	
	replace final_city = "CHATHAM" if  clean_hcity == "CHATHAMTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "CHATHAMTOWNSHIP"
	
	replace final_city = "HANOVERTOWNSHIP" if  clean_hcity == "HANOVERTWP"
	replace good_city = 1 if  clean_hcity == "HANOVERTWP"
	
	replace final_city = "MADISON" if  clean_hcity == "MADISONBOROUGH"
	replace good_city = 1 if  clean_hcity == "MADISONBOROUGH" 
	
	replace final_city = "MORRISTOWN" if  clean_hcity == "MORRISTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "MORRISTOWNSHIP" 
	
	replace final_city = "MORRISTOWN" if  clean_hcity == "MORRISTWP"
	
	replace final_city = "MOUNTOLIVE" if  clean_hcity == "MOUNTOLIVETWP"
	replace good_city = 1 if  clean_hcity == "MOUNTOLIVETWP" 
	
	replace final_city = "MOUNTOLIVE" if  clean_hcity == "MTOLIVE"
	replace good_city = 1 if  clean_hcity == "MTOLIVE" 
	
	replace final_city = "MOUNTOLIVE" if  clean_hcity == "MTOLIVETWP"
	replace good_city = 1 if  clean_hcity == "MTOLIVETWP" 
	
	replace final_city = "PARSIPPANY" if  clean_hcity == "PARSIPPANY-TROYHILLS"
	replace good_city = 1 if  clean_hcity == "PARSIPPANYTROYHILLS" 
	
	replace final_city = "PARSIPPANY" if  clean_hcity == "PARSIPPANY-TROYHILLS"
	
	replace final_city = "BROOKLYN" if  clean_hcity == "BAYRIDGE,BROOKLYN "
	replace good_city = 1 if  clean_hcity == "BAYRIDGE,BROOKLYN " 	
	
	replace final_city = "BROOKLYN" if  clean_hcity == "BKLYN"
	replace good_city = 1 if clean_hcity == "BKLYN" 	
	
	replace final_city = "FLUSHING" if  clean_hcity == "FLUSHINGQUEENS"
	replace good_city = 1 if  clean_hcity == "FLUSHINGQUEENS" 

	replace final_city = "ARDMORE" if  clean_hcity == "ARDMOOR"
	replace good_city = 1 if  clean_hcity == "ARDMOOR"
	
	replace final_city = "HAVERTOWN" if  clean_hcity == "HAVERTOWNTOWNSHIP"
	replace good_city = 1 if  clean_hcity == "HAVERTOWNTOWNSHIP" 
	
	replace final_city = "PROSPECTPARK" if  clean_hcity == "PROSPECTPARKBORO"
	replace good_city = 1 if  clean_hcity == "PROSPECTPARKBORO" 
	
	replace final_city = "WARRINGTON" if  clean_hcity == "WARRINGTONLANE"
	replace good_city = 1 if  clean_hcity == "WARRINGTONLANE" 	
	
	replace final_city = "WARRINGTON" if  clean_hcity == "WARRINGTONTWP"
	replace good_city = 1 if  clean_hcity == "WARRINGTONTWP"

	replace final_city = "CHESTERFIELD" if  clean_hcity == "CHESTEFILED"
	replace good_city = 1 if  clean_hcity == "CHESTEFILED" 
	
	replace final_city = "YUCCAVALLEY" if  clean_hcity == "92284"
	replace good_city = 1 if  clean_hcity == "92284" 
	 	
	replace final_city = "SANANTONIO" if  clean_hcity == "SANANTONIO,TX"
	replace good_city = 1 if  clean_hcity == "SANANTONIO,TX" 
	
	replace final_city = "DELMAR" if  clean_hcity == "DELMARHEIGHTS"
	replace good_city = 1 if  clean_hcity == "DELMARHEIGHTS"  
		
	replace final_city = "SANJOSE" if  clean_hcity == "SOUTHSANJOSE"
	replace good_city = 1 if  clean_hcity == "SOUTHSANJOSE" 
	
	replace final_city = "TAMPA" if  clean_hcity == "33614"
	replace good_city = 1 if  clean_hcity == "33614" 
	
	replace final_city = "BRANDON" if  clean_hcity == "33511"
	replace good_city = 1 if  clean_hcity == "33511" 
	
	replace final_city = "CLEARWATER" if  clean_hcity == "CLEARWATERBEACH"
	replace good_city = 1 if  clean_hcity == "CLEARWATERBEACH" 
	
	replace final_city = "PALMHARBOR" if  clean_hcity == "PALM/HUDSON"
	replace good_city = 1 if  clean_hcity == "PALM/HUDSON" 	
	
	replace final_city = "REDINGTONBEACH" if  clean_hcity == "REDINGTONSHORES"
	replace good_city = 1 if  clean_hcity == "REDINGTONSHORES"
	
	replace final_city = "BEACHWOOD" if  clean_hcity == "BEACHWOODOH44122"
	replace good_city = 1 if  clean_hcity == "BEACHWOODOH44122"
	
	replace final_city = "HILLSBOROUGH" if clean_hcity == "HILLSBORO"
	replace good_city = 1 if  clean_hcity == "HILLSBORO"
	
	replace final_city = "THORNTON" if clean_hcity == "THORTON"
	replace good_city = 1 if  clean_hcity == "THORTON"
	
	replace final_city = "BELAIR" if clean_hcity == "NBELAIR"
	replace good_city = 1 if  clean_hcity == "NBELAIR"
	
	replace final_city = "NARBERTH" if clean_hcity == "NARBETH"
	replace good_city = 1 if clean_hcity == "NARBETH"
	
	replace final_city = "MILLBURY" if clean_hcity == "MILBURY"
	replace good_city = 1 if clean_hcity == "MILBURY"
	
	replace final_city = "CARROLLTON" if clean_hcity == "CARROLTON"
	replace good_city = 1 if clean_hcity == "CARROLTON"
	
	replace final_city = "MOUNT AIRY" if clean_hcity == "MTAIRY"
	replace good_city = 1 if clean_hcity == "MTAIRY"
	
	replace final_city = "WASHINGTON" if clean_hcity == "NORTHEAST"
	replace good_city = 1 if clean_hcity == "NORTHEAST"
	
	replace final_city = "GREENBELT" if clean_hcity == "GREENBELY"
	replace good_city = 1 if clean_hcity == "GREENBELY"
	
	replace final_city = "CORINTH" if clean_hcity == "CORNITH"
	replace good_city = 1 if clean_hcity == "CORNITH"
	
	replace final_city = "SLEEPYHOLLOW" if clean_hcity == "SLEEPHOLLOW"
	replace good_city = 1 if clean_hcity == "SLEEPHOLLOW"

	replace final_city = "BRATENAHL" if clean_hcity == "BRETENAHL"
	replace good_city = 1 if clean_hcity == "BRETENAHL"
	
	replace final_city = "THEWOODLANDS" if clean_hcity == "WOODLANDS"
	replace good_city = 1 if clean_hcity == "WOODLANDS"
	
	replace final_city = "FOUNTAINVALLEY" if clean_hcity == "VALLEY"
	replace good_city = 1 if clean_hcity == "VALLEY"
	
	replace final_city = "BOSTON" if clean_hcity == "BEACONHILL"
	replace good_city = 1 if clean_hcity == "BEACONHILL"
	
	replace final_city = "WESTMINSTER" if clean_hcity == "TANEYTOWN"
	replace good_city = 1 if clean_hcity == "TANEYTOWN"
	
	replace final_city = "DALLAS" if clean_hcity == "UNIVERSITYPARK"
	replace good_city = 1 if clean_hcity == "UNIVERSITYPARK"
	
	replace final_city = "KANSASCITY" if clean_hcity == "NORTHKANSASCITY"
	replace good_city = 1 if clean_hcity == "NORTHKANSASCITY"
	
	replace final_city = "ROXBURY" if clean_hcity == "ROXBURRY"
	replace good_city = 1 if clean_hcity == "ROXBURRY"
	
	replace final_city = "EASTBRUNSWICK" if clean_hcity == "RAMBLINGHILL"
	replace good_city = 1 if clean_hcity == "RAMBLINGHILL"
	
	replace final_city = "BASKINGRIDGE" if clean_hcity == "BERNARDSTWP"
	replace good_city = 1 if clean_hcity == "BERNARDSTWP"
	
	replace final_city = "TABOR" if clean_hcity == "MTTABOR"
	replace good_city = 1 if clean_hcity == "MTTABOR"
	
	replace final_city = "MIAMI" if clean_hcity == "IVESESTATES"
	replace good_city = 1 if clean_hcity == "IVESESTATES"
	
	replace final_city = "MIAMI" if clean_hcity == "KENDALL"
	replace good_city = 1 if clean_hcity == "KENDALL"
	
	replace final_city = "NORTHOLMSTED" if clean_hcity == "NOLMSTEAD"
	replace good_city = 1 if clean_hcity == "NOLMSTEAD"
	
	replace final_city = "HESPERIA" if (hzip == "92344")  & (good_city == 0)
	replace good_city = 1 if final_city == "HESPERIA"

	
	/*179 not classified, 1,043 unique cities*/
	replace final_city = "BUMPASS" if clean_hcity == "BUMPASS"
	replace good_city = 1 if clean_hcity == "BUMPASS"
	
	replace final_city = "ROXBURY" if clean_hcity == "ROXBURY"
	replace good_city = 1 if clean_hcity == "ROXBURY"
	
	/*consider anything with a frequency of 3 as good, but don't count duplicate addresses */
	preserve
	
	keep if good_city == 0
	
	keep clean_hcity hsitead market
	
	duplicates drop
	
	bysort market clean_hcity: egen bad_count = count(clean_hcity)
	
	save "${PATH}\Cleaner\uniquehlistings_t10.dta", replace 
	
	restore
	
	merge m:1 clean_hcity market hsitead using  "${PATH}\Cleaner\uniquehlistings_t10.dta"
	
	drop _merge

	replace final_city = clean_hcity if (bad_count >=3) & (final_city == ".")
	
	replace good_city = 1 if bad_count >=3
	
	/*this corrected 144, 80 left, 1,034 unique cities*/
	
*save "${PATH}data\tab6addzip.dta", replace

	
/*city names for regression, final city if good_city=1, original otherwise */
	
	gen temp_city = ""
	replace temp_city = final_city if good_city==1
	replace temp_city = clean_hcity if good_city==0
	
	
save "${PATH}\Cleaner\tab10addzip.dta", replace

/*drop new variables*/
drop zip decommissioned primary_city acceptable_cities unacceptable_cities county timezone area_codes world_region country irs_estimated_population allaccept1 allaccept2 allaccept3 allaccept4 allaccept5 allaccept6 allaccept7 allaccept8 allaccept9 allaccept10 allaccept11 allaccept12 allaccept13 allaccept14 allaccept15 allaccept16 officialuspscityname officialuspsstatecode officialstatename officialcountyname allcounty1 allcounty2 allcounty3 allcounty4 allcounty5 allcounty6 upper_hcity upper_primary upper_usps upper_allaccept1 upper_allaccept2 upper_allaccept3 upper_allaccept4 upper_allaccept5 upper_allaccept6 upper_allaccept7 upper_allaccept8 upper_allaccept9 upper_allaccept10 upper_allaccept11 upper_allaccept12 upper_allaccept13 upper_allaccept14 upper_allaccept15 upper_allaccept16 upper_allcounty1 upper_allcounty2 upper_allcounty3 upper_allcounty4 upper_allcounty5 upper_allcounty6 clean_primary clean_usps clean_allaccept1 clean_allaccept2 clean_allaccept3 clean_allaccept4 clean_allaccept5 clean_allaccept6 clean_allaccept7 clean_allaccept8 clean_allaccept9 clean_allaccept10 clean_allaccept11 clean_allaccept12 clean_allaccept13 clean_allaccept14 clean_allaccept15 clean_allaccept16 clean_allaccept14 clean_allcounty1 clean_allcounty2 clean_allcounty3 clean_allcounty4 clean_allcounty5 clean_allcounty6 split_hcity1 split_hcity2 split_hcity3 grouped_hcity group_freq grouping_hcity group_mode

compress

save "${PATH}\Cleaned Datasets\Table10_cityadjusted.dta", replace