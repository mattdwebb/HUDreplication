/*Authors: Areez Gangji & Matt Webb */
/*Date: August 21, 2023 */

/*clean up zip*/
	gen lenzip = strlen(hzip_rec)

*there are some 9/10 digit zips, clean those up
	gen zip = substr(hzip_rec,1,5)
	
*make the string numeric to match other file
	destring zip, replace force

/*merging city*/
	merge m:1 zip using "${DATA}\zipinfo.dta"
	
*drop the not matched from zip file
	drop if _merge==2	
	drop _merge
	
/*drop the unneeded alternatives*/
	drop allaccept17-allaccept31
	

/* merging county */
	merge m:1 zip using "${DATA}\zipinfo-county.dta"
	
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
	gen upper_hcityx = upper(hcityx)
	gen upper_primary = upper(primary_city)
	gen upper_usps = upper(officialuspscityname)

	replace good_city = 1 if upper_hcityx == upper_primary
	replace good_city = 1 if upper_hcityx == upper_usps
	
/*this matched another 2,123 cities*/
/*counties did not match any*/
/*down to 1,323 unique cities*/
	
/*loop over all the acceptable cities*/
		forvalues i = 1 /16{
			gen upper_allaccept`i' = upper(allaccept`i')
			replace good_city = 1 if upper_hcityx == 	upper_allaccept`i'
		}
		
		forvalues i = 1 /6{
			gen upper_allcounty`i' = upper(allcounty`i')
			replace good_city = 1 if upper_hcityx == 	upper_allcounty`i'
		}
		
/*the first was the only one that matched*/

/*delete spaces and periods*/
	
	gen clean_hcityx = subinstr(upper_hcityx," ","",100000)
	replace clean_hcityx = subinstr(clean_hcityx, ".","",10000)
	replace clean_hcityx = subinstr(clean_hcityx, "-","",10000)
	replace clean_hcityx = subinstr(clean_hcityx, "'","",10000)
	replace clean_hcityx = subinstr(clean_hcityx, ",","",10000)
	
	forvalues i = 0/9{
		replace clean_hcityx = subinstr(clean_hcityx,"`i'", "", 10000)
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
	
	replace good_city = 1 if clean_hcityx == clean_primary
	replace good_city = 1 if clean_hcityx == clean_usps
	
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

		replace good_city = 1 if clean_hcityx == clean_allaccept`i'
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

		replace good_city = 1 if clean_hcityx == clean_allcounty`i'
	}
		
		
/*this matched 892 more obs, down to 2,250 with a bad flag*/
/*unique cities at 1,280*/

/*clean up missing zip code ones*/
/*if two hcities share a name, and one is flagged good, then all should be*/
	bysort clean_hcityx: egen mean_good = mean(good_city)
	replace good_city = 1 if mean_good != 0
	
/*1,267 more with good flag*/
	
/*create the final city*/
	gen final_city = "."
	replace final_city = clean_hcityx if good_city==1
	
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
	split upper_hcityx, parse(" ") generate(split_hcity)
	
	replace final_city = clean_primary if (split_hcity1 == clean_primary) & (good_city == 0)
	replace good_city = 1 if split_hcity1 == (clean_primary)
	/* fixes 196 more, 787 left */
	

	forvalues i = 1/4 {
		replace final_city =clean_primary if (split_hcity`i' == clean_primary) & (good_city == 0) & (upper_primary != "")
		replace good_city = 1 if (split_hcity`i' == clean_primary) & (upper_primary != "")
	}
/* fixes 20 more, 767 to go, 1,036 unique cities */
/*now let's try the fuzzy matching approach*/
/*unique items*/
	matchit clean_hcityx clean_primary, similmethod(soundex_ext) weights(log)
	replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
	replace good_city = 1 if similscore > 0
	drop similscore

/*find the modal string for each string group*/
/*339 real changes, 428 left,*/
	preserve
	bysort clean_hcityx: gen count_city = _n
	keep if count_city ==1
	sort market
	by market: strgroup clean_hcityx, gen(grouped_hcity) threshold(0.25) first 
	
/*generate frequency of the groups*/
	bysort grouped_hcity: egen group_freq = count(grouped_hcity)	
	keep clean_hcityx grouped_hcity group_freq	
	save "${OUTPUT}\uniquehcity.dta", replace 
	restore
	merge m:1 clean_hcityx using  "${OUTPUT}\uniquehcity.dta"
	
	drop _merge
	
/*find the modal string for each string group*/
	gen grouping_hcity = .
	replace grouping_hcity = grouped_hcity if group_freq>=2
	bysort grouping_hcity: egen group_mode = mode(clean_hcityx) 
	
	replace final_city = group_mode if (group_freq >=2) & (final_city == ".")
	replace good_city=1 if group_freq >=2
	
/* 97 changes, 331 left */

/* loop matchit through other accepts */
	forvalues i = 1 /4{
			matchit clean_hcityx clean_allaccept`i', similmethod(soundex_ext) weights(log)
			replace final_city = clean_primary if (similscore > 0) & (final_city == ".")
			replace good_city = 1 if similscore > 0
			drop similscore
		}
	
/* 47 changes,  284 left */

	
/*manual inspections by market*/
	levelsof market, local(mklist)
	foreach mkt in `mklist' {
		disp "market is `mkt'"
		tab clean_hcityx good_city if market == "`mkt'" 
	}

/* tries to match on splits of the typed city name and the primary city name */
	
/*manual changes - some of these are legacy of prior iterations and are no longer necessary*/
	replace final_city = "ALBUQUERQUE" if  clean_hcityx == "87111"
	replace good_city = 1 if  clean_hcityx == "87111"
	
	replace final_city = "CARROLLTON" if  clean_hcityx == "CAROLLTON"
	replace good_city = 1 if  clean_hcityx == "CAROLLTON" 
	
	replace final_city = "SMYRNA" if  clean_hcityx == "SMRYNA"
	replace good_city = 1 if  clean_hcityx == "SMRYNA" 
	
	replace final_city = "ANNAPOLIS" if  clean_hcityx == "ANN"
	replace good_city = 1 if  clean_hcityx == "ANN" 
	
	 replace final_city = "COLUMBIA" if  clean_hcityx == "COLUMBIAMD"
	replace good_city = 1 if  clean_hcityx == "COLUMBIAMD" 
	
	replace final_city = "BELAIR " if  clean_hcityx == " NBELAIR"
	replace good_city = 1 if  clean_hcityx == " NBELAIR" 
	
	replace final_city = "MIDDLEBURGHEIGHTS" if  clean_hcityx == "MIDDLEBURGHTS"
	replace good_city = 1 if  clean_hcityx == "MIDDLEBURGHTS" 
	
	replace final_city = "NORTHRIDGEVILLE" if  clean_hcityx == "MORTHRIDGEVILLE"
	replace good_city = 1 if  clean_hcityx == "MORTHRIDGEVILLE"
	
	replace final_city = "WOODBRIDGE" if  clean_hcityx == "WOODBRIGE"
	replace good_city = 1 if  clean_hcityx == "WOODBRIGE" 
	
	replace final_city = "LEWISVILLE" if  clean_hcityx == "LEWSIVILLE"
	replace good_city = 1 if  clean_hcityx == "LEWSIVILLE" 
	
	replace final_city = "SACHSE" if  clean_hcityx == "SACSHE"
	replace good_city = 1 if  clean_hcityx == "SACSHE" 
	
	replace final_city = "MACOMB" if  clean_hcityx == "MACOMBTWP"
	replace good_city = 1 if  clean_hcityx == "MACOMBTWP" 
	
	replace final_city = "MACOMB" if  clean_hcityx == "MACOMBTOWNSHIP"
	replace good_city = 1 if  clean_hcityx == "MACOMBTOWNSHIP" 
	
	replace final_city = "NORTHRICHLANDHILLS" if  clean_hcityx == "NORTHRICHLANDS"
	replace good_city = 1 if  clean_hcityx == "NORTHRICHLANDS" 

	replace final_city = "NORTHRICHLANDHILLS" if  clean_hcityx == "NRICHLANDHILLS"
	replace good_city = 1 if  clean_hcityx == "NRICHLANDHILLS" 

	replace final_city = "HOUSTON" if  clean_hcityx == "HOUS"
	replace good_city = 1 if  clean_hcityx == "HOUS"
	
	replace final_city = "HOUSTON" if  clean_hcityx == "HUOSTON"
	replace good_city = 1 if  clean_hcityx == "HUOSTON"
	
	replace final_city = "BELTON" if  clean_hcityx == "BLETON"
	replace good_city = 1 if  clean_hcityx == "BLETON" 
	
	replace final_city = "OVERLANDPARK" if  clean_hcityx == "OVERLAND"
	replace good_city = 1 if  clean_hcityx == "OVERLAND" 
	
	replace final_city = "LONGBEACH" if  clean_hcityx == "NORTHLONGBEACH"
	replace good_city = 1 if  clean_hcityx == "NORTHLONGBEACH" 
	
	replace final_city = "EASTAMWELLTWP" if  clean_hcityx == "EASTAMWELL"
	replace good_city = 1 if  clean_hcityx == "EASTAMWELL" 
	
	replace final_city = "GREENBROOK" if  clean_hcityx == "GLENBROOKE"
	replace good_city = 1 if  clean_hcityx == "GLENBROOKE" 
	
	replace final_city = "MONMOUTHJUNCTION" if  clean_hcityx == "MONJUNCTION"
	replace good_city = 1 if  clean_hcityx == "MONJUNCTION" 
	
	replace final_city = "EDISON" if  clean_hcityx == "NORTHEDISON"
	replace good_city = 1 if  clean_hcityx == "NORTHEDISON" 
	
	replace final_city = "EDISON" if  clean_hcityx == "SOUTHEDISON"
	replace good_city = 1 if  clean_hcityx == "SOUTHEDISON" 
	
	replace final_city = "WOODBRIDGE" if  clean_hcityx == "WOODBRIDGEPROPER"
	replace good_city = 1 if  clean_hcityx == "WOODBRIDGEPROPER"
	
	replace final_city = "BLOOMFIELD" if  clean_hcityx == "BLOOMFIELDTWP"
	replace good_city = 1 if  clean_hcityx == "BLOOMFIELDTWP"
 	
	replace final_city = "BOONTON" if  clean_hcityx == "BOONTONTOWN"
	replace good_city = 1 if  clean_hcityx == "BOONTONTOWN" 	
	
	replace final_city = "CALDWELL" if  clean_hcityx == "CALDWELLBOROTWP"
	replace good_city = 1 if  clean_hcityx == "CALDWELLBOROTWP" 
	
	replace final_city = "CHATHAM" if  clean_hcityx == "CHATHAMBOROUGH"
	replace good_city = 1 if  clean_hcityx == "CHATHAMBOROUGH"
	
	replace final_city = "CHATHAM" if  clean_hcityx == "CHATHAMTOWNSHIP"
	replace good_city = 1 if  clean_hcityx == "CHATHAMTOWNSHIP"
	
	replace final_city = "HANOVERTOWNSHIP" if  clean_hcityx == "HANOVERTWP"
	replace good_city = 1 if  clean_hcityx == "HANOVERTWP"
	
	replace final_city = "MADISON" if  clean_hcityx == "MADISONBOROUGH"
	replace good_city = 1 if  clean_hcityx == "MADISONBOROUGH" 
	
	replace final_city = "MORRISTOWN" if  clean_hcityx == "MORRISTOWNSHIP"
	replace good_city = 1 if  clean_hcityx == "MORRISTOWNSHIP" 
	
	replace final_city = "MORRISTOWN" if  clean_hcityx == "MORRISTWP"
	
	replace final_city = "MOUNTOLIVE" if  clean_hcityx == "MOUNTOLIVETWP"
	replace good_city = 1 if  clean_hcityx == "MOUNTOLIVETWP" 
	
	replace final_city = "MOUNTOLIVE" if  clean_hcityx == "MTOLIVE"
	replace good_city = 1 if  clean_hcityx == "MTOLIVE" 
	
	replace final_city = "MOUNTOLIVE" if  clean_hcityx == "MTOLIVETWP"
	replace good_city = 1 if  clean_hcityx == "MTOLIVETWP" 
	
	replace final_city = "PARSIPPANY" if  clean_hcityx == "PARSIPPANY-TROYHILLS"
	replace good_city = 1 if  clean_hcityx == "PARSIPPANYTROYHILLS" 
	
	replace final_city = "PARSIPPANY" if  clean_hcityx == "PARSIPPANY-TROYHILLS"
	
	replace final_city = "BROOKLYN" if  clean_hcityx == "BAYRIDGE,BROOKLYN "
	replace good_city = 1 if  clean_hcityx == "BAYRIDGE,BROOKLYN " 	
	
	replace final_city = "BROOKLYN" if  clean_hcityx == "BKLYN"
	replace good_city = 1 if clean_hcityx == "BKLYN" 	
	
	replace final_city = "FLUSHING" if  clean_hcityx == "FLUSHINGQUEENS"
	replace good_city = 1 if  clean_hcityx == "FLUSHINGQUEENS" 

	replace final_city = "ARDMORE" if  clean_hcityx == "ARDMOOR"
	replace good_city = 1 if  clean_hcityx == "ARDMOOR"
	
	replace final_city = "HAVERTOWN" if  clean_hcityx == "HAVERTOWNTOWNSHIP"
	replace good_city = 1 if  clean_hcityx == "HAVERTOWNTOWNSHIP" 
	
	replace final_city = "PROSPECTPARK" if  clean_hcityx == "PROSPECTPARKBORO"
	replace good_city = 1 if  clean_hcityx == "PROSPECTPARKBORO" 
	
	replace final_city = "WARRINGTON" if  clean_hcityx == "WARRINGTONLANE"
	replace good_city = 1 if  clean_hcityx == "WARRINGTONLANE" 	
	
	replace final_city = "WARRINGTON" if  clean_hcityx == "WARRINGTONTWP"
	replace good_city = 1 if  clean_hcityx == "WARRINGTONTWP"

	replace final_city = "CHESTERFIELD" if  clean_hcityx == "CHESTEFILED"
	replace good_city = 1 if  clean_hcityx == "CHESTEFILED" 
	
	replace final_city = "YUCCAVALLEY" if  clean_hcityx == "92284"
	replace good_city = 1 if  clean_hcityx == "92284" 
	 	
	replace final_city = "SANANTONIO" if  clean_hcityx == "SANANTONIO,TX"
	replace good_city = 1 if  clean_hcityx == "SANANTONIO,TX" 
	
	replace final_city = "DELMAR" if  clean_hcityx == "DELMARHEIGHTS"
	replace good_city = 1 if  clean_hcityx == "DELMARHEIGHTS"  
		
	replace final_city = "SANJOSE" if  clean_hcityx == "SOUTHSANJOSE"
	replace good_city = 1 if  clean_hcityx == "SOUTHSANJOSE" 
	
	replace final_city = "TAMPA" if  clean_hcityx == "33614"
	replace good_city = 1 if  clean_hcityx == "33614" 
	
	replace final_city = "BRANDON" if  clean_hcityx == "33511"
	replace good_city = 1 if  clean_hcityx == "33511" 
	
	replace final_city = "CLEARWATER" if  clean_hcityx == "CLEARWATERBEACH"
	replace good_city = 1 if  clean_hcityx == "CLEARWATERBEACH" 
	
	replace final_city = "PALMHARBOR" if  clean_hcityx == "PALM/HUDSON"
	replace good_city = 1 if  clean_hcityx == "PALM/HUDSON" 	
	
	replace final_city = "REDINGTONBEACH" if  clean_hcityx == "REDINGTONSHORES"
	replace good_city = 1 if  clean_hcityx == "REDINGTONSHORES"
	
	replace final_city = "BEACHWOOD" if  clean_hcityx == "BEACHWOODOH44122"
	replace good_city = 1 if  clean_hcityx == "BEACHWOODOH44122"
	
	replace final_city = "HILLSBOROUGH" if clean_hcityx == "HILLSBORO"
	replace good_city = 1 if  clean_hcityx == "HILLSBORO"
	
	replace final_city = "THORNTON" if clean_hcityx == "THORTON"
	replace good_city = 1 if  clean_hcityx == "THORTON"
	
	replace final_city = "BELAIR" if clean_hcityx == "NBELAIR"
	replace good_city = 1 if  clean_hcityx == "NBELAIR"
	
	replace final_city = "NARBERTH" if clean_hcityx == "NARBETH"
	replace good_city = 1 if clean_hcityx == "NARBETH"
	
	replace final_city = "MILLBURY" if clean_hcityx == "MILBURY"
	replace good_city = 1 if clean_hcityx == "MILBURY"
	
	replace final_city = "CARROLLTON" if clean_hcityx == "CARROLTON"
	replace good_city = 1 if clean_hcityx == "CARROLTON"
	
	replace final_city = "MOUNT AIRY" if clean_hcityx == "MTAIRY"
	replace good_city = 1 if clean_hcityx == "MTAIRY"
	
	replace final_city = "WASHINGTON" if clean_hcityx == "NORTHEAST"
	replace good_city = 1 if clean_hcityx == "NORTHEAST"
	
	replace final_city = "GREENBELT" if clean_hcityx == "GREENBELY"
	replace good_city = 1 if clean_hcityx == "GREENBELY"
	
	replace final_city = "CORINTH" if clean_hcityx == "CORNITH"
	replace good_city = 1 if clean_hcityx == "CORNITH"
	
	replace final_city = "SLEEPYHOLLOW" if clean_hcityx == "SLEEPHOLLOW"
	replace good_city = 1 if clean_hcityx == "SLEEPHOLLOW"

	replace final_city = "BRATENAHL" if clean_hcityx == "BRETENAHL"
	replace good_city = 1 if clean_hcityx == "BRETENAHL"
	
	replace final_city = "THEWOODLANDS" if clean_hcityx == "WOODLANDS"
	replace good_city = 1 if clean_hcityx == "WOODLANDS"
	
	replace final_city = "FOUNTAINVALLEY" if clean_hcityx == "VALLEY"
	replace good_city = 1 if clean_hcityx == "VALLEY"
	
	replace final_city = "BOSTON" if clean_hcityx == "BEACONHILL"
	replace good_city = 1 if clean_hcityx == "BEACONHILL"
	
	replace final_city = "WESTMINSTER" if clean_hcityx == "TANEYTOWN"
	replace good_city = 1 if clean_hcityx == "TANEYTOWN"
	
	replace final_city = "DALLAS" if clean_hcityx == "UNIVERSITYPARK"
	replace good_city = 1 if clean_hcityx == "UNIVERSITYPARK"
	
	replace final_city = "KANSASCITY" if clean_hcityx == "NORTHKANSASCITY"
	replace good_city = 1 if clean_hcityx == "NORTHKANSASCITY"
	
	replace final_city = "ROXBURY" if clean_hcityx == "ROXBURRY"
	replace good_city = 1 if clean_hcityx == "ROXBURRY"
	
	replace final_city = "EASTBRUNSWICK" if clean_hcityx == "RAMBLINGHILL"
	replace good_city = 1 if clean_hcityx == "RAMBLINGHILL"
	
	replace final_city = "BASKINGRIDGE" if clean_hcityx == "BERNARDSTWP"
	replace good_city = 1 if clean_hcityx == "BERNARDSTWP"
	
	replace final_city = "TABOR" if clean_hcityx == "MTTABOR"
	replace good_city = 1 if clean_hcityx == "MTTABOR"
	
	replace final_city = "MIAMI" if clean_hcityx == "IVESESTATES"
	replace good_city = 1 if clean_hcityx == "IVESESTATES"
	
	replace final_city = "MIAMI" if clean_hcityx == "KENDALL"
	replace good_city = 1 if clean_hcityx == "KENDALL"
	
	replace final_city = "NORTHOLMSTED" if clean_hcityx == "NOLMSTEAD"
	replace good_city = 1 if clean_hcityx == "NOLMSTEAD"
	
	replace final_city = "HESPERIA" if (hzip_rec == "92344")  & (good_city == 0)
	replace good_city = 1 if final_city == "HESPERIA"

	
/*179 not classified, 1,043 unique cities*/
	replace final_city = "BUMPASS" if clean_hcityx == "BUMPASS"
	replace good_city = 1 if clean_hcityx == "BUMPASS"
	
	replace final_city = "ROXBURY" if clean_hcityx == "ROXBURY"
	replace good_city = 1 if clean_hcityx == "ROXBURY"
	
/*consider anything with a frequency of 3 as good, but don't count duplicate addresses */
	preserve

	keep if good_city == 0	
	keep clean_hcityx hsitead_rec market	
	duplicates drop

	bysort market clean_hcityx: egen bad_count = count(clean_hcityx)
	save "${OUTPUT}\uniquehlistings.dta", replace 
	restore
	
	merge m:1 clean_hcityx market hsitead_rec using  "${OUTPUT}\uniquehlistings.dta"
	drop _merge
	replace final_city = clean_hcityx if (bad_count >=3) & (final_city == ".")
	replace good_city = 1 if bad_count >=3
	
/*this corrected 144, 80 left, 1,034 unique cities*/	
*save "${PATH}data\tab6addzip.dta", replace
/*city names for regression, final city if good_city=1, original otherwise */	
gen temp_city = ""
replace temp_city = final_city if good_city==1
replace temp_city = clean_hcityx if good_city==0