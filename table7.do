/*Written by: Matthew D. Webb - table 7*/
/*Date: August 21, 2023 */

clear

	/* Set path to the parent folder of the local location of the git repository */
	global PATH "C:\Users\antho\OneDrive - University of Toronto\Research\Replication Games"

	global CODE "${PATH}/HUDreplication" //set the file path to the main code directory
	global DATA "${CODE}/Data" // set the file path to the data subdirectory

	cap mkdir "${PATH}/Output" // make an Output folder if it doesn't already exist
	global OUTPUT "${PATH}/Output" // set the output file path
	
	cap log close
	log using "${OUTPUT}/table7_log.txt", text replace

import delimited "${DATA}\HUDprocessed_JPE_census_042021.csv"

/*--------------------------------------*/
/*cleaning*/
/*--------------------------------------*/	

/*generate incorrect ofcolor variable*/
gen ofcolor = 0
replace ofcolor = 1 if apracex == 2 | apracex == 3 | apracex == 4

/*generate the market variable*/
gen market = substr(control,1,2)

/*generate race variables*/
	gen racecat2 = apracex==2
	label variable racecat2 "African American"

	gen racecat3 = apracex==3 
	label variable racecat3 "Hispanic"

	gen racecat4 = apracex==4 
	label variable racecat4 "Asian"

	gen racecat5 = apracex==5 
	label variable racecat5 "Other Race"

/*generate correct ofcolor variable*/
	gen newofcolor = 0
	replace newofcolor = 1 if apracex == 2 | apracex == 3 | apracex == 4 | apracex == 5

/*destring everything*/

replace savlbadx = "." if savlbadx == "NA"
replace sapptamx = "." if sapptamx == "NA"
replace dpmtexpx = "." if dpmtexpx == "NA"
replace aleasetpx = "." if aleasetpx  == "NA"
replace acarownx = "." if acarownx  == "NA"

replace acarownx = "." if acarownx  == "NA"

global VARS "whitehi_rec ofcolor w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad  sequencexx monthx   arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx "

foreach var in $VARS {
	cap replace `var' = "." if `var' == ""
	cap destring `var', replace force
}

/*corrected city*/
	qui do "${CODE}/table_7_city_name_cleaner.do"
	
/*--------------------------------------*/
/*regressions*/
/*--------------------------------------*/	

/*loop over stuff - tables by clustering, panels by depvar, cols by corrections*/

global CLUSTER "control market"
global DEPVAR "ofcolor race*"

global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad"
global ABSVARSSAME "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx "

global depvar_1 = "ofcolor"
global depvar_2 = "racecat*"

foreach cluster in $CLUSTER {
	
	forvalues d =1/2 {
		
		/*string for esttab tables*/
		local coltab_`d'_`cluster' = " "
		
		forvalues cols = 1/5 {
			
			if inlist(`cols',3,4,5) &  `d'==1 {
				local depvaruse = "newofcolor"
			}
			else{
				local depvaruse = "${depvar_`d'}"
			}
			if inlist(`cols',1,3) {
				local geofe = "hcityx"
			}
			else if inlist(`cols',2,4){
				local geofe = "temp_city"
			}
			else {
				local geofe = "hzip_rec"
			}
			
			disp " "
			disp "depvar is `depvaruse'"
			disp "geofe is `geofe'"
			disp "cluster is `cluster'"
			reghdfe whitehi_rec `depvaruse' ${CONTVARS}, absorb(${ABSVARSSAME} `geofe') cluster(`cluster') keepsingle
			eststo clus_`cluster'_dep_`d'_col_`cols'
			qui estadd local share_white "No", replace
			qui estadd local ln_price "No", replace
			qui estadd local race_compo "No", replace
			qui estadd local pov_share "No", replace

			local coltab_`d'_`cluster' = " `coltab_`d'_`cluster'' clus_`cluster'_dep_`d'_col_`cols'  "	
		}
	}
	
	disp "*******************************************************"
	
}

/*build the tables*/

global depvar_1 = "ofcolor newofcolor"
global depvar_2 = "racecat*"

foreach cluster in $CLUSTER {
	
	forvalues d =1/2 {
			
		esttab `coltab_`d'_`cluster''  ///
		using "${OUTPUT}/table7_d`d'_clust_`cluster'.tex", ///
		replace booktabs label ///
		mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name \& Correct Race" "Zip Code FE",pattern(1 1 1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title( Discriminatory Steering and Neighborhood Racial Composition by Income (Panel `d')) ///
		alignment(c) page(dcolumn) nomtitle ///
		se star(* 0.10 ** 0.05 *** 0.01) ///
		s(share_white ln_price race_compo pov_share N r2_a, ///
		label("Share White Advert Home" "ln(price), advertised home" "Racial composition, advertised home" "Poverty Share Advert Home" "Observations" "Adjusted R$^2$")) ///
		keep(${depvar_`d'})
				
	}
	
}
