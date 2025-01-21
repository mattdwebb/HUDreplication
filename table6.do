/* Stata Do File for Table 6 */
/* Written by: Matthew D. Webb */
/* Created: July 23, 2024 */
/* Modified: Sept 27, 2024 */

clear

// import data
/*set path here*/
*global PATH "C:\Users\mattw\Dropbox\HuD_Replication\"

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

/*generate incorrect ofcolor variable*/
	gen newofcolor = 0
	replace newofcolor = 1 if apracex == 2 | apracex == 3 | apracex == 4 | apracex == 5
	
	/* treat "other race" as a seperate group */
	qui gen othrace = 0
	qui replace othrace = 1 if aprace == 5
	qui label variable othrace "Other Race"




/*destring everything*/

replace savlbadx = "." if savlbadx == "NA"
replace sapptamx = "." if sapptamx == "NA"
replace dpmtexpx = "." if dpmtexpx == "NA"
replace aleasetpx = "." if aleasetpx  == "NA"
replace acarownx = "." if acarownx  == "NA"

replace acarownx = "." if acarownx  == "NA"

global VARS "whitehi_rec ofcolor w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice povrate_ad  sequencexx monthx   arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx w2012pc_rec "


foreach var in $VARS {
	cap replace `var' = "." if `var' == ""
	cap destring `var', replace force
}


/*corrected city*/
	qui do "${CODE}\data_cleaner.do"

* Run the reghdfe regression - col 3?
*reghdfe w2012pc_rec ofcolor, absorb(control sequencexx monthx hcityx market arelate2x sapptamx tsexx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx) keepsingle cluster(control)

	
/*--------------------------------------*/
/*regressions*/
/*--------------------------------------*/	

/*loop over stuff - tables by clustering, panels by depvar, cols by corrections*/

global CLUSTER "control"
global DEPVAR "ofcolor race*"

/*
global CONTVARS " b2012pc_ad a2012pc_ad hisp2012pc_ad  povrate_ad"
global ABSVARSSAME "control sequencexx monthx market arelate2x sapptamx tsexx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx "
*/
global CONTVARS " logadprice w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad  povrate_ad"
global ABSVARSSAME "control sequencexx monthx market arelate2x sapptamx tsexx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx "

/*
W5 <- felm(w2012pc_Rec ~  + logAdPrice + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad  + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
*/

global depvar_1 = "ofcolor"
global depvar_2 = "racecat*"

foreach cluster in $CLUSTER {
	
	forvalues d =1/2 {
		
		/*string for esttab tables*/
		local coltab_`d'_`cluster' = " "
		
		forvalues cols = 1/3 {
			

			*/
			if inlist(`cols',2,3) &  `d'==1 {
				local depvaruse = "newofcolor othrace"
			}
			else{
				local depvaruse = "${depvar_`d'}"
			}
			if inlist(`cols',1,2) {
				local geofe = "hcity "
			}
			else if inlist(`cols',3,3){
				local geofe = "temp_city"
			}
			
			disp " "
			disp "depvar is `depvaruse'"
			disp "geofe is `geofe'"
			disp "cluster is `cluster'"
			reghdfe w2012pc_rec `depvaruse' ${CONTVARS}, absorb(${ABSVARSSAME} `geofe') cluster(`cluster') keepsingle
			eststo clus_`cluster'_dep_`d'_col_`cols'
			qui estadd local share_white "No", replace
			qui estadd local ln_price "No", replace
			qui estadd local race_compo "No", replace
			qui estadd local pov_share "No", replace
			
			*qui levelsof(`geofe')
			matrix hdfe = e(dof_table)
			local num_levels_geofe = hdfe[rowsof(hdfe),1]
			
			*local num_levels_geofe = r(r)
			qui estadd local num_cities  =  `num_levels_geofe'

			local coltab_`d'_`cluster' = " `coltab_`d'_`cluster'' clus_`cluster'_dep_`d'_col_`cols'  "	
			
		}
	}
	
	disp "*******************************************************"
	
}

/*build the tables*/

global depvar_1 = "ofcolor newofcolor othrace"
global depvar_2 = "racecat* "

foreach cluster in $CLUSTER {
	
	forvalues d =1/2 {
			
		esttab `coltab_`d'_`cluster''  ///
		using "${OUTPUT}/table6_d`d'_clust_`cluster'.tex", ///
		replace booktabs label ///
		cells(b(fmt(a4) star) se(fmt(a4) par) ci(fmt(a4) par)) collabels(none) ///
		mgroups("Original Data" "Updated City Name Only"  "Updated City Name \& Correct Race" ,pattern(1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title( Discriminatory Steering and Neighborhood Racial Composition by Income (Panel `d')) ///
		alignment(c) page(dcolumn) nomtitle ///
		se star(* 0.10 ** 0.05 *** 0.01) ///
		s(share_white ln_price race_compo pov_share N r2_a num_cities, ///
		label("Share white, advertised home" "ln(price), advertised home" "Racial composition, advertised home" "Poverty Share Advert Home" "Observations" "Adjusted R$^2$" "Number of Cities")) ///
		keep(${depvar_`d'})
				
	}
	
}

