/* Stata Do File for Table 8 */
/* Written by: Matthew D. Webb */
/* Updated: July 30th, 2024, by Sunny Karim */


/*----------------*/
/* Cleaning dataset */
/*----------------*/

clear

// import data
use "${DATA}/Table 8.dta",replace
rename *, lower // Changes variables to lower case

/*generate correct correct ofcolor and aprace variable*/
	gen noc = ofcolor
	replace noc = 2 if apracex == 5
	gen nrace = apracex

/*generate the market variable*/
	drop market
	gen market = substr(control,1,2)

/*Label Race variable*/

	recode apracex (1=1 "White") (2=2 "African American") (3=3 "Hispanic") (4=4 "Asian") (5=5 "Other Race Categories"), gen(race)

/*Rename variables*/

	rename povrate_rec pov
	rename povrate_ad pov_ad
	rename elementary_school_score_rec ranking
	rename elementary_school_score_ad ranking_ad
	
	
	rename ofcolor oc
	drop race
	rename apracex race
	
tostring zip_ad, replace

// clean city names
do "${CODE}/data_cleaner.do"


save "${OUTPUT}\Table8_adjustedcities_score.dta", replace

	
/*----------------------------------------------*/
/*regressions original and new race categories */
/*----------------------------------------------*/	

/*loop over stuff - tables by clustering, panels by depvar, cols by corrections*/

global CLUSTER "control market"
global DVAR "oc noc race nrace"

global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

global YVARS "ranking pov"  /*--stems */ 


foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
	
		qui reghdfe `yvar' i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} hcity) keepsingle cluster(`cluster')

		// Extract number of levels of city variable
        	matrix hdfe = e(dof_table)
		local geo_fe = "hcity"
        	local num_levels_geofe = hdfe[rowsof(hdfe),1]
        	qui estadd scalar num_cities = `num_levels_geofe'
		


		qui eststo `yvar'_`dvar'_`cluster'
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}


/*------------------------------------------------------------*/
/*regressions adjusted city names new and new race categories */
/*------------------------------------------------------------*/	

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		qui reghdfe `yvar' i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} temp_city) keepsingle cluster(`cluster')

		// Extract number of levels of city variable
        	matrix hdfe = e(dof_table)
        	local num_levels_geofe = hdfe[rowsof(hdfe),1]
        	qui estadd scalar num_cities = `num_levels_geofe'


		qui eststo `yvar'_`dvar'_`cluster'_ca
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}




/*------------------------------------------------------------*/
/*Generating Outputs */
/*------------------------------------------------------------*/	

label variable oc "Racial Minority"
label variable noc "Racial Minority"

global YVAR "ranking pov"


foreach yvar in $YVAR {
	foreach cluster in $CLUSTER {
	
	esttab `yvar'_oc_`cluster' `yvar'_noc_`cluster' `yvar'_noc_`cluster'_ca using "${OUTPUT}/row1_`cluster'_`yvar'.tex" ///
	, b(%8.3f) se(%8.3f) ///
	replace booktabs label ///
		mgroups("Original Data" "Correct Race Only" "Updated City Name and Correct Race",pattern(1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title(School Quality and Neighbourhood Safety: Housing Search Platform (Elementary School) (Panel A)) ///
		alignment(c) page(dcolumn) nomtitle ///
		cells("b(star fmt(4))" se ci(fmt(4) par)) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		s(ln_price race_compo ad_home N r2_a num_cities, ///
		label("ln(price) advertised home" "Racial composition, advertised home" "Outcome, advertised home" "Observations" "Adjusted R$^2$" "Number of Cities")) ///
		keep(`racial_minority')
		
	}
		
}

foreach yvar in $YVAR {
	foreach cluster in $CLUSTER {
	
	esttab `yvar'_race_`cluster' `yvar'_nrace_`cluster' `yvar'_nrace_`cluster'_ca  using "${OUTPUT}/row2_`cluster'_`yvar'.tex" ///
	, b(%8.3f) se(%8.3f) ///
	replace booktabs label ///
		mgroups("Original Data" "Correct Race Only" "Updated City Name and Correct Race",pattern(1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title(American Community Survey: Poverty Rate (Panel B)) ///
		alignment(c) page(dcolumn) nomtitle ///
		cells("b(star fmt(4))" se ci(fmt(4) par)) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		s(ln_price race_compo ad_home N r2_a num_cities, ///
		label("ln(price) advertised home" "Racial composition, advertised home" "Outcome, advertised home" "Observations" "Adjusted R$^2$" "Number of Cities")) ///
		
	}
		
}
