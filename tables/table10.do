/* Stata Do File for Table 10 */
/* Written by: Matthew D. Webb */
/* Updated: September 4, 2023, by Anthony McCanny */

clear

/*----------------*/
/* FIRST DATA SET */
/*----------------*/

clear

// import data
import delimited "${DATA}/table10_2_mom.csv", bindquote(strict)

tostring zip_ad, replace

// clean city names
do "${CODE}/data_cleaner.do"	

/*generate correct correct ofcolor and aprace variable*/
	gen noc = ofcolor
	replace noc = . if apracex == 5
	gen nrace = apracex
	replace nrace = . if apracex == 5

/*generate the market variable*/
drop market
gen market = substr(control,1,2)

/*Label Race variable*/

recode apracex (1=1 "White") (2=2 "African American") (3=3 "Hispanic") (4=4 "Asian") (5=5 "Other Race Categories"), gen(race)

/*Rename variables*/

	rename mn_avg_ol_elem_rec score_rec
	rename mn_avg_ol_elem_ad score_ad
	
	rename ofcolor oc
	drop race
	rename apracex race

save "${OUTPUT}\Table10_adjustedcities_score.dta", replace

// Original and New Race Categories
global CLUSTER "control market"
global DVAR "oc noc race nrace"

global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

global YVARS "ranking skill sf"  /*--stems */ 

foreach dvar in $DVAR {
	foreach cluster in $CLUSTER {
	
		qui reghdfe score_rec i.`dvar' score_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice, absorb(control sequencex monthx hcity arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex) keepsingle cluster(`cluster')
		qui eststo score_`dvar'_`cluster'
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace

	}
}

// Adjusted City Names with Original and New Race Categories

foreach dvar in $DVAR {
	foreach cluster in $CLUSTER {
	
		qui reghdfe score_rec i.`dvar' score_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice, absorb(control sequencex monthx temp_city arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex) keepsingle cluster(`cluster')
		qui eststo score_`dvar'_`cluster'_ca:
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace

	}
}


// Geographical Controls for Zip Code with New Race Categories
global DVAR "noc nrace"

foreach dvar in $DVAR {
	foreach cluster in $CLUSTER {
	
		qui reghdfe score_rec i.`dvar' score_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice, absorb(control sequencex monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex hzip) keepsingle cluster(`cluster')
		qui eststo score_`dvar'_`cluster'_zip 
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace

	}
}


/*-----------------*/
/* SECOND DATA SET */
/*-----------------*/

clear

// import data
import delimited "${DATA}/table10_mom.csv", bindquote(strict)

tostring zip_ad, replace

// clean city names
do "${CODE}/data_cleaner.do"

/*generate the market variable*/
drop market
gen market = substr(control,1,2)

/*Label Race variable*/

recode apracex (1=1 "White") (2=2 "African American") (3=3 "Hispanic") (4=4 "Asian") (5=5 "Other Race Categories"), gen(race)


/*generate correct correct ofcolor and aprace variable*/
	gen noc = ofcolor
	replace noc = . if apracex == 5
	gen nrace = apracex
	replace nrace = . if apracex == 5
	
/* Rename Elementary school ranking and single family variable*/

	rename elementary_school_score_rec ranking_rec
	rename elementary_school_score_ad ranking_ad
	
	rename singlefamily_rec sf_rec
	rename singlefamily_ad sf_ad
	
	rename ofcolor oc
	drop race
	rename apracex race
	
	
/*----------------------------------------------*/
/*regressions original and new race categories */
/*----------------------------------------------*/	

/*loop over stuff - tables by clustering, panels by depvar, cols by corrections*/

global CLUSTER "control market"
global DVAR "oc noc race nrace"

global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

global YVARS "ranking skill sf"  /*--stems */ 

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		qui reghdfe `yvar'_rec i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} hcity) keepsingle cluster(`cluster')
		qui eststo `yvar'_`dvar'_`cluster'
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}


/*------------------------------------------------------------*/
/*regressions adjusted city names new and old race categories */
/*------------------------------------------------------------*/	

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		qui reghdfe `yvar'_rec i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} temp_city) keepsingle cluster(`cluster')
		qui eststo `yvar'_`dvar'_`cluster'_ca
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}


/*------------------------------------------------------------*/
/*regressions ZIP */
/*------------------------------------------------------------*/	

global DVAR "noc nrace"

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		qui reghdfe `yvar'_rec i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} hzip) keepsingle cluster(`cluster')
		eststo `yvar'_`dvar'_`cluster'_zip
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

global YVAR "score ranking skill sf"

foreach yvar in $YVAR {
	foreach cluster in $CLUSTER {
	
	esttab `yvar'_oc_`cluster' `yvar'_noc_`cluster' `yvar'_oc_`cluster'_ca `yvar'_noc_`cluster'_ca `yvar'_noc_`cluster'_zip using "${OUTPUT}/row1_`cluster'_`yvar'.tex" ///
	, b(%8.3f) se(%8.3f) ///
	replace booktabs label ///
		mgroups("Original Data" "Correct Race Only" "Updated City Name Only" "Updated City Name and Correct Race" "Zip Code FE",pattern(1 1 1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title(School Quality and Neighbourhood Safety: Elementary School Test Scores (Panel A)) ///
		alignment(c) page(dcolumn) nomtitle ///
		se star(* 0.10 ** 0.05 *** 0.01) ///
		s(ln_price race_compo ad_home N r2_a, ///
		label("ln(price) advertised home" "Racial composition, advertised home" "Outcome, advertised home" "Observations" "Adjusted R$^2$")) ///
		
	}
		
}

foreach yvar in $YVAR {
	foreach cluster in $CLUSTER {
	
	esttab `yvar'_race_`cluster' `yvar'_nrace_`cluster' `yvar'_race_`cluster'_ca `yvar'_nrace_`cluster'_ca `yvar'_nrace_`cluster'_zip using "${OUTPUT}/row2_`cluster'_`yvar'.tex" ///
	, b(%8.3f) se(%8.3f) ///
	replace booktabs label ///
		mgroups("Original Data" "Correct Race Only" "Updated City Name Only" "Updated City Name and Correct Race" "Zip Code FE",pattern(1 1 1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title(School Quality and Neighbourhood Safety: Elementary School Test Scores (Panel A)) ///
		alignment(c) page(dcolumn) nomtitle ///
		se star(* 0.10 ** 0.05 *** 0.01) ///
		s(ln_price race_compo ad_home N r2_a, ///
		label("ln(price) advertised home" "Racial composition, advertised home" "Outcome, advertised home" "Observations" "Adjusted R$^2$")) ///
		
	}
		
}


































