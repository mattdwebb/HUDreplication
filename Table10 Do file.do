/*MDW - table 10*/
clear

/*set path here*/
	/* Set path to the parent folder of the local location of the git repository */
	global PATH "C:\Users\antho\OneDrive - University of Toronto\Research\Replication Games"

	global CODE "${PATH}/HUDreplication" //set the file path to the main code directory
	global DATA "${CODE}/Data" // set the file path to the data subdirectory

	cap mkdir "${PATH}/Output" // make an Output folder if it doesn't already exist
	global OUTPUT "${PATH}/Output" // set the output file path
	
	cap log close
	log using "${OUTPUT}/table10_log.txt", text replace

/*corrected city*/
	qui do "${CODE}/cleanerTable10.do"

/*---------------------------------------------*/
/*Cleaning first dataset with Adjusted Cities */
/*---------------------------------------------*/

clear

use "${OUTPUT}/Table10_2_cityadjusted.dta"

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


/*---------------------------------------------*/
/*Clearning second dataset with Adjusted Cities */
/*---------------------------------------------*/

clear

use "${OUTPUT}\Table10_cityadjusted.dta"	

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
	
/* Define Y variables */
	
global YVAR ranking skill sf

foreach yvar in $YVAR {
	save "${OUTPUT}\Table10_adjustedcities_`yvar'.dta", replace
}

	
/*----------------------------------------------*/
/*regressions original and new race categories */
/*----------------------------------------------*/	

/*loop over stuff - tables by clustering, panels by depvar, cols by corrections*/

clear

global CLUSTER "control market"
global DVAR "oc noc race nrace"

global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "control sequencex monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"

global YVARS "ranking skill sf"  /*--stems */ 

use "${OUTPUT}\Table10_adjustedcities_score.dta"

foreach dvar in $DVAR {
	foreach cluster in $CLUSTER {
	
		qui reghdfe score_rec i.`dvar' score_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice, absorb(control sequencex monthx hcityx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex) keepsingle cluster(`cluster')
		qui eststo score_`dvar'_`cluster'
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace

	}
}

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		use "${OUTPUT}\Table10_adjustedcities_`yvar'.dta", clear
		
		qui reghdfe `yvar'_rec i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} hcityx) keepsingle cluster(`cluster')
		qui eststo `yvar'_`dvar'_`cluster'
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}

disp "Race Categories complete"

/*------------------------------------------------------------*/
/*regressions adjusted city names new and old race categories */
/*------------------------------------------------------------*/	

use "${OUTPUT}\Table10_adjustedcities_score.dta", clear

foreach dvar in $DVAR {
	foreach cluster in $CLUSTER {
	
		qui reghdfe score_rec i.`dvar' score_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice, absorb(control sequencex monthx temp_city arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex) keepsingle cluster(`cluster')
		qui eststo score_`dvar'_`cluster'_ca:
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace

	}
}

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		use "${OUTPUT}\Table10_adjustedcities_`yvar'.dta", clear
		
		qui reghdfe `yvar'_rec i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} temp_city) keepsingle cluster(`cluster')
		qui eststo `yvar'_`dvar'_`cluster'_ca
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}

disp "City names complete"

/*------------------------------------------------------------*/
/*regressions ZIP */
/*------------------------------------------------------------*/	

global DVAR "noc nrace"

use "${OUTPUT}\Table10_adjustedcities_score.dta", clear

foreach dvar in $DVAR {
	foreach cluster in $CLUSTER {
	
		qui reghdfe score_rec i.`dvar' score_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice, absorb(control sequencex monthx arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx agex hzip) keepsingle cluster(`cluster')
		qui eststo score_`dvar'_`cluster'_zip 
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace

	}
}

foreach yvar in $YVARS {
	foreach cluster in $CLUSTER {
		foreach dvar in $DVAR {
		
		use "${OUTPUT}\Table10_adjustedcities_`yvar'.dta", clear
		
		qui reghdfe `yvar'_rec i.`dvar' `yvar'_ad ${CONTVARS}, absorb(${ABSVARSSAME} hzip) keepsingle cluster(`cluster')
		eststo `yvar'_`dvar'_`cluster'_zip
		qui estadd local ln_price "Yes", replace
		qui estadd local race_compo "Yes", replace
		qui estadd local ad_home "Yes", replace
		}
	}
}

disp "ZIP complete"

/*------------------------------------------------------------*/
/*Generating Outputs */
/*------------------------------------------------------------*/	

label variable oc "Racial Minority"
label variable noc "Racial Minority"

global YVAR "score ranking skill sf"

foreach yvar in $YVAR {
	foreach cluster in $CLUSTER {
	
	esttab `yvar'_oc_`cluster' `yvar'_noc_`cluster' `yvar'_oc_`cluster'_ca `yvar'_noc_`cluster'_ca `yvar'_noc_`cluster'_zip using "C:\Users\sunny\OneDrive\Desktop\Replication Games\Output\row1_`cluster'_`yvar'.tex" ///
	, b(%8.3f) se(%8.3f) ///
	replace booktabs label ///
		mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name and Correct Race" "Zip Code FE",pattern(1 1 1 1 1) ///
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
	
	esttab `yvar'_race_`cluster' `yvar'_nrace_`cluster' `yvar'_race_`cluster'_ca `yvar'_nrace_`cluster'_ca `yvar'_nrace_`cluster'_zip using "C:\Users\sunny\OneDrive\Desktop\Replication Games\Output\row2_`cluster'_`yvar'.tex" ///
	, b(%8.3f) se(%8.3f) ///
	replace booktabs label ///
		mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name and Correct Race" "Zip Code FE",pattern(1 1 1 1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		title(School Quality and Neighbourhood Safety: Elementary School Test Scores (Panel A)) ///
		alignment(c) page(dcolumn) nomtitle ///
		se star(* 0.10 ** 0.05 *** 0.01) ///
		s(ln_price race_compo ad_home N r2_a, ///
		label("ln(price) advertised home" "Racial composition, advertised home" "Outcome, advertised home" "Observations" "Adjusted R$^2$")) ///
		
	}
		
}


































