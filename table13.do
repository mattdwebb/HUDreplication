/* Stata Do File for Table 13 */
/* Written by: Shi Chen */
/* Updated on Aug 14, 2024 */

clear
import delimited "${DATA}/HUDprocessed_JPE_census_042021.csv", bindquote(strict)

/*-------------------------------------*/
/*---- Cleaning, labelling variables --*/
/*-------------------------------------*/

qui gen market = substr(control,1,2)

/* original `ofcolor' is incorrectly generated */
qui gen ofcolor = 0
qui replace ofcolor = 1 if aprace == 2 | aprace == 3 | aprace == 4
qui label variable ofcolor "Racial Minority"

/* rename for consistency */
ren apracex aprace

/* treat "other race" as a seperate group */
qui gen othrace = 0
qui replace othrace = 1 if aprace == 5
qui label variable othrace "Other Race"

qui label define race 1 "White" 2 "African American" 3 "Hispanic" 4 "Asian" 5 "Other Race"
qui label values aprace race

global DESVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad sequencex monthx sapptamx algncurx aelng1x dpmtexpx agex aleasetpx acarownx elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec logadprice"
	
foreach var in $DESVARS {
	qui cap replace `var' = "." if `var' == "NA" | `var' == ""
	qui cap destring `var', replace force
}

/* generate differences */
gen dif_esadrace = elementary_school_score_rec - elementary_school_score_ad
gen dif_skadrace = skill_rec - skill_ad

/*-------------------------------------*/
/*---- Getting correct city names -----*/
/*-------------------------------------*/

do "${CODE}/data_cleaner.do"

/*-------------------------------------*/
/*---- Regressions --------------------*/
/*-------------------------------------*/

global CLUSTER "control market"
global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "sequencexx monthx market arelate2x sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"
global depvar_1 = "ofcolor"
global depvar_2 = "i.aprace"
global tvar_1 = "dif_esadrace"
global tvar_2 = "dif_skadrace"


forvalues t = 1/2 {
		foreach cluster in $CLUSTER {
			forvalues d = 1/2 {
				local ct_`t'_`d'_`cluster' = " "
				forvalues cols = 1/4 {
					if inlist(`cols',3,4) & `d'==1 {
						local depvaruse = "ofcolor othrace"
					}
					else {
						local depvaruse = "${depvar_`d'}"
					}
					if inlist(`cols',1,2,3) {
						local geofe = "hcity"
					}
					else {
						local geofe = "temp_city"
					}
					local tvaruse = "${tvar_`t'}"
					disp " "
					disp as text "Dep. Var. is: " as result "`tvaruse'" 
					disp as text "Indep. Var. is: " as result "`depvaruse'"
					disp as text "Geo. FE is: " as result "`geofe'"
					if `cols'==1 {
						disp as text "Clusterd by: " as result "not clustered"
						reghdfe `tvaruse' `depvaruse' ${CONTVARS}, absorb(${ABSVARSSAME} `geofe') keepsingle
					}
					else {
						disp as text "Clusterd by: " as result "`cluster'"
						reghdfe `tvaruse' `depvaruse' ${CONTVARS}, absorb(${ABSVARSSAME} `geofe') keepsingle cluster(`cluster')
					}
					matrix hdfe = e(dof_table)
					local numGeofe = hdfe[rowsof(hdfe),1]
					qui estadd local numCiti "`numGeofe'", replace
					qui eststo s`t'_cl_`cluster'_dp_`d'_co_`cols'
					local ct_`t'_`d'_`cluster' = " `ct_`t'_`d'_`cluster'' s`t'_cl_`cluster'_dp_`d'_co_`cols' "
				}
			}
		di _dup(60) as text "*"
		}
	}

/*-------------------------------------*/
/*---- Export Results to LaTeX --------*/
/*-------------------------------------*/

global depvar_1 = "ofcolor othrace"
global depvar_2 = "2.aprace 3.aprace 4.aprace 5.aprace"
	
forvalues t = 1/2 {
	foreach cluster in $CLUSTER {
		forvalues d = 1/2 {
			local tvar = "${tvar_`t'}"
			esttab `ct_`t'_`d'_`cluster'' ///
			using "${OUTPUT}/table13_`t'_`d'_`cluster'.tex", ///
			cells(b(fmt(a4) star) se(fmt(a4) par) ci(fmt(a4) par)) collabels(none) ///
			replace booktabs label ///
			mgroups("Original Results" "Original Data" "Correct Race" "Updated City Name \& Correct Race",pattern(1 1 1 1) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			title(Neighbourhood Attributes as `tvar', Clustered at `cluster') ///
			alignment(c) page(dcolumn) nomtitle ///
			star(* 0.10 ** 0.05 *** 0.01) ///
			s(N r2_a numCiti, ///
			label("Observations" "Adjusted R$^2$" "Number of cities")) ///
			keep(${depvar_`d'})
		}
	}
}
