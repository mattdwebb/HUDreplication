/* Stata Do File for Table 5 */
/* Originally written by: Shi Chen and Anthony McCanny */
/* Updated by Shi Chen on Aug 14, 2024 */

clear
import delimited "${DATA}/adsprocessed_JPE.csv", bindquote(strict)

/*-------------------------------------*/
/*---- Cleaning, labelling variables --*/
/*-------------------------------------*/

qui gen show = stotunit
qui gen home_av = savlbad
qui replace home_av = "." if home_av == "NA" | home_av == "-1"

qui gen market = substr(control,1,2)

/* original `ofcolor' is incorrectly generated */
qui gen ofcolor = 0
qui replace ofcolor = 1 if aprace == 2 | aprace == 3 | aprace == 4
qui label variable ofcolor "Racial Minority"

/* treat "other race" as a seperate group */
qui gen othrace = 0
qui replace othrace = 1 if aprace == 5
qui label variable othrace "Other Race"

qui label define race 1 "White" 2 "African American" 3 "Hispanic" 4 "Asian" 5 "Other Race"
qui label values aprace race

global DESVARS "show home_av w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice sequencex month arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown"

foreach var in $DESVARS {
	qui cap replace `var' = "." if `var' == "NA" | `var' == ""
	qui cap destring `var', replace force
}

qui replace show = . if show < 0
qui replace home_av = 0 if home_av > 1 & home_av != .
	
/*-------------------------------------*/
/*---- Getting correct city names -----*/
/*-------------------------------------*/

do "${CODE}/data_cleaner.do"

/*-------------------------------------*/
/*---- Regressions --------------------*/
/*-------------------------------------*/	

global CLUSTER "control market"
global CONTVARS "w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "control sequencex month market arelate2 hhmtype sapptam tsexx thhegai tpegai thighedu tcurtenr algncur aelng1 dpmtexp amovers age aleasetp acarown"
global depvar_1 = "ofcolor"
global depvar_2 = "i.aprace"
global tvar_1 = "show"
global tvar_2 = "home_av"

forvalues t = 1/2 {
	foreach cluster in $CLUSTER {
		forvalues d = 1/2 {
			local ct_`t'_`d'_`cluster' = " "
			forvalues cols = 1/3 {
				if inlist(`cols',2,3) & `d'==1 {
					local depvaruse = "ofcolor othrace"
				}
				else {
					local depvaruse = "${depvar_`d'}"
				}
				if inlist(`cols',1,2) {
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
				disp as text "Clusterd by: " as result "`cluster'"
				reghdfe `tvaruse' `depvaruse' ${CONTVARS}, absorb(${ABSVARSSAME} `geofe') keepsingle cluster(`cluster')
				qui eststo s`t'_cl_`cluster'_dp_`d'_co_`cols'
				qui estadd local ln_price "Yes", replace
				qui estadd local race_compo "Yes", replace
				matrix hdfe = e(dof_table)
				local numGeofe = hdfe[rowsof(hdfe),1]
				qui estadd local numCiti "`numGeofe'", replace
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
			using "${OUTPUT}/table5_`t'_`d'_`cluster'.tex", ///
			cells(b(fmt(a4) star) se(fmt(a4) par) ci(fmt(a4) par)) collabels(none) ///
			replace booktabs label ///
			mgroups("Original Data" "Correct Race Only" "Updated City Name \& Correct Race",pattern(1 1 1) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			title(Differences in `tvar', Clustered at `cluster') ///
			alignment(c) page(dcolumn) nomtitle ///
			starlevels(* 0.10 ** 0.05 *** 0.01) ///
			s(ln_price race_compo N r2_a numCiti, ///
			label("ln(price), advertised home" "Racial composition, advertised home" "Observations" "Adjusted R$^2$" "Number of cities")) ///
			keep(${depvar_`d'})
		}
	}
}
