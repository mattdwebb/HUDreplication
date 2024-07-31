clear

// import data
import delimited "${DATA}/HUDprocessed_JPE_census_042021.csv", bindquote(strict)

/*-------------------------------------*/
/*---- Cleaning, labelling variables --*/
/*-------------------------------------*/

qui gen market = substr(control,1,2)

/* original `ofcolor' is incorrectly generated */
qui gen ofcolor = 0
qui replace ofcolor = 1 if aprace == 2 | aprace == 3 | aprace == 4
qui label variable ofcolor "Racial Minority"

/* treat "other race" as a separate group */
qui gen othrace = 0
qui replace othrace = 1 if aprace == 5
qui label variable othrace "Other Race"

/* racial groups */
qui gen racecat = aprace
qui replace racecat = . if racecat == 5

qui label define race 1 "White" 2 "African American" 3 "Hispanic" 4 "Asian" 5 "Other Race"
qui label values aprace race
qui label values racecat race

global DESVARS "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad sequencex monthx sapptamx algncurx aelng1x dpmtexpx agex aleasetpx acarownx elementary_school_score_ad elementary_school_score_rec skill_ad skill_rec logadprice hhmtypex savlbadx stotunit_rec povrate_rec"
	
foreach var in $DESVARS {
	qui cap replace `var' = "." if `var' == "NA" | `var' == ""
	qui cap destring `var', replace force
}

/* generate differences */
gen low_povrate = 0
replace low_povrate = 1 if povrate_rec < 0.1
keep if povrate_ad < 0.1
egen tag = tag(testerid control)
egen cnt = total(tag), by(control)
keep if cnt > 1


/*-------------------------------------*/
/*---- Getting correct city names -----*/
/*-------------------------------------*/

do "${CODE}/data_cleaner.do"

/*-------------------------------------*/
/*---- Regressions --------------------*/
/*-------------------------------------*/

global CLUSTER "control market"
global CONTVARS "povrate_ad w2012pc_ad b2012pc_ad a2012pc_ad hisp2012pc_ad logadprice"
global ABSVARSSAME "control sequencexx monthx market arelate2x hhmtypex savlbadx stotunit_rec sapptamx tsexxx thhegaix tpegaix thighedux tcurtenrx algncurx aelng1x dpmtexpx amoversx agex aleasetpx acarownx"
global depvar = "i.racecat"
global tvar = "low_povrate"

foreach cluster in $CLUSTER {
	local ct_`cluster' = " "
	forvalues cols = 1/4 {
		if inlist(`cols',1,2) {
			local depvaruse = "i.apracex"
		}
		else {
			local depvaruse = "${depvar}"
		}
		if inlist(`cols',1,3) {
			local geofe = "hcity"
		}
		else {
			local geofe = "temp_city"
		}
		disp " "
		disp as text "Dep. Var. is: " as result "`tvaruse'" 
		disp as text "Indep. Var. is: " as result "`depvaruse'"
		disp as text "Geo. FE is: " as result "`geofe'"
		disp as text "Clustered by: " as result "`cluster'"
		reghdfe ${tvar} `depvaruse' ${CONTVARS}, absorb(${ABSVARSSAME} `geofe') keepsingle cluster(`cluster')
		qui eststo s_cl_`cluster'_dp_co_`cols'
		local ct_`cluster' = " `ct_`cluster'' s_cl_`cluster'_dp_co_`cols' "
	}
	disp as text "*******************************************************"
}


/*-------------------------------------*/
/*---- Export Results to LaTeX --------*/
/*-------------------------------------*/

global depvar = "2.apracex 3.apracex 4.apracex 2.racecat 3.racecat 4.racecat"
	
foreach cluster in $CLUSTER {
	local tvar = "${tvar}"
	esttab `ct_`cluster'' ///
	using "${CODE}/table11_`cluster'.tex", ///
	replace booktabs label ///
	mgroups("Original Data" "Updated City Name Only" "Correct Race Only" "Updated City Name \& Correct Race",pattern(1 1 1 1) ///
	prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	title(Neighbourhood Attributes as `tvar', Clustered at `cluster') ///
	varwidth(25) ///
	alignment(c) page(dcolumn) nomtitle ///
	se star(* 0.10 ** 0.05 *** 0.01) ///
	s(N r2_a, ///
	label("Observations" "Adjusted R$^2$")) ///
	keep(${depvar})
}