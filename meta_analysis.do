/*Stata Do File for Meta Analysis Figure*/
/*Written by: Matthew D. Webb */
/*Created: October 2024*/
/*Current Version: January 31, 2025*/

clear all

import delimited "${DATA}/meta_comparison.csv"

	   
gen pold = 2*min(normprob(original/varianceoriginal),1-normprob(original/varianceoriginal))

gen pnew = 2*min(normprob(updated/varianceupdated),1-normprob(updated/varianceupdated))

gen cube_root_pnew = pnew^(1/3)

gen cube_root_pold = pold^(1/3)

	   

	   
	   // Create scatter plot for African coefficient with labeled lines
twoway (scatter cube_root_pnew cube_root_pold if coeff == "African" & mother == 1, ///
        mcolor(green) msymbol(circle) ///
        legend(label(1 "Mother = 1"))) ///
       (scatter cube_root_pnew cube_root_pold if coeff == "African" & mother == 0, ///
        mcolor(blue) msymbol(triangle) ///
        legend(label(2 "Mother = 0"))) ///
       (line cube_root_pold cube_root_pold if inrange(original, -1, 1), ///
        lcolor(black) lpattern(dot) legend(label(3 "45-Degree Line"))) ///
       , ///
       legend(order(1 2 3) position(5) ring(0) colgap(2)) ///
       xlabel(,nolabels) ///
       ylabel(,nolabels) ///
       yscale(range(0 1.00)) ///
       yline(.21544347) xline(.21544347) ///
       yline(.36840315) xline(.36840315) ///
       yline(.46415888) xline(.46415888) ///
       xtitle("Cube Root of Original P value") ytitle("Cube Root of New P Value") ///
       title("P Values: African American") ///
       text(0.19 0.05 "1%" , place(east) size(small)) ///
       text(0.33 0.05 "5%", place(east) size(small)) ///
       text(0.44 0.05 "10%", place(east) size(small)) ///
       text(0.05 0.19 "1%", place(north) size(small)) ///
       text(0.05 0.35 "5%", place(north) size(small)) ///
       text(0.05 0.44 "10%", place(north) size(small)) ///
       name(african_plot, replace)

	   
	   	   
	   // Create scatter plot for Asian coefficient with labeled lines
twoway (scatter cube_root_pnew cube_root_pold if coeff == "Asian" & mother == 1, ///
        mcolor(green) msymbol(circle) ///
        legend(label(1 "Mother = 1"))) ///
       (scatter cube_root_pnew cube_root_pold if coeff == "Asian" & mother == 0, ///
        mcolor(blue) msymbol(triangle) ///
        legend(label(2 "Mother = 0"))) ///
       (line cube_root_pold cube_root_pold if inrange(original, -1, 1), ///
        lcolor(black) lpattern(dot) legend(label(3 "45-Degree Line"))) ///
       , ///
       legend(order(1 2 3) position(5) ring(0) colgap(2)) ///
       xlabel(,nolabels) ///
       ylabel(,nolabels) ///
       yscale(range(0 1.00)) ///
       yline(.21544347) xline(.21544347) ///
       yline(.36840315) xline(.36840315) ///
       yline(.46415888) xline(.46415888) ///
       xtitle("Cube Root of Original P value") ytitle("Cube Root of New P Value") ///
       title("P Values: Asian") ///
       text(0.19 0.05 "1%" , place(east) size(small)) ///
       text(0.33 0.05 "5%", place(east) size(small)) ///
       text(0.44 0.05 "10%", place(east) size(small)) ///
       text(0.05 0.19 "1%", place(north) size(small)) ///
       text(0.05 0.35 "5%", place(north) size(small)) ///
       text(0.05 0.44 "10%", place(north) size(small)) ///
       name(asian_plot, replace)
	   
	   
	   
	   // Create scatter plot for Hispanic coefficient with labeled lines
twoway (scatter cube_root_pnew cube_root_pold if coeff == "Hispanic" & mother == 1, ///
        mcolor(green) msymbol(circle) ///
        legend(label(1 "Mother = 1"))) ///
       (scatter cube_root_pnew cube_root_pold if coeff == "Hispanic" & mother == 0, ///
        mcolor(blue) msymbol(triangle) ///
        legend(label(2 "Mother = 0"))) ///
       (line cube_root_pold cube_root_pold if inrange(original, -1, 1), ///
        lcolor(black) lpattern(dot) legend(label(3 "45-Degree Line"))) ///
       , ///
       legend(order(1 2 3) position(5) ring(0) colgap(2)) ///
       xlabel(,nolabels) ///
       ylabel(,nolabels) ///
       yscale(range(0 1.00)) ///
       yline(.21544347) xline(.21544347) ///
       yline(.36840315) xline(.36840315) ///
       yline(.46415888) xline(.46415888) ///
       xtitle("Cube Root of Original P value") ytitle("Cube Root of New P Value") ///
       title("P Values: Hispanic") ///
       text(0.19 0.05 "1%" , place(east) size(small)) ///
       text(0.33 0.05 "5%", place(east) size(small)) ///
       text(0.44 0.05 "10%", place(east) size(small)) ///
       text(0.05 0.19 "1%", place(north) size(small)) ///
       text(0.05 0.35 "5%", place(north) size(small)) ///
       text(0.05 0.44 "10%", place(north) size(small)) ///
       name(hispanic_plot, replace)
	   
	   
	   // Create scatter plot for Overall coefficient with labeled lines
twoway (scatter cube_root_pnew cube_root_pold if coeff == "Overall" & mother == 1, ///
        mcolor(green) msymbol(circle) ///
        legend(label(1 "Mother = 1"))) ///
       (scatter cube_root_pnew cube_root_pold if coeff == "Overall" & mother == 0, ///
        mcolor(blue) msymbol(triangle) ///
        legend(label(2 "Mother = 0"))) ///
       (line cube_root_pold cube_root_pold if inrange(original, -1, 1), ///
        lcolor(black) lpattern(dot) legend(label(3 "45-Degree Line"))) ///
       , ///
       legend(order(1 2 3) position(5) ring(0) colgap(2)) ///
       xlabel(,nolabels) ///
       ylabel(,nolabels) ///
       yscale(range(0 1.00)) ///
       yline(.21544347) xline(.21544347) ///
       yline(.36840315) xline(.36840315) ///
       yline(.46415888) xline(.46415888) ///
       xtitle("Cube Root of Original P value") ytitle("Cube Root of New P Value") ///
       title("P Values: Racial Minority") ///
       text(0.19 0.05 "1%" , place(east) size(small)) ///
       text(0.33 0.05 "5%", place(east) size(small)) ///
       text(0.44 0.05 "10%", place(east) size(small)) ///
       text(0.05 0.19 "1%", place(north) size(small)) ///
       text(0.05 0.35 "5%", place(north) size(small)) ///
       text(0.05 0.44 "10%", place(north) size(small)) ///
       name(overall_plot, replace)	   
	   
	   
	   
// Combine the four plots into one figure
graph combine african_plot asian_plot hispanic_plot overall_plot,  cols(2)

