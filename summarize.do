clear mata
clear matrix
clear
set mem 100m
set maxvar 10000
set more off
use twoSibsPared.dta
/*duplicates drop family, force*/
/*use pcgPared*/
/*label variable odiffBTOT97 "Both Parents: $ y_2-o_1$ "*/
/*label variable meanBTOT97 "Both Parents: $ (y_2+o_1)/2$ "*/
quietly sum RT if dadhead, detail
/*gen highRT =  RT >= .369*/
gen highRT =  RT >= .5
gen badRT = RT > .17 & RT < .18
gen kindahighRT = RT >.1
replace badRT = 1 if RT > 2.42 & RT < 2.43
replace badRT = 1 if RT > 3.61 & RT < 3.62
label values highRT highRT
label define highRT 1 "High $\theta$" 0 "Low $\theta$"
label variable BTOT97 "All"
label variable Bbasic97 "Basic"
label variable Brec97 "Rec."
label variable Btravel97 "Travel"
label variable Beduc97 "Educ."
**********************************************************************
  * Summary Statistics for children
eststo clear
global X "BTOT97 Bbasic97 Brec97 Beduc97 Btravel97 selfBTOT97 selfBbasic97 selfBrec97 selfBeduc97 selfBtravel97 ageyoungest inschool RT male wifeinlf income fcdip fhsdip cdip hsdip nonwhite chagem"
global weight "[aweight=CH97PRWT]"
/*global weight ""*/
local restriction "RT != . & dadhead & selfBTOT97 != . & BTOT07 != ."
quietly estpost tabstat $X if `restriction' $weight,  by(highRT) statistics(mean sd semean min max) columns(statistics)
esttab . using summary.tex , main(mean) aux(semean) nostar nonote unstack label nomtitle varwidth(37) replace fragment nonumber
esttab . using summary.txt , main(mean) aux(semean) nostar nonote unstack label nomtitle varwidth(37) replace fragment nonumber
/*cells("mean(fmt(a3)) semean(fmt(a3))") */
  quietly count if `restriction' & highRT == 0
  local condfalse=r(N)
  quietly count if `restriction' & highRT == 1
  local condtrue=r(N)
  quietly count if `restriction'
  local total = r(N)
!sed -i 's/mean/Mean/' summary.tex
!sed -i 's/semean/S.E./' summary.tex
!sed -i '1,3s/sd/St. Dev./' summary.tex
!sed -i '1,3s/min/Min/' summary.tex
!sed -i '1,3s/max/Max/' summary.tex
!sed -i 's/\\_/_/g' summary.tex
*!sed -i 's/\\\\/\\\\\\\\/' summary.tex
  !sed -i "/^Observations/d" summary.tex
  !echo "Observations & `condfalse' & `condtrue' & `total' \\\\" >> summary.tex
cat summary.tex
**********************************************************************
/*quietly estpost tabstat $X if `restriction' $weight,  by(highRT) statistics(mean sd semean min max) columns(statistics)*/

label values highRT highRT
label drop highRT
label define highRT 1 "Risk Tolerant" 0 "Risk Averse"
local categorylist "TOT educ rec travel basic"
#delimit;
foreach category of local categorylist{;
local mylabel : variable label B`category'97;
graph bar B`category'97 B`category'02 B`category'07 if `restriction' $weight,
over(highRT)
legend( label(1 "1997 (Avg. Age 6)") label(2 "2002 (Avg. Age 11)") 
label(3 "2007 (Avg. Age 16)")) 
ytitle("Hours per Week")
title("`mylabel' Parental Time with Child")
note("Source: PSID Child Development Supplement")
subtitle("By Father's Risk Tolerance level");
/*graph bar B`category'97 B`category'02, over(highRT) yscale(range(20 40))*/
graph export `category'.eps, replace;
};
#delimit cr
