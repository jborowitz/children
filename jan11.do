clear matrix
clear
clear mata
set mem 100m
set maxvar 15000
set more off
use twoSibsPared.dta
drop *02
drop if race > 2
gen ageatbirth = parentage - chage
gen teenmom = ageatbirth <=20
gen blackXurban = black * urban
gen blackXmarriedrateall = black * marriedrateall
gen blackXmarriedraterace = black * marriedraterace
drop if parentage < 0
label variable black "Black"
label variable blackXmarriedraterace "Black * % Married"
label variable marriedraterace "% Married"
label variable Ffracbasic97 "Public/All Time"
label variable Ffracrec97 "Public/All Time"
label variable Ffraceduc97 "Public/All Time"
label variable urban "Urban Area"
/*local y "MfracTOT97 "*/
/*local unitlist "MfracTOT97  Mfracbasic97  Mfracrec97  Mfractravel97 Mfraceduc97"*/
/*local unitlist "MTOT97  Mbasic97  Mrec97  Mtravel97 Meduc97"*/
/*local unitlist "FTOT97  Fbasic97  Frec97  Ftravel97 Feduc97"*/
/*local unitlist "BfracTOT97  Bfracbasic97  Bfracrec97  Bfractravel97 Bfraceduc97"*/
/*local unitlist "BTOT97  Bbasic97  Brec97  Btravel97 Beduc97"*/
local interactions "i.famsize i.ageyoungest "
local tableinteractions `"indicate("Dummies = *famsize *ageyoungest ", labels("X" " "))"'
local tableiqr "stats(iqr sd1 r2 N)"
local iqrlabel "IQR of R.T. Effect"
local sd1label "1 sd of R.T. Effect"
local percentiles "25 75"
local unitlist "Ffracrec97 Mfracrec97 Bfracrec97"
/*local unitlist "MTOT97"*/
local x1 "black marriedraterace blackXmarriedraterace "
local x2 "black marriedraterace blackXmarriedraterace chagem chagem2 hsdip cdip fhsdip fcdip  urban parentage income "
local x3 "black marriedraterace blackXmarriedraterace hsdip cdip fhsdip fcdip income "
local x4 "black marriedraterace blackXmarriedraterace hsdip cdip fhsdip fcdip income urban"
local x5 "black marriedraterace blackXmarriedraterace chagem chagem2 hsdip cdip fhsdip fcdip   parentage income "
local x6 "black marriedraterace blackXmarriedraterace chagem chagem2 hsdip cdip fhsdip fcdip  urban parentage income "
local X1 "`x1'"
local X2 "`x2'"
local X3 "`x3' `interactions'"
local X4 "`x4' `interactions'"
local X5 "`x5' `interactions'"
local X6 "`x6' `interactions'"
/*local X1 "RT "*/
/*local X2 "RT cdip hsdip fcdip fhsdip bothschool noschool males females nonwhite parentage meanBTOT97 income i.famsize i.yearsdiff i.ageyoungest"*/
/*local X3 "RT cdip hsdip fcdip fhsdip bothschool noschool males females nonwhite parentage  i.famsize i.yearsdiff i.ageyoungest"*/
/*local X4 "cdip hsdip fcdip fhsdip bothschool noschool males females nonwhite parentage meanBTOT97 income  i.famsize i.yearsdiff i.ageyoungest"*/
/*local X5 "cdip hsdip fcdip fhsdip bothschool noschool males females nonwhite parentage  i.famsize i.yearsdiff i.ageyoungest"*/
local RTvar1 "black asian"
local RTvar1 "black hispanic"
local weight "[pweight=CH97PRWT] "
local RTvarlist "RTvar1 "
local keeplist "`x6'"
local xlist "X1 X2 X3 X4 X5 X6"
local mainrestriction " mschooling97 < 97 & dadhead & married & race < 3 & famsize < 6 "
/*local mainrestriction " mschooling97 < 97   &  race < 3 & famsize < 6"*/
/*fvset base 3 famsize*/
fvset base 6 statefip
local tablist  ""

**********************************************************************
* Start X Explore
**********************************************************************
foreach unit of local unitlist{
        eststo clear
            local name `unit'
            local tablist "`tablist' `name'"
            foreach X of local xlist{
                        /*local y `unit'`person'`category'*/
                        local y `unit'
                        di "Running command: reg `y'   ``X'' if `mainrestriction' `weight', robust"
                        di "``X''"
                        /*eststo: reg `y'  ``X'' if `mainrestriction' `weight', vce(cluster family)*/
                        eststo: reg `y'  ``X'' if `mainrestriction' `weight', robust
                        /*quietly _pctile   if `mainrestriction', percentiles(`percentiles')*/
                        /*matrix define beta = e(b)*/
                        /*local b = beta[1,1]*/
                        /*local iqrvar = (`r(r2)'-`r(r1)') * `b'*/
                        /*quietly sum  if `mainrestriction', detail*/
                        /*local sd1var = `r(sd)' * `b'*/
                        /*estadd scalar iqr = `iqrvar' */
                        /*estadd scalar sd1 = `sd1var' */
            }
        esttab using `name'ols.tex, se r2 keep(`keeplist' ) `tableinteractions' label fragment replace longtable
            esttab using `name'ols.txt, se r2 keep(`keeplist' ) `tableinteractions' label fragment replace
            !sed -i 's/o2y1/$ y_1 - o_2$ /g' `name'ols.tex
            !sed -i 's/r2/$ R^2 $/' `name'ols.tex
            !sed -i '/(1)/d' `name'ols.tex
            !sed -i 's/1em/.25em/' `name'ols.tex
            !sed -i 's/sd1/`sd1label'/' `name'ols.tex
            !sed -i 's/%/\\%/' `name'ols.tex
            !sed -i 's/sd1 \{15\}/`sd1label'/' `name'ols.txt
            !sed -i 's/iqr/`iqrlabel'/' `name'ols.tex
            !sed -i 's/iqr \{15\}/`iqrlabel'/' `name'ols.txt
            !cp `name'ols.tex results
}

**********************************************************************
* End X Explore
**********************************************************************
foreach  tab  of local tablist{
!echo Outcome `tab'
!cat `tab'ols.txt
}
!echo `tablist'
