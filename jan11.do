clear matrix
clear
clear mata
set mem 100m
set maxvar 15000
set more off
use twoSibsPared.dta
drop *02
drop if race > 2
drop if parentage < 0
label variable black "Black"
label variable blackXmarriedraterace "Black * % Married"
label variable marriedraterace "% Married"
label variable Ffracbasic97 "Frac. Public"
label variable Ffracrec97 "Frac. Public"
label variable Ffraceduc97 "Frac. Public"
label variable FfracTOT97 "Frac. Public"
label variable Ffractravel97 "Frac. Public"
label variable Mfracbasic97 "Frac. Public"
label variable Mfracrec97 "Frac. Public"
label variable Mfraceduc97 "Frac. Public"
label variable MfracTOT97 "Frac. Public"
label variable Mfractravel97 "Frac. Public"
label variable Bfracbasic97 "Frac. Public"
label variable Bfracrec97 "Frac. Public"
label variable Bfraceduc97 "Frac. Public"
label variable BfracTOT97 "Frac. Public"
label variable Bfractravel97 "Frac. Public"
label variable urban "Urban Area"
/*local y "MfracTOT97 "*/
/*local unitlist "MfracTOT97  Mfracbasic97  Mfracrec97  Mfractravel97 Mfraceduc97"*/
/*local unitlist "MTOT97  Mbasic97  Mrec97  Mtravel97 Meduc97"*/
/*local unitlist "FTOT97  Fbasic97  Frec97  Ftravel97 Feduc97"*/
/*local unitlist "BfracTOT97  Bfracbasic97  Bfracrec97  Bfractravel97 Bfraceduc97"*/
/*local unitlist "BTOT97  Bbasic97  Brec97  Btravel97 Beduc97"*/
local personlist "F M B"
local interactions "i.famsize i.ageyoungest "
local tableinteractions `"indicate("Dummies = *famsize *ageyoungest ", labels("X" " "))"'
local tableiqr "stats(iqr sd1 r2 N)"
local iqrlabel "IQR of R.T. Effect"
local sd1label "1 sd of R.T. Effect"
local percentiles "25 75"
local unitlist "rec97 TOT97 basic97 educ97 "
/*local unitlist "MTOT97"*/
local x1 "wifeinlf black marriedraterace "
local x2 "wifeinlf black marriedraterace chagem chagem2 hsdip cdip fhsdip fcdip  urban parentage income "
local x3 "wifeinlf black marriedraterace hsdip cdip fhsdip fcdip income "
local x4 "wifeinlf black marriedraterace hsdip cdip fhsdip fcdip income urban"
local x5 "wifeinlf black marriedraterace chagem chagem2 hsdip cdip fhsdip fcdip   parentage income "
local x6 "wifeinlf black marriedraterace chagem chagem2 hsdip cdip fhsdip fcdip  urban parentage income "
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
local weight "[pweight=CH97HHWT] "
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
foreach person of local personlist{
        eststo clear
            local name `person'`unit'
            local tablist "`tablist' `name'"
            foreach X of local xlist{
                        /*local y `unit'`person'`category'*/
                        local y `person'pub`unit'
                        di "eststo: reg `y'  `person'`unit' ``X'' if `mainrestriction' `weight', robust"
                        eststo: reg `y'  `person'`unit' ``X'' if `mainrestriction' `weight', robust
                        /*quietly _pctile   if `mainrestriction', percentiles(`percentiles')*/
                        /*matrix define beta = e(b)*/
                        /*local b = beta[1,1]*/
                        /*local iqrvar = (`r(r2)'-`r(r1)') * `b'*/
                        /*quietly sum  if `mainrestriction', detail*/
                        /*local sd1var = `r(sd)' * `b'*/
                        /*estadd scalar iqr = `iqrvar' */
                        /*estadd scalar sd1 = `sd1var' */
            }
        esttab using `name'.tex, se r2 keep(`person'`unit' `keeplist' ) `tableinteractions' label fragment replace longtable
            esttab using `name'.txt, se r2 keep(`person'`unit' `keeplist' ) `tableinteractions' label fragment replace
            !sed -i 's/o2y1/$ y_1 - o_2$ /g' `name'.tex
            !sed -i 's/r2/$ R^2 $/' `name'.tex
            !sed -i '/(1)/d' `name'.tex
            !sed -i 's/1em/.25em/' `name'.tex
            !sed -i 's/sd1/`sd1label'/' `name'.tex
            !sed -i 's/%/\\%/' `name'.tex
            !sed -i 's/sd1 \{15\}/`sd1label'/' `name'.txt
            !sed -i 's/iqr/`iqrlabel'/' `name'.tex
            !sed -i 's/iqr \{15\}/`iqrlabel'/' `name'.txt
}
}

**********************************************************************
* End X Explore
**********************************************************************
foreach  tab  of local tablist{
!echo Outcome `tab'
!cat `tab'.txt
}
!echo `tablist'
