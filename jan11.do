clear matrix
clear
clear mata
set mem 100m
set more off
use twoSibsPared.dta
label variable selfBTOT97 "oiyi"
local personlist "B"
local unitlist " `""' "
local categorylist "TOT02 "
local iqrlabel "IQR of R.T. Effect"
local sd1label "1 sd of R.T. Effect"
local percentiles "25 75"

local interactions "i.famsize i.ageyoungest i.SBLNUM03 i.ageyoungest02"
/*local interactions "i.famsize i.ageyoungest "*/
local tableinteractions `"indicate("Dummies = *famsize *ageyoungest", labels("X" " "))"'
local x1 ""
local x2 " chagem chagem2 cdip hsdip fcdip fhsdip nonwhite male parentage income inschool "
local x3 ""
local x4 " chagem chagem2 nonwhite male parentage inschool "
local x5 " chagem chagem2 cdip hsdip fcdip fhsdip nonwhite male parentage income inschool "
local x6 " chagem chagem2 cdip hsdip fcdip fhsdip nonwhite male parentage inschool"
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
local RTvar1 "RT"
local weight "[pweight=CH97PRWT] "
local RTvarlist "RTvar1 "
local keeplist "`RTvar1' `x5'"
local xlist "X1 X2 X3 X4 X5 X6"
local mainrestriction "dadhead & mschooling97 < 97 & fschooling97 < 97 & selfBTOT97 != . "
fvset base 3 famsize
fvset base 3 SBLNUM03
fvset base 3 ageyoungest
fvset base 3 ageyoungest02
fvset base 3 yearsdiff
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
                foreach category of local categorylist{
                    foreach RTvar of local RTvarlist{
                        /*local y `unit'`person'`category'*/
                        local y "BTOT02"
                        di "Running command: reg `y'  ``RTvar'' ``X'' if `mainrestriction' `weight', robust"
                        di "``X''"
                        eststo: reg `y'  ``RTvar'' ``X'' if `mainrestriction' `weight', robust
                        quietly _pctile  ``RTvar'' if `mainrestriction', percentiles(`percentiles')
                        matrix define beta = e(b)
                        local b = beta[1,1]
                        local iqrvar = (`r(r2)'-`r(r1)') * `b'
                        quietly sum ``RTvar'' if `mainrestriction', detail
                        local sd1var = `r(sd)' * `b'
                        estadd scalar iqr = `iqrvar' 
                        estadd scalar sd1 = `sd1var' 
                    }
                }
            }
        esttab using `name'ols.tex, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace longtable
            esttab using `name'ols.txt, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace
            !sed -i 's/o2y1/$ y_1 - o_2$ /g' `name'ols.tex
            !sed -i 's/r2/$ R^2 $/' `name'ols.tex
            !sed -i '/(1)/d' `name'ols.tex
            !sed -i 's/1em/.25em/' `name'ols.tex
            !sed -i 's/sd1/`sd1label'/' `name'ols.tex
            !sed -i 's/sd1 \{15\}/`sd1label'/' `name'ols.txt
            !sed -i 's/iqr/`iqrlabel'/' `name'ols.tex
            !sed -i 's/iqr \{15\}/`iqrlabel'/' `name'ols.txt
            !cp `name'ols.tex results
    }
}
**********************************************************************
* End X Explore
**********************************************************************
**********************************************************************
* Start time type
**********************************************************************
local categorylist "TOT02 basic02 educ02 rec02 travel02"
label variable BTOT02 "All Time"
label variable Bbasic02 "Basic"
label variable Brec02 "Rec."
label variable Btravel02 "Travel"
label variable Beduc02 "Educ."
local xlist "X6"
local keeplist "`RTvar1' `x6'"
foreach unit of local unitlist{
    foreach person of local personlist{
        eststo clear
            local name `person'`unit'type
            local tablist "`tablist' `name'"
            foreach X of local xlist{
                foreach category of local categorylist{
                    foreach RTvar of local RTvarlist{
                        local y `unit'`person'`category'
                        di "Running command: reg `y' ``RTvar'' `interactions' ``X''  if `mainrestriction', robust"
                        eststo: reg `y' ``RTvar'' `interactions' ``X'' if `mainrestriction' `weight', robust
                        quietly _pctile ``RTvar'' if `mainrestriction', percentiles(`percentiles')
                        matrix define beta = e(b)
                        local b = beta[1,1]
                        local iqrvar = (`r(r2)'-`r(r1)') * `b'
                        quietly sum ``RTvar'' if `mainrestriction', detail
                        local sd1var = `r(sd)' * `b'
                        estadd scalar iqr = `iqrvar' 
                        estadd scalar sd1 = `sd1var' 
                    }
                }
            }
        esttab using `name'ols.tex, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace longtable
            esttab using `name'ols.txt, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace
            !sed -i 's/o1y2/$ y_2 - o_1$ /g' `name'ols.tex
            !sed -i 's/r2/$ R^2 $/' `name'ols.tex
            !sed -i '/(1)/d' `name'ols.tex
            !sed -i 's/1em/.25em/' `name'ols.tex
            !sed -i 's/sd1/`sd1label'/' `name'ols.tex
            !sed -i 's/sd1 \{15\}/`sd1label'/' `name'ols.txt
            !sed -i 's/iqr/`iqrlabel'/' `name'ols.tex
            !sed -i 's/iqr \{15\}/`iqrlabel'/' `name'ols.txt
            !cp `name'ols.tex results
    }
}

**********************************************************************
* End time type
**********************************************************************

**********************************************************************
* Start Old-Young
**********************************************************************
local categorylist "TOT97 "
local xlist "X3 X6"
local keeplist "`RTvar1' `x6'"
local unitlist "BTOT97 BTOT02"
local titles `" "yioi " " $ y_i $ " " $ o_i $ " "yioi " " $ y_i $ " " $ o_i $ " "'
/*local titles `" " $ y_i - o_i $ " " $ y_i $ " " $ o_i $ " "$ y_i - o_i $  " " $ y_i $ " " $ o_i $ " "'*/
local extrarestriction " 1" 
foreach person of local personlist{
eststo clear
local name "BTOT97age"
            local tablist "`tablist' `name'"
foreach X of local xlist{
foreach unit of local unitlist{
        foreach RTvar of local RTvarlist{
            local y `unit'
            di "Running command: reg `y' ``RTvar'' `interactions' ``X''  if `mainrestriction' & `extrarestriction', robust"
            eststo: reg `y'  ``RTvar'' ``X'' if `mainrestriction' & `extrarestriction' `weight', robust
            quietly _pctile ``RTvar'' if `mainrestriction' & `extrarestriction' `weight', percentiles(`percentiles')
            matrix define beta = e(b)
            local b = beta[1,1]
            local iqrvar = (`r(r2)'-`r(r1)') * `b'
            quietly sum ``RTvar'' if `mainrestriction' & `extrarestriction', detail
            local sd1var = `r(sd)' * `b'
            estadd scalar iqr = `iqrvar' 
            estadd scalar sd1 = `sd1var' 
        }
    }
}
esttab using `name'ols.tex, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace longtable mtitles(`titles')
esttab using `name'ols.txt, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace mtitles(`titles')
!sed -i 's/yioi/$ y_i - o_i$ /g' `name'ols.tex
!sed -i 's/yi/$ y_i$ /g' `name'ols.tex
!sed -i 's/oi/$ o_i$ /g' `name'ols.tex
!sed -i 's/\\_/_/g' `name'ols.tex
!sed -i 's/r2/$ R^2 $/' `name'ols.tex
!sed -i '/(1)/d' `name'ols.tex
!sed -i '/o\./d' `name'ols.tex
!sed -i 's/1em/.25em/' `name'ols.tex
!sed -i 's/sd1/`sd1label'/' `name'ols.tex
!sed -i 's/sd1 \{15\}/`sd1label'/' `name'ols.txt
!sed -i 's/iqr/`iqrlabel'/' `name'ols.tex
!sed -i 's/iqr \{15\}/`iqrlabel'/' `name'ols.txt
!cp `name'ols.tex results
}
**********************************************************************
* End Old-Young
**********************************************************************

**********************************************************************
* Start Housewives
**********************************************************************
local unitlist " `""'"
local restrictionlist " `" 1"' `"wifeinlf == 0"' `"wifeinlf == 1"' "
local xlist "X3 X6"
local keeplist "`RTvar1' `x6'"
/*local weight "[pweight=CH97HHWT] "*/
local titles `" "All " " Not in LF " " In LF " "All " " Not in LF " " In LF " "'
foreach unit of local unitlist{
    foreach person of local personlist{
        eststo clear
            local name `person'`unit'housewives
            local tablist "`tablist' `name'"
            foreach X of local xlist{
                foreach restriction of local restrictionlist{
                    foreach category of local categorylist{
                        foreach RTvar of local RTvarlist{
                            local y `unit'`person'`category'
                            di "Running command: reg `y' ``RTvar'' `interactions' ``X''  if `mainrestriction' & `restriction', robust"
                            eststo: reg `y' ``RTvar'' `interactions' ``X'' if `mainrestriction' & `restriction' `weight', robust
                            quietly _pctile ``RTvar'' if `mainrestriction' & `restriction', percentiles(`percentiles')
                            matrix define beta = e(b)
                            local b = beta[1,1]
                            local iqrvar = (`r(r2)'-`r(r1)') * `b'
                            quietly sum ``RTvar'' if `mainrestriction' & `restriction', detail
                            local sd1var = `r(sd)' * `b'
                            estadd scalar iqr = `iqrvar' 
                            estadd scalar sd1 = `sd1var' 
                        }
                    }
                }
            }
        esttab using `name'ols.tex, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace longtable mtitles(`titles')
            esttab using `name'ols.txt, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace mtitles(`titles')
            !sed -i 's/o1y2/$ y_2 - o_1$ /g' `name'ols.tex
            !sed -i 's/r2/$ R^2 $/' `name'ols.tex
            !sed -i '/(1)/d' `name'ols.tex
            !sed -i 's/1em/.25em/' `name'ols.tex
            !sed -i 's/sd1/`sd1label'/' `name'ols.tex
            !sed -i 's/sd1 \{15\}/`sd1label'/' `name'ols.txt
            !sed -i 's/iqr/`iqrlabel'/' `name'ols.tex
            !sed -i 's/iqr \{15\}/`iqrlabel'/' `name'ols.txt
            !cp `name'ols.tex results
    }
}
**********************************************************************
* End Housewives
**********************************************************************

**********************************************************************
* Start Age Level
**********************************************************************
local stagecut 2
gen s1 = chage <= `stagecut' 
gen s2 = chage > `stagecut' 
local restrictionlist " `" 1"' `" s1 "' `" s2 "' "
local xlist "X3 X6"
local keeplist "`RTvar1' `x6'"
local titles `" "All " "  Age $ \leq `stagecut'$ " "  Age $ > `stagecut'$" "All " "  Age $ \leq `stagecut'$ " "  Age $ > `stagecut'$ " "'
foreach unit of local unitlist{
    foreach person of local personlist{
        eststo clear
            local name `person'`unit'stage
            local tablist "`tablist' `name'"
            foreach X of local xlist{
                foreach restriction of local restrictionlist{
                    foreach category of local categorylist{
                        foreach RTvar of local RTvarlist{
                            local y `unit'`person'`category'
                            di "Running command: reg `y' ``RTvar'' `interactions' ``X''  if `mainrestriction' & `restriction', robust"
                            eststo: reg `y' ``RTvar'' `interactions' ``X'' if `mainrestriction' & `restriction' `weight', robust
                            quietly _pctile ``RTvar'' if `mainrestriction' & `restriction', percentiles(`percentiles')
                            matrix define beta = e(b)
                            local b = beta[1,1]
                            local iqrvar = (`r(r2)'-`r(r1)') * `b'
                            quietly sum ``RTvar'' if `mainrestriction' & `restriction', detail
                            local sd1var = `r(sd)' * `b'
                            estadd scalar iqr = `iqrvar' 
                            estadd scalar sd1 = `sd1var' 
                        }
                    }
                }
            }
        esttab using `name'ols.tex, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace longtable mtitles(`titles')
            esttab using `name'ols.txt, se r2 keep(`keeplist' ) `tableinteractions' label `tableiqr' fragment replace mtitles(`titles')
            !sed -i 's/o1y2/$ y_2 - o_1$ /g' `name'ols.tex
            !sed -i 's/r2/$ R^2 $/' `name'ols.tex
            !sed -i '/(1)/d' `name'ols.tex
            !sed -i 's/1em/.25em/' `name'ols.tex
            !sed -i 's/sd1/`sd1label'/' `name'ols.tex
            !sed -i 's/sd1 \{15\}/`sd1label'/' `name'ols.txt
            !sed -i 's/iqr/`iqrlabel'/' `name'ols.tex
            !sed -i 's/iqr \{15\}/`iqrlabel'/' `name'ols.txt
            !cp `name'ols.tex results
    }
}
**********************************************************************
* End Age Level
**********************************************************************
foreach  tab  of local tablist{
!echo `tab':
!cat `tab'ols.txt
}
!echo `tablist'
