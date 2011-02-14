**********************************************************************
* Begin making CPS Data for comparison

clear 
clear matrix 
set more off
set mem 200m
local kids "7"
use cps97/cps96-sample.dta, clear
duplicates drop hrhhid pulineno, force
/*Obvservable variables to get*/
/*mschooling - mother's education*/
/*nonwhite - race is not white - done*/
/*married - marital status = 1 if married - done */
/*income - done*/
/*famsize - number of kids*/
/*wifeinlf - fraction of wives in the labor force*/
/*age distribution of kids*/
/*num kids in school*/
/*parentage*/

local keeplist "dadhead elig income parentage hsdip cdip married nonwhite famsize wifeinlf cps CH97PRWT CH97HHWT "
/*local keeplist "dadhead elig income parentage hsdip cdip married nonwhite famsize wifeinlf cps psid educ mhsdip mcdip fhsdip fcdip"*/
***********************************************************************
gen parent = prfamrel == 1 | prfamrel == 2
gen sample = 1
gen march = hrmonth == 3
* sample is the variable that I'll use instead of dropping.  Everything should
* ultimately be based on if sample ==1
/*note: nchild pertains to how many children the individual has, so we need*/
/*nchild for the householder, and to set that for every family member*/
/*replace sample = 0 if gq == 1*/
gen nonwhite = perace != 1
gen black = perace == 2 if perace <= 2
gen married = pemaritl < 3
sort hrhhid peage
/*unique hrhhid, generate(household)*/
/*unique hrhhid pulineno, generate(individual)*/
/*rename famsize famsize_old*/
/*gen famsize = nchild if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2*/
/*by serial: egen tempfamsize = min(famsize)*/
bysort hrhhid prfamnum: gen tempischild = prfamrel == 3 if prfamtyp == 1
replace tempischild = 0 if tempischild == .
bysort hrhhid prfamnum: egen numkids = sum(tempischild)
/*replace famsize = tempfamsize if famsize == .*/
/*by serial: gen headnchild = nchild if relate == 101*/
/*by serial: egen tempheadnchild = min(headnchild)*/
/*replace numkids = tempheadnchild if headnchild == .*/
/*[>gen wifeinlf = empstat >= 10 & empstat <=13 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2<]*/
/*gen wifeinlf = empstat >= 10 & empstat <=22 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2*/
/*by serial: egen wifeinlftemp = min(wifeinlf)*/
/*replace wifeinlf = wifeinlftemp if wifeinlf == .*/
/*drop tempheadnchild*/
sort hrhhid prfamnum peage
foreach n of numlist 1/`kids'{
    by hrhhid prfamnum: gen kid`n'age = peage[`n'] if prfamrel == 3 & numkids >= `n'
    by hrhhid prfamnum: egen tempage = min(kid`n'age)
    replace kid`n'age = tempage if kid`n'age == .
    by hrhhid prfamnum: gen kid`n'sex = pesex[`n'] if prfamrel == 3 & numkids >= `n'
    by hrhhid prfamnum: egen tempsex = min(kid`n'sex)
    replace kid`n'sex = tempsex if kid`n'sex == .
    by hrhhid prfamnum: gen kid`n'elig = kid`n'age >= 0 & kid`n'age < 13
    drop temp*
}
egen numelig = rowtotal(kid1elig kid2elig kid3elig kid4elig kid5elig kid6elig kid7elig )
/*keep hrhhid prfamnum kid* numkids nonwhite black married prfamrel perace pemaritl*/
gen temp = pemaritl == 1
rename gestfips statefip
bysort statefip: egen marriedrateall = mean(temp) if numelig >= 1
bysort statefip: egen mrase = semean(temp) if numelig >= 1
drop if marriedrateall == .
bysort statefip black: egen marriedraterace = mean(temp) if numelig >= 1
bysort statefip black: egen mrrse = semean(temp) if numelig >= 1
drop if marriedraterace == .
bysort statefip black: gen samp = _N if numelig >= 1
drop if black == .
duplicates drop statefip black, force
keep marriedrateall statefip marriedraterace black mrase mrrse samp
save married.dta, replace 
