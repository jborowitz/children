**********************************************************************
* Begin making CPS Data for comparison

clear 
clear matrix 
set more off
set mem 200m
local kids "9"
use cps_00003.dta, clear
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
gen parent = (relate <= 201 | relate == 1114 | relate == 1113)
gen sample = 1
* sample is the variable that I'll use instead of dropping.  Everything should
* ultimately be based on if sample ==1
/*note: nchild pertains to how many children the individual has, so we need*/
/*nchild for the householder, and to set that for every family member*/
replace sample = 0 if gq == 1
gen nonwhite = race != 100
gen black = race == 200 if race <= 200
gen married = marst < 3
gen income = ftotval/100000
sort serial age
rename famsize famsize_old
gen famsize = nchild if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
by serial: egen tempfamsize = min(famsize)
replace famsize = tempfamsize if famsize == .
by serial: gen headnchild = nchild if relate == 101
by serial: egen tempheadnchild = min(headnchild)
replace headnchild = tempheadnchild if headnchild == .
/*gen wifeinlf = empstat >= 10 & empstat <=13 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2*/
gen wifeinlf = empstat >= 10 & empstat <=22 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
by serial: egen wifeinlftemp = min(wifeinlf)
replace wifeinlf = wifeinlftemp if wifeinlf == .
gen numkids = headnchild
drop tempheadnchild
foreach n of numlist 1/`kids'{
    by serial: gen kid`n'age = age[`n'] if relate == 301 & headnchild >= `n' 
    by serial: egen tempage = min(kid`n'age)
    replace kid`n'age = tempage if kid`n'age == .
    by serial: gen kid`n'sex = sex[`n'] if relate == 301 & headnchild >= `n' 
    by serial: egen tempsex = min(kid`n'sex)
    replace kid`n'sex = tempsex if kid`n'sex == .
    by serial: gen kid`n'elig = kid`n'age >= 0 & kid`n'age < 13
    drop temp*
}
gen boykid = relate == 301 & sex == 1
by serial: egen numboys = total(boykid)
gen girlkid = relate == 301 & sex == 2
by serial: egen numgirls = total(girlkid)
gen fracboys = numboys/numkids
gen fathersed = educ99 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
gen meduc = 1 if educ == 11 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 0 if educ == 2 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 2.5 if educ == 10 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 5.5 if educ == 20 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 7.5 if educ == 30 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 12 if educ >= 70 & educ <= 73 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 2 if educ == 12 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 3 if educ == 13 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 4 if educ == 14 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 5 if educ == 21 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 6 if educ == 22 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 7 if educ == 31 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 8 if educ == 32 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 9 if educ == 40 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 10 if educ == 50 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 11 if educ == 60 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 12 if educ == 70 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 13 if educ == 80 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 14 if educ == 81 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 14 if educ == 90 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 14 if educ == 91 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 14 if educ == 92 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 15 if educ == 100 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 16 if educ > 110 & educ < 113 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 18 if educ >= 113 & educ <= 114 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
replace meduc = 20 if educ == 115  & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
by serial: egen meductemp = min(meduc)
replace meduc = meductemp if meduc == .
gen feduc = 1 if educ == 11 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 0 if educ == 2 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 2.5 if educ == 10 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 5.5 if educ == 20 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 7.5 if educ == 30 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 12 if educ >= 70 & educ <= 73 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 2 if educ == 12 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 3 if educ == 13 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 4 if educ == 14 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 5 if educ == 21 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 6 if educ == 22 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 7 if educ == 31 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 8 if educ == 32 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 9 if educ == 40 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 10 if educ == 50 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 11 if educ == 60 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 12 if educ == 70 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 13 if educ == 80 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 14 if educ == 81 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 14 if educ == 90 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 14 if educ == 91 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 14 if educ == 92 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 15 if educ == 100 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 16 if educ > 110 & educ < 113 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 18 if educ >= 113 & educ <= 114 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
replace feduc = 20 if educ == 115 & (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
by serial: egen feductemp = min(feduc)
replace feduc = feductemp if feduc == .

/*gen mhsdip = educ >= 73 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2*/
gen mhsdip = meduc >= 12 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
by serial: egen mhsdiptemp = min(mhsdip)
replace mhsdip = mhsdiptemp if mhsdip == .

/*gen fhsdip = educ >= 73 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1*/
gen fhsdip = feduc >= 12 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
by serial: egen fhsdiptemp = min(fhsdip)
replace fhsdip = fhsdiptemp if fhsdip == .

gen momage = age if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
by serial: egen momagetemp = min(momage)
replace momage = momagetemp if momage == .

gen dadage = age if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
by serial: egen dadagetemp = min(dadage)
replace dadage = dadagetemp if dadage == .

/*gen mcdip = educ >= 111 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2*/
gen mcdip = meduc >= 16 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 2
by serial: egen mcdiptemp = min(mcdip)
replace mcdip = mcdiptemp if mcdip == .

/*gen fcdip = educ == 111 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1*/
gen fcdip = feduc >= 16 if (relate <= 201 | relate == 1114 | relate == 1113) & sex == 1
by serial: egen fcdiptemp = min(fcdip)
replace fcdip = fcdiptemp if fcdip == .

gen parentage = momage
sort serial relate
duplicates drop serial, force
rename meduc mschooling
rename feduc fschooling

gen hsdip = mhsdip
gen cdip = mcdip
egen numelig = rowtotal(kid1elig kid2elig kid3elig kid4elig kid5elig kid6elig kid7elig kid8elig kid9elig)
gen dadhead = dadage != . & momage != .
gen elig = numelig >= 2 
gen cps = 1
gen psid = 0
gen CH97PRWT = 1
gen CH97HHWT = 1
save cps97pared, replace
gen temp = marst == 1
bysort statefip: egen marriedrateall = mean(temp) if kid1elig
bysort statefip: egen mrase = semean(temp) if kid1elig
drop if marriedrateall == .
bysort statefip black: egen marriedraterace = mean(temp) if kid1elig
bysort statefip black: egen mrrse = semean(temp) if kid1elig
drop if marriedraterace == .
bysort statefip black: gen samp = _N if kid1elig
drop if black == .
/*I am now calculating marriage rate by ALL people, not just eligible CDS families.  I thought about this, and went back to for eligible families.*/
duplicates drop statefip black, force
keep marriedrateall statefip marriedraterace black mrase mrrse samp
save married.dta, replace 
use cps97pared, replace
sum fracboys income parentage mhsdip mcdip married nonwhite famsize wifeinlf if dadhead & elig
keep `keeplist'
save cps97sum, replace

clear 
set maxvar 10000
use pcgPared, clear
keep `keeplist'
append using cps97sum
label values cps cps
label define cps 1 "CPS" 0 "PSID"
save comparison.dta, replace
* End CPS Data 
**********************************************************************

