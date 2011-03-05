* How to handle directory variables here?  I think I want to set all the variables in the .do file
global dir /home/jborowitz/project
global extractnum 123176
global extractname test
global ID PCGID_97
global PN PCGPN_97

global parentlist "RT RT1 ER32021 ER32022 ER32023 ER32024 ER32025 ER32026 ER32027 ER32022 ER10016 ER33402 ER33403 ER33303 ER32049 ER32039 ER32036 ER32046 ER32043 parentbirthyear* ER33415 ER11488 ER12079 ER10081 ER10082 ER10083 ER10563 ER10564 ER10565"
  
/*

parentlist is the list of variables that will come from the parent
file.  The construct of the file is as follows.  Every variable is
downloaded for both CDS kids and CDS parents.  Some of these variables
(age) apply to both.  Others (such as woodcock johnson test scores)
apply only to kids, and others, such as labor force info, apply only
to parents. Thus this is a list of the variables that I will DROP from
the CDS version of the file.  Then, when the CDS and parent portions
are remerged together, these variables won't exist and they will come
from the parent file.

I don't need to do "kidlist", since this is the master file when I
merge, and hence all variables by default pertain to kids.

Instead of limiting to just splitting and merging the CDS data, I also had to
calculate sibling information, since non-cds kids can be in the family and have
valid sibling info - 2010-10-08

*/
  
global extractcode J$extractnum
global mapextractcode  M$extractnum

cd $dir/tmp
capture log close
log using $dir/temp.log, replace
set mem 300m
set more off
do $extractcode.do
**********************************************************************
  * Generate duplicate variables to put on the parent list
gen parentbirthyear07 = ER33406
gen parentbirthyear97 = ER33906
gen kidrelationtohead = ER33403
**********************************************************************

compress
save $extractname-compressed.dta, replace
gen age_months = 12*(1997-ER33406) + (ER10005 - ER33405)
sort ER30001 ER32010 age_months
by ER30001 ER32010: gen birthorder = _N - _n + 1  
local maxkids "10"
foreach n of numlist 1/`maxkids' {
 gen kid`n'agem = age_months if birthorder == `n'
 by ER30001 ER32010:  egen tempage = min(kid`n'agem)
 replace kid`n'agem = tempage if kid`n'agem == .
 gen kid`n'sex = ER32000 if birthorder == `n'
 by ER30001 ER32010:  egen tempsex = min(kid`n'sex)
 replace kid`n'sex = tempsex if kid`n'sex == .
 drop temp*
}
sort ER30001 ER30002
keep if ER33420 == 1
/*Above command keeps only CDS kids*/
drop $parentlist
save $extractname-cds.dta, replace
clear
do $mapextractcode.do
sort $ID $PN
save $extractname-map.dta, replace
use $extractname-cds, clear
merge 1:1 ER30001 ER30002 using $extractname-map.dta
tab _merge
drop if _merge != 3
drop _merge
save $extractname-cds, replace

use $extractname-compressed.dta, clear
gen $ID = ER30001
gen $PN = ER30002
sort $ID $PN
rename ER32022 numlivebirths
rename ER32021 birthinfoyear
save $extractname-psid.dta, replace
use $extractname-cds.dta, clear
merge m:1 $ID $PN using $extractname-psid.dta
save $extractname-merged.dta, replace

**********************************************************************
/*
Begin renaming
*/
  /*
rename ER36020 kidsathome07
rename ER10012 kidsathome97
rename ER21020 kidsathome03
*/
/*rename WD025494 homeworkWD
rename WE025494 homeworkWE
*/
rename WD97_221 homeworkWD
rename WE97_221 homeworkWE
/*  
rename WD023926 socializingWD
rename WE023926 socializingWE
*/
**********************************************************************

* begin creating data within households
keep if _merge == 3
* keep only successfully merged parents and kids

bysort PCGID_97 PCGPN_97: gen numkids = _N
drop if numkids == 1
* Drop single kids, since I'm looking at parents with multiple kids in the CDS
drop if RT == .
* Drop parents that don't have risk measures

replace homeworkWD = . if homeworkWD == 99999
replace homeworkWE = . if homeworkWE == 99999

drop if homeworkWD == .
bysort PCGID_97 PCGPN_97: egen hwsd = sd(5*homeworkWD + 2*homeworkWE)
gen hw = log(hwsd)
bysort PCGID_97 PCGPN_97: drop if _n > 1
count if hwsd == .
count if hwsd == 0
sum hw*
  count

  /* replace socializingWD = . if socializingWD == 99999 */
  /* replace socializingWE = . if socializingWE == 99999 */

/* bysort PCGID_97 PCGPN_97: egen socsd = sd((5*socializingWD + 2*socializingWE)/3600) */
/* gen soc = log(socsd) */
/* bysort PCGID_97 PCGPN_97: drop if _n > 1 */
/* count if socsd == 0 */
/* count if socsd == . */

/*
What I'm gonna do here is calculate the within family variance:

bysort PCGID_97 PCGPN_97: egen outcomevarsd = sd(inputvar)
gen outcomevar = log(outcomevarsd)

* Note that here the problem is that you can't use log inside egen.  Fuck you, Stata.

*/
save $extractname-parents.dta, replace
sum
exit, clear
