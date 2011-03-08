clear 
clear matrix
set mem 300m
#delimit;
use data/atusact_0309.dta;
egen id = group(tucaseid);
local basic " (trcodep == 30101) | (trcodep == 30108) | (trcodep == 30109) | (trcodep == 30110)| (trcodep == 30111)| (trcodep == 30112)| (trcodep == 30199)| (trcodep >= 30300 & trcodep < 30400) | (trcodep >= 39900 & trcodep < 40000)| (trcodep == 40101) | (trcodep == 40108) | (trcodep == 40109) | (trcodep == 40110)| (trcodep == 40111)| (trcodep == 40112)| (trcodep == 40199)| (trcodep >= 40300 & trcodep < 40400) | (trcodep >= 49900 & trcodep < 50000)";
local travel "(trcodep == 10301) | (trcodep == 10401) | (trcodep == 10303) | (trcodep == 10403) | (trcodep == 10302) | (trcodep == 10402)"; 
local educ " (trcodep == 30102) | (trcodep == 30106) | (trcodep == 30107) | (trcodep >= 30200 & trcodep < 30300) |     (trcodep == 40102) | (trcodep == 40106) | (trcodep == 40107) | (trcodep >= 40200 & trcodep < 40300) ";
local rec " (trcodep == 30103) | (trcodep == 30104) | (trcodep == 30105) | (trcodep == 40103) | (trcodep == 40104) | (trcodep == 40105)";  
/*There will be a variable named after the local variable name here that will*/
/*consist of the total time spent in particular activities.*/

local childlist "basic travel educ rec";
foreach i of local childlist {;
/*count if ``i''*/;
bysort tucaseid: gen `i'cond = ``i'';
bysort tucaseid: gen `i'contribution = tuactdur24 * `i'cond;
bysort tucaseid: egen `i' = sum(`i'contribution);
drop `i'cond `i'contribution;
};

save data/temp.dta, replace;
/*gen childcare = trcodep = */
duplicates drop tucaseid , force;
keep tucaseid `childlist' tewhere;
save data/act-temp.dta, replace;

use data/atuscps_0309.dta, clear;
keep if tulineno==1 & hrhtype <= 2;
keep peeduca tucaseid tulineno ptdtrace pehspnon hufaminc gestfips hrhtype tubwgt;
save data/cps-temp.dta, replace;

use data/atusrost_0309.dta, clear;
gen eligkid = terrp == 22 & teage <= 12;
bysort tucaseid : egen numelig = sum(eligkid);
gen elig = numelig > 0;
keep if tulineno == 1;
/*duplicates drop tucaseid tulineno, force;*/
keep *elig tucaseid tulineno;
save data/roster-temp.dta, replace;
/*TRERNWA*/

use data/atusresp_0309.dta, clear;
keep if tulineno == 1;
keep trernwa tudiaryday tufnwgtp tuyear tucaseid tulineno;
save data/resp-temp.dta, replace;

use data/act-temp.dta;
merge 1:1 tucaseid using data/roster-temp.dta, keep(3);
drop _merge;

merge 1:1 tucaseid using data/cps-temp.dta, keep(3);
drop _merge;

merge 1:1 tucaseid using data/resp-temp.dta, keep(3);
drop _merge;

keep if ptdtrace <= 2;
foreach i of local childlist {;
    bysort gestfips ptdtrace: egen `i'cellold = mean(`i');
    /*This would generate weighted cell means:*/
    bysort gestfips ptdtrace: gen double sumweight = sum(tufnwgtp);
    bysort gestfips ptdtrace: replace sumweight=sumweight[_N];
    gen double weightedoutcome = `i'*tufnwgtp/sumweight;
    bysort gestfips ptdtrace: gen double `i'cell=sum(weightedoutcome);
    bysort gestfips ptdtrace: replace `i'cell=`i'cell[_N];
    drop sumweight weightedoutcome;
    compress `i'cell;
};
bysort gestfips ptdtrace: gen cellsize = _N;
save data/atus.dta, replace;
duplicates drop gestfips ptdtrace, force;
keep gestfips ptdtrace *cell cellsize *cellold;
save data/cells.dta, replace;
