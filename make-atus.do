clear 
clear matrix
set mem 300m
use data/atusact_0309.dta
egen id = group(tucaseid)
local childcarecode "trcodep == 30301 | trcodep == 30302"
local childcarecode2 "trcodep == 301 | trcodep == 30302"
/*There will be a variable named after the local variable name here that will*/
/*consist of the total time spent in particular activities.*/

local childlist "childcarecode childcarecode2 "
foreach i of local childlist {
/*count if ``i''*/
bysort id: gen `i'cond = ``i''
bysort id: gen `i'contribution = tuactdur24 * `i'cond
bysort id: egen `i' = sum(`i'contribution)
drop `i'cond `i'contribution
}
save temp.dta, replace
/*gen childcare = trcodep = */
duplicates drop id, force
keep id `childlist'
save atus.dta, replace

!rm temp.dta
