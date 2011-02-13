**********************************************************************
* Begin 1997 Time Diary Aggregation
**********************************************************************
clear
clear matrix
set more off
set maxvar 10000
set mem 500m
local varslist97 "`rec1list97'`rec2list97'`rec3list97'`rec4list97' `reclist97' `basiclist97' `educlist97' `travellist97'"
local publist97 "148 149 449"
local pubplacelist97 "0 0 0 "
/*list: home is 10 or 40*/
/*Each entry in publist should be an activity code and each entry in*/
/*pubplacelist97 should be a list of place codes.  The place codes can be zero*/
/*for any place*/
local extralist97 "488 439 943" 
local reclist97 "502 503  631 632 633 634 635 651 652 641 643 644 601 602 671 672 661 662 611 621 622 612 689 709 729 739 749 719 752 769 771 772 789 883 884 801 802 803 804 805 806 807 808 809 810 865 811 812 813 814 815 816 817 818 821 822 823 824 825 826 831 832 833 834 835 841 842 843 844 851 852 861 862 863 864 866 871 872 873 874 875 876 877 878 879 882 909 919 929 961 962 964 965 979 981 982 983 963 989"
local rec1list97 "502 503  631 632 633 634 635 651 652 641 643 644 601 602 671 672 661 662 611 621 622 612 689 "
local rec2list97 "709 729 739 749 719 752 769 771 772 789 "
local rec3list97 "883 884 801 802 803 804 805 806 807 808 809 810 865 811 812 813 814 815 816 817 818 821 822 823 824 825 826 831 832 833 834 835 841 842 843 844 851 852 861 862 863 864 866 871 872 873 874 875 876 877 878 879 882 "
local rec4list97 "909 919 929 961 962 964 965 979 981 982 983 963 989"
local basiclist97 "209 219 218 248 221 258 222 236 237 239 249 259 238 269 278 277 279 439 449 484 488 489 487 485 486 966 967"
local educlist97 "501 504 505 510 511 512 549 569 939 941 959 942 943"
local travellist97 "498 499 597 598 599 698 699 799 899 999"
local personlist "M F B N O"
use td97.dta
sort ER30001 ER30002
gen basic97 = 0
foreach i of local basiclist97 {
  replace basic97 = 1 if COLA == `i'
}
gen educ97 = 0
foreach i of local educlist97 {
  replace educ97 = 1 if COLA == `i'
}
gen travel97 = 0
foreach i of local travellist97 {
  replace travel97 = 1 if COLA == `i'
}
gen rec97 = 0
foreach i of local reclist97 {
  replace rec97 = 1 if COLA == `i'
}
gen rec197 = 0
foreach i of local rec1list97 {
  replace rec197 = 1 if COLA == `i'
}
gen rec297 = 0
foreach i of local rec2list97 {
  replace rec297 = 1 if COLA == `i'
}
gen rec397 = 0
foreach i of local rec3list97 {
  replace rec397 = 1 if COLA == `i'
}
gen rec497 = 0
foreach i of local rec4list97 {
  replace rec497 = 1 if COLA == `i'
}
gen rec102 = 0
foreach i of local rec1list02 {
  replace rec102 = 1 if COLA == `i'
}
gen rec202 = 0
foreach i of local rec2list02 {
  replace rec202 = 1 if COLA == `i'
}
gen rec302 = 0
foreach i of local rec3list02 {
  replace rec302 = 1 if COLA == `i'
}
gen rec402 = 0
foreach i of local rec4list02 {
  replace rec402 = 1 if COLA == `i'
}
gen pub97 = 1
replace pub97 = 0 if COLF == 10 | COLF == 40 | COLF == 45
gen priv97 = 0
replace priv97 = 1 if COLF == 10 | COLF == 40 | COLF == 45
/*foreach i of local publist97 {*/
/*}*/
gen pubbasic97 = 0
replace pubbasic97 = 1 if pub97 == 1 & basic97 == 1
gen pubrec97 = 0
replace pubrec97 = 1 if pub97 == 1 & rec97 == 1
gen pubeduc97 = 0
replace pubeduc97 = 1 if pub97 == 1 & educ97 == 1
gen pubtravel97 = 0
replace pubtravel97 = 1 if pub97 == 1 & travel97 == 1
foreach var in pubbasic97 pubeduc97 pubtravel97 pubrec97 basic97 educ97 travel97 rec97 rec197 rec297 rec397 rec497 rec102 rec202 rec302 rec402 pub97 priv97{
  gen WDM`var' = DURATION if `var'  & WDAYWEND == 1 & COLG_B == 1
  gen WDF`var' = DURATION if `var'  & WDAYWEND == 1 & COLG_C == 1
  gen WDA`var' = DURATION if `var'  & WDAYWEND == 1 & COLG_C == 1 & COLG_B == 1
  gen WDN`var' = DURATION if `var'  & WDAYWEND == 1 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
  replace WDM`var' = 0 if WDM`var' == .
  replace WDF`var' = 0 if WDF`var' == .
  replace WDN`var' = 0 if WDN`var' == .
  replace WDA`var' = 0 if WDA`var' == .
  gen WEM`var' = DURATION if `var'  & WDAYWEND == 0 & COLG_B == 1
  gen WEF`var' = DURATION if `var'  & WDAYWEND == 0 & COLG_C == 1
  gen WEA`var' = DURATION if `var'  & WDAYWEND == 0 & COLG_C == 1 & COLG_B == 1
  gen WEN`var' = DURATION if `var'  & WDAYWEND == 0 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
  replace WEM`var' = 0 if WEM`var' == .
  replace WEF`var' = 0 if WEF`var' == .
  replace WEN`var' = 0 if WEN`var' == .
  replace WEA`var' = 0 if WEA`var' == .
  bysort ER30001 ER30002: egen M`var' = total((5*WDM`var'+ 2*WEM`var')/3600)
  bysort ER30001 ER30002: egen F`var' = total((5*WDF`var'+ 2*WEF`var')/3600)
  bysort ER30001 ER30002: egen N`var' = total((5*WDN`var'+ 2*WEN`var')/3600)
  bysort ER30001 ER30002: egen A`var' = total((5*WDA`var'+ 2*WEA`var')/3600)
  gen B`var' = M`var' + F`var'
  gen O`var' = B`var' - A`var'
  /*drop WDM`var' WEM`var'*/
  /*drop WDF`var' WEF`var'*/
  /*drop WDN`var' WEN`var'*/
  /*drop WDA`var' WEA`var'*/
}
gen MSUM97 = Mbasic97 + Meduc97 + Mtravel97 + Mrec97
gen FSUM97 = Fbasic97 + Feduc97 + Ftravel97 + Frec97
gen BSUM97 = Bbasic97 + Beduc97 + Btravel97 + Brec97
gen NSUM97 = Nbasic97 + Neduc97 + Ntravel97 + Nrec97
gen ASUM97 = Abasic97 + Aeduc97 + Atravel97 + Arec97
gen MpubSUM97 = Mpubbasic97 + Mpubeduc97 + Mpubtravel97 + Mpubrec97
gen FpubSUM97 = Fpubbasic97 + Fpubeduc97 + Fpubtravel97 + Fpubrec97
gen BpubSUM97 = Bpubbasic97 + Bpubeduc97 + Bpubtravel97 + Bpubrec97
gen NpubSUM97 = Npubbasic97 + Npubeduc97 + Npubtravel97 + Npubrec97
gen ApubSUM97 = Apubbasic97 + Apubeduc97 + Apubtravel97 + Apubrec97
gen OSUM97 = BSUM97 - ASUM97 
gen OpubSUM97 = BpubSUM97 - ApubSUM97 
gen Mnotravel97 = MSUM97 - Mtravel97
gen Fnotravel97 = FSUM97 - Ftravel97
gen Bnotravel97 = BSUM97 - Btravel97
gen Nnotravel97 = NSUM97 - Ntravel97
gen Anotravel97 = ASUM97 - Atravel97
gen Onotravel97 = Bnotravel97 - Anotravel97 
foreach var in `extralist97'{
  gen WDM`var' = DURATION if COLA == `var'  & WDAYWEND == 1 & COLG_B == 1
  gen WDF`var' = DURATION if COLA == `var'  & WDAYWEND == 1 & COLG_C == 1
  gen WDA`var' = DURATION if COLA == `var'  & WDAYWEND == 1 & COLG_C == 1 & COLG_B == 1
  gen WDN`var' = DURATION if COLA == `var'  & WDAYWEND == 1 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
  gen WDB`var' = WDM`var' + WDF`var'
  replace WDM`var' = 0 if WDM`var' == .
  replace WDF`var' = 0 if WDF`var' == .
  replace WDB`var' = 0 if WDB`var' == .
  replace WDN`var' = 0 if WDN`var' == .
  replace WDA`var' = 0 if WDA`var' == .
  * This command generates a variable which is the total number of seconds spent on activity 488 on weekdays when the mom is participating.
  gen WEM`var' = DURATION if COLA == `var'  & WDAYWEND == 0 & COLG_B == 1
  gen WEF`var' = DURATION if COLA == `var'  & WDAYWEND == 0 & COLG_C == 1
  gen WEA`var' = DURATION if COLA == `var'  & WDAYWEND == 0 & COLG_C == 1 & COLG_B == 1
  gen WEN`var' = DURATION if COLA == `var'  & WDAYWEND == 0 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
  gen WEB`var' = WEM`var' + WEF`var'
  replace WEM`var' = 0 if WEM`var' == .
  replace WEF`var' = 0 if WEF`var' == .
  replace WEB`var' = 0 if WEB`var' == .
  replace WEN`var' = 0 if WEN`var' == .
  replace WEA`var' = 0 if WEA`var' == .
  bysort ER30001 ER30002: egen M`var' = total((5*WDM`var'+ 2*WEM`var')/3600)
  bysort ER30001 ER30002: egen F`var' = total((5*WDF`var'+ 2*WEF`var')/3600)
  bysort ER30001 ER30002: egen B`var' = total((5*WDB`var'+ 2*WEB`var')/3600)
  bysort ER30001 ER30002: egen N`var' = total((5*WDN`var'+ 2*WEN`var')/3600)
  bysort ER30001 ER30002: egen A`var' = total((5*WDA`var'+ 2*WEA`var')/3600)
  gen O`var' = B`var' - A`var'
  /*drop WDM`var' WEM`var' WDF`var' WEF`var' WDB`var' WEB`var' WDN`var' WEN`var' WDA`var' WEA`var'*/
}

gen WDMTOT97 = DURATION if WDAYWEND == 1 & COLG_B == 1
gen WEMTOT97 = DURATION if WDAYWEND == 0 & COLG_B == 1
gen WDFTOT97 = DURATION if WDAYWEND == 1 & COLG_C == 1
gen WEFTOT97 = DURATION if WDAYWEND == 0 & COLG_C == 1
gen WDATOT97 = DURATION if WDAYWEND == 1 & COLG_B == 1 & COLG_C == 1
gen WEATOT97 = DURATION if WDAYWEND == 1 & COLG_B == 1 & COLG_C == 1
gen WDNTOT97 = DURATION if WDAYWEND == 1 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
gen WENTOT97 = DURATION if WDAYWEND == 0 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
replace WDMTOT97 = 0 if WDMTOT97 == .
replace WEMTOT97 = 0 if WEMTOT97 == .
replace WDFTOT97 = 0 if WDFTOT97 == .
replace WEFTOT97 = 0 if WEFTOT97 == .
replace WDNTOT97 = 0 if WDNTOT97 == .
replace WENTOT97 = 0 if WENTOT97 == .
replace WDATOT97 = 0 if WDATOT97 == .
replace WEATOT97 = 0 if WEATOT97 == .
bysort ER30001 ER30002: egen MTOT97 = total((5*WDMTOT97+ 2*WEMTOT97)/3600)
bysort ER30001 ER30002: egen FTOT97 = total((5*WDFTOT97+ 2*WEFTOT97)/3600)
bysort ER30001 ER30002: egen NTOT97 = total((5*WDNTOT97+ 2*WENTOT97)/3600)
bysort ER30001 ER30002: egen ATOT97 = total((5*WDATOT97+ 2*WEATOT97)/3600)
gen BTOT97 = FTOT97 + MTOT97
gen OTOT97 = BTOT97 - ATOT97
gen WDMpubTOT97 = DURATION if WDAYWEND == 1 & pub97 == 1 & COLG_B == 1
gen WEMpubTOT97 = DURATION if WDAYWEND == 0 & pub97 == 1 & COLG_B == 1
gen WDFpubTOT97 = DURATION if WDAYWEND == 1 & pub97 == 1 & COLG_C == 1
gen WEFpubTOT97 = DURATION if WDAYWEND == 0 & pub97 == 1 & COLG_C == 1
gen WDApubTOT97 = DURATION if WDAYWEND == 1 & pub97 == 1 & COLG_B == 1 & COLG_C == 1
gen WEApubTOT97 = DURATION if WDAYWEND == 1 & pub97 == 1 & COLG_B == 1 & COLG_C == 1
gen WDNpubTOT97 = DURATION if WDAYWEND == 1 & pub97 == 1 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
gen WENpubTOT97 = DURATION if WDAYWEND == 0 & pub97 == 1 & (COLG_B == 1 | COLG_C == 1) & COLG_D == 0
replace WDMpubTOT97 = 0 if WDMpubTOT97 == .
replace WEMpubTOT97 = 0 if WEMpubTOT97 == .
replace WDFpubTOT97 = 0 if WDFpubTOT97 == .
replace WEFpubTOT97 = 0 if WEFpubTOT97 == .
replace WDNpubTOT97 = 0 if WDNpubTOT97 == .
replace WENpubTOT97 = 0 if WENpubTOT97 == .
replace WDApubTOT97 = 0 if WDApubTOT97 == .
replace WEApubTOT97 = 0 if WEApubTOT97 == .
bysort ER30001 ER30002: egen MpubTOT97 = total((5*WDMpubTOT97+ 2*WEMpubTOT97)/3600)
bysort ER30001 ER30002: egen FpubTOT97 = total((5*WDFpubTOT97+ 2*WEFpubTOT97)/3600)
bysort ER30001 ER30002: egen NpubTOT97 = total((5*WDNpubTOT97+ 2*WENpubTOT97)/3600)
bysort ER30001 ER30002: egen ApubTOT97 = total((5*WDApubTOT97+ 2*WEApubTOT97)/3600)
gen BpubTOT97 = FpubTOT97 + MpubTOT97
gen OpubTOT97 = BpubTOT97 - ApubTOT97
egen wday = max(T1) if WDAYWEND == 1
egen wend = max(T1) if WDAYWEND == 0
/*drop WDMTOT97 WEMTOT97 WDFTOT97 WEFTOT97 WDFTOT97 WEFTOT97 WDATOT97 WEATOT97*/
drop COL* DURATION 
drop WDAYWEND T1
duplicates drop ER30001 ER30002, force
save tdag97.dta, replace

* End 1997 time diary aggregation
**********************************************************************

