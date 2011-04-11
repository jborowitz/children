**********************************************************************
* Begin 2007 time diary aggregation
clear 
set more off
use td07.dta
local reclist07 "9090 9190 9290 9390 5020 5021 5030 5031 6310 6311 6312 6313 6320 6321 6322 6323 6330 6331 6332 6333 6340 6341 6342 6343 6350 6351 6352 6353 6510 6520 6410 6420 6430 6440 6010 6020 6710 6711 6712 6720 6721 6722 6610 6620 6210 6220 6110 6120 6130 6131 6132 6133 6134 6135 6136 6137 6138 6890 7090 7091 7092 7093 7094 7095 7096 7097 7098 7099 70290 7390 7490 7491 7190 7520 7690 7710 7720 7890 8810 8850 8851 8852 8853 8854 8860 8861 8862 8863 8864 8865 8870 8871 8872 8880 8830 8831 8832 8833 8834 8835 8836 8837 8838 8839 8840 8841 8842 8843 8844 8845 8846 8847 8010 8011 8012 8013 8014 8015 8016 8017 8020 8021 8022 8023 8024 8030 8040 8041 8042 8050 8051 8052 8053 8054 8055 8060 8061 8062 8032 8070 8080 8090 8091 8092 8100 8650 8110 8120 8130 8140 8150 8160 8170 8180 8240 8241 8242 8250 8260 8210 8220 8230 8310 8320 8330 8340 8350 8410 8420 8430 8440 8510 8511 8512 8513 8514 8520 8521 8522 8523 8610 8611 8612 8620 8630 8640 8660 8710 8720 8730 8740 8750 8760 8770 8780 8790 8820 8890 9610 9620 9810 9820 9830 9890"
local basiclist07 "4390 4490 4840 4850 4860 4870 4880 9660 9661 9662 9670 9630"
local educlist07 "5010 5040 5050 5051 5010 5100 5101 5110 5120 5190 5191 5192 5490 5491 5492 5493 5494 9410 9590 9420 9430"
local travellist07 "4980 4990 5390 5970 5980 5990 6980 6990 7990 8990 9990"
local rec1list07 `reclist07'
local rec2list07 `reclist07'
local rec3list07 `reclist07'
local rec4list07 `reclist07'
local varslist07 "`reclist07' `basiclist07' `educlist07' `travellist07'"
sort ER30001 ER30002
gen basic07 = 0
foreach i of local basiclist07 {
  replace basic07 = 1 if COLA_07 == `i'
}
gen educ07 = 0
foreach i of local educlist07 {
  replace educ07 = 1 if COLA_07 == `i'
}
gen travel07 = 0
foreach i of local travellist07 {
  replace travel07 = 1 if COLA_07 == `i'
}
gen rec07 = 0
foreach i of local reclist07 {
  replace rec07 = 1 if COLA_07 == `i'
}
gen rec107 = 0
foreach i of local rec1list07 {
  replace rec107 = 1 if COLA_07 == `i'
}
gen rec207 = 0
foreach i of local rec2list07 {
  replace rec207 = 1 if COLA_07 == `i'
}
gen rec307 = 0
foreach i of local rec3list07 {
  replace rec307 = 1 if COLA_07 == `i'
}
gen rec407 = 0
foreach i of local rec4list07 {
  replace rec407 = 1 if COLA_07 == `i'
}
foreach var in basic07 educ07 travel07 rec07 rec107 rec207 rec307 rec407 {
  gen WDM`var' = DUR_07 if `var'  & DIARY_07 == 1 & COLHB_07 == 1
  gen WDF`var' = DUR_07 if `var'  & DIARY_07 == 1 & COLHC_07 == 1
  gen WDA`var' = DUR_07 if `var'  & DIARY_07 == 1 & COLHC_07 == 1 & COLHB_07 == 1
  gen WDN`var' = DUR_07 if `var'  & DIARY_07 == 1 & (COLHC_07 == 1 | COLHB_07 == 1 ) & COLHD_07 != 1
  replace WDM`var' = 0 if WDM`var' == .
  replace WDF`var' = 0 if WDF`var' == .
  replace WDN`var' = 0 if WDN`var' == .
  replace WDA`var' = 0 if WDA`var' == .
  gen WEM`var' = DUR_07 if `var'  & DIARY_07 == 0 & COLHB_07 == 1
  gen WEF`var' = DUR_07 if `var'  & DIARY_07 == 0 & COLHC_07 == 1
  gen WEA`var' = DUR_07 if `var'  & DIARY_07 == 1 & COLHC_07 == 1 & COLHB_07 == 1
  gen WEN`var' = DUR_07 if `var'  & DIARY_07 == 0 & (COLHC_07 == 1 | COLHB_07 == 1 ) & COLHD_07 != 1
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
gen MSUM07 = Mbasic07 + Meduc07 + Mtravel07 + Mrec07
gen FSUM07 = Fbasic07 + Feduc07 + Ftravel07 + Frec07
gen BSUM07 = Bbasic07 + Beduc07 + Btravel07 + Brec07
gen NSUM07 = Nbasic07 + Neduc07 + Ntravel07 + Nrec07
gen ASUM07 = Abasic07 + Aeduc07 + Atravel07 + Arec07
gen Mnotravel07 = MSUM07 - Mtravel07
gen Fnotravel07 = FSUM07 - Ftravel07
gen Bnotravel07 = BSUM07 - Btravel07
gen Nnotravel07 = NSUM07 - Ntravel07
gen Anotravel07 = ASUM07 - Atravel07
gen WDMTOT07 = DUR_07 if DIARY_07 == 1 & COLHB_07 == 1
gen WEMTOT07 = DUR_07 if DIARY_07 == 0 & COLHB_07 == 1
gen WDFTOT07 = DUR_07 if DIARY_07 == 1 & COLHC_07 == 1
gen WEFTOT07 = DUR_07 if DIARY_07 == 0 & COLHC_07 == 1
gen WDNTOT07 = DUR_07 if DIARY_07 == 1 & (COLHC_07 == 1 | COLHB_07 == 1 ) & COLHD_07 != 1
gen WENTOT07 = DUR_07 if DIARY_07 == 0 & (COLHC_07 == 1 | COLHB_07 == 1 ) & COLHD_07 != 1
gen WDATOT07 = DUR_07 if DIARY_07 == 1 & COLHC_07 == 1 & COLHB_07 == 1
gen WEATOT07 = DUR_07 if DIARY_07 == 0 & COLHC_07 == 1 & COLHB_07 == 1
replace WDMTOT07 = 0 if WDMTOT07 == .
replace WEMTOT07 = 0 if WEMTOT07 == .
replace WDFTOT07 = 0 if WDFTOT07 == .
replace WEFTOT07 = 0 if WEFTOT07 == .
replace WDNTOT07 = 0 if WDNTOT07 == .
replace WENTOT07 = 0 if WENTOT07 == .
replace WDATOT07 = 0 if WDATOT07 == .
replace WEATOT07 = 0 if WEATOT07 == .
bysort ER30001 ER30002: egen MTOT07 = total((5*WDMTOT07+ 2*WEMTOT07)/3600)
bysort ER30001 ER30002: egen FTOT07 = total((5*WDFTOT07+ 2*WEFTOT07)/3600)
bysort ER30001 ER30002: egen NTOT07 = total((5*WDNTOT07+ 2*WENTOT07)/3600)
bysort ER30001 ER30002: egen ATOT07 = total((5*WDATOT07+ 2*WEATOT07)/3600)
gen BTOT07 = FTOT07 + MTOT07
gen OTOT07 = BTOT07 - ATOT07
gen Mnobasic07 = MTOT07 - Mbasic07
gen Fnobasic07 = FTOT07 - Fbasic07
gen Bnobasic07 = BTOT07 - Bbasic07
gen Nnobasic07 = NTOT07 - Nbasic07
gen Anobasic07 = ATOT07 - Abasic07
gen Onotravel07 = Bnotravel07 - Anotravel07
gen OSUM07 = BSUM07 - ASUM07
/*drop WDMTOT07 WEMTOT07 WDFTOT07 WEFTOT07 WDNTOT07 WENTOT07 WDATOT07 WEATOT07*/
drop COL* DIARY_07 DUR_07 
duplicates drop ER30001 ER30002, force
save tdag07.dta, replace
* End 2007 time diary aggregation
**********************************************************************

