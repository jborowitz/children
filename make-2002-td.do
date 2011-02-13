**********************************************************************
* Begin 2002 time diary aggregation
clear matrix
clear
set more off
set maxvar 10000
set mem 500m
use td02.dta
local reclist02 "9090 9190 9290 9390 5020 5030 6310 6311 6312 6313 6320 6321 6322 6323 6330 6331 6332 6333 6340 6341 6342 6343 6350 6351 6352 6353 6510 6520 6410 6420 6430 6440 6010 6020 6710 6720 6610 6620 6210 6220 6110 6120 6130 6131 6132 6133 6134 6135 6136 6137 6138 6890 7090 7091 7092 7093 7094 7095 7096 7097 7098 7099 70290 7390 7490 7491 7190 7520 7690 7710 7720 7890 8810 8850 8851 8852 8853 8854 8860 8861 8862 8863 8864 8865 8870 8871 8872 8880 8830 8831 8832 8833 8834 8835 8836 8837 8838 8839 8840 8841 8842 8843 8844 8845 8846 8847 8010 8011 8012 8013 8014 8015 8016 8017 8020 8021 8022 8023 8024 8030 8040 8041 8042 8050 8051 8052 8053 8054 8055 8060 8061 8062 8032 8070 8080 8090 8091 8092 8100 8650 8110 8120 8130 8140 8150 8160 8170 8180 8240 8241 8242 8250 8260 8210 8220 8230 8310 8320 8330 8340 8350 8410 8420 8430 8440 8510 8511 8512 8513 8514 8520 8521 8522 8523 8610 8611 8612 8620 8630 8640 8660 8710 8720 8730 8740 8750 8760 8770 8780 8790 8820 8890 9610 9620 9810 9820 9830 9890"
local rec1list02 `reclist02'
local rec2list02 `reclist02'
local rec3list02 `reclist02'
local rec4list02 `reclist02'
local publist02 ""
local varslist02 "`reclist02' `basiclist02' `educlist02' `travellist02'"
local basiclist02 "4390 4490 4840 4850 4860 4870 4880 9660 9661 9662 9670 9630"
/*local basiclist02 "4880 9660 9661 9662 9670 9630"*/
local educlist02 "5010 5040 5050 5010 5100 5110 5111 5120 5190 5191 5192 5490 5491 5492 5493 5494 9410 9590 9420 9430"
local travellist02 "4980 4990 5390 5970 5980 5990 6980 6990 7990 8990 9990"
sort ER30001 ER30002
gen basic02 = 0
foreach i of local basiclist02 {
  replace basic02 = 1 if COLA_02 == `i'
}
gen educ02 = 0
foreach i of local educlist02 {
  replace educ02 = 1 if COLA_02 == `i'
}
gen travel02 = 0
foreach i of local travellist02 {
  replace travel02 = 1 if COLA_02 == `i'
}
gen rec02 = 0
foreach i of local reclist02 {
  replace rec02 = 1 if COLA_02 == `i'
}
gen pub02 = 0
foreach i of local publist02 {
  replace pub02 = 1 if COLA_02 == `i'
}
gen priv02 = 0
foreach i of local privlist02 {
  replace priv02 = 1 if COLA_02 == `i'
}
gen pubbasic02 = 1 if pub02 == 1 & basic02 == 1
gen pubrec02 = 1 if pub02 == 1 & rec02 == 1
gen pubeduc02 = 1 if pub02 == 1 & educ02 == 1
gen pubtravel02 = 1 if pub02 == 1 & travel02 == 1
foreach var in pubbasic02 pubeduc02 pubtravel02 pubrec02 basic02 educ02 travel02 rec02 pub02 priv02{
  gen WDM`var' = DUR_02 if `var'  & DIARY_02 == 1 & COLGB_02 == 1
  gen WDF`var' = DUR_02 if `var'  & DIARY_02 == 1 & COLGC_02 == 1
  gen WDA`var' = DUR_02 if `var'  & DIARY_02 == 1 & COLGC_02 == 1 & COLGB_02 == 1
  gen WDN`var' = DUR_02 if `var'  & DIARY_02 == 1 & (COLGC_02 == 1 | COLGB_02 == 1 ) & COLGD_02 != 1
  replace WDM`var' = 0 if WDM`var' == .
  replace WDF`var' = 0 if WDF`var' == .
  replace WDN`var' = 0 if WDN`var' == .
  replace WDA`var' = 0 if WDA`var' == .
  gen WEM`var' = DUR_02 if `var'  & DIARY_02 == 0 & COLGB_02 == 1
  gen WEF`var' = DUR_02 if `var'  & DIARY_02 == 0 & COLGC_02 == 1
  gen WEA`var' = DUR_02 if `var'  & DIARY_02 == 1 & COLGC_02 == 1 & COLGB_02 == 1
  gen WEN`var' = DUR_02 if `var'  & DIARY_02 == 0 & (COLGC_02 == 1 | COLGB_02 == 1 ) & COLGD_02 != 1
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
gen MSUM02 = Mbasic02 + Meduc02 + Mtravel02 + Mrec02
gen FSUM02 = Fbasic02 + Feduc02 + Ftravel02 + Frec02
gen BSUM02 = Bbasic02 + Beduc02 + Btravel02 + Brec02
gen NSUM02 = Nbasic02 + Neduc02 + Ntravel02 + Nrec02
gen ASUM02 = Abasic02 + Aeduc02 + Atravel02 + Arec02
gen MpubSUM02 = Mpubbasic02 + Mpubeduc02 + Mpubtravel02 + Mpubrec02
gen FpubSUM02 = Fpubbasic02 + Fpubeduc02 + Fpubtravel02 + Fpubrec02
gen BpubSUM02 = Bpubbasic02 + Bpubeduc02 + Bpubtravel02 + Bpubrec02
gen NpubSUM02 = Npubbasic02 + Npubeduc02 + Npubtravel02 + Npubrec02
gen ApubSUM02 = Apubbasic02 + Apubeduc02 + Apubtravel02 + Apubrec02
gen Mnotravel02 = MSUM02 - Mtravel02
gen Fnotravel02 = FSUM02 - Ftravel02
gen Bnotravel02 = BSUM02 - Btravel02
gen Nnotravel02 = NSUM02 - Ntravel02
gen Anotravel02 = ASUM02 - Atravel02
gen WDMTOT02 = DUR_02 if DIARY_02 == 1 & COLGB_02 == 1
gen WEMTOT02 = DUR_02 if DIARY_02 == 0 & COLGB_02 == 1
gen WDFTOT02 = DUR_02 if DIARY_02 == 1 & COLGC_02 == 1
gen WEFTOT02 = DUR_02 if DIARY_02 == 0 & COLGC_02 == 1
gen WDNTOT02 = DUR_02 if DIARY_02 == 1 & (COLGC_02 == 1 | COLGB_02 == 1 ) & COLGD_02 != 1
gen WENTOT02 = DUR_02 if DIARY_02 == 0 & (COLGC_02 == 1 | COLGB_02 == 1 ) & COLGD_02 != 1
gen WDATOT02 = DUR_02 if DIARY_02 == 1 & COLGC_02 == 1 & COLGB_02 == 1
gen WEATOT02 = DUR_02 if DIARY_02 == 0 & COLGC_02 == 1 & COLGB_02 == 1
replace WDMTOT02 = 0 if WDMTOT02 == .
replace WEMTOT02 = 0 if WEMTOT02 == .
replace WDFTOT02 = 0 if WDFTOT02 == .
replace WEFTOT02 = 0 if WEFTOT02 == .
replace WDNTOT02 = 0 if WDNTOT02 == .
replace WENTOT02 = 0 if WENTOT02 == .
replace WDATOT02 = 0 if WDATOT02 == .
replace WEATOT02 = 0 if WEATOT02 == .
bysort ER30001 ER30002: egen MTOT02 = total((5*WDMTOT02+ 2*WEMTOT02)/3600)
bysort ER30001 ER30002: egen FTOT02 = total((5*WDFTOT02+ 2*WEFTOT02)/3600)
bysort ER30001 ER30002: egen NTOT02 = total((5*WDNTOT02+ 2*WENTOT02)/3600)
bysort ER30001 ER30002: egen ATOT02 = total((5*WDATOT02+ 2*WEATOT02)/3600)
gen WDMpubTOT02 = DUR_02 if DIARY_02 == 1 & pub02 == 1 & COLGB_02 == 1
gen WEMpubTOT02 = DUR_02 if DIARY_02 == 0 & pub02 == 1 & COLGB_02 == 1
gen WDFpubTOT02 = DUR_02 if DIARY_02 == 1 & pub02 == 1 & COLGC_02 == 1
gen WEFpubTOT02 = DUR_02 if DIARY_02 == 0 & pub02 == 1 & COLGC_02 == 1
gen WDNpubTOT02 = DUR_02 if DIARY_02 == 1 & pub02 == 1 & (COLGC_02 == 1 | COLGB_02 == 1 ) & COLGD_02 != 1
gen WENpubTOT02 = DUR_02 if DIARY_02 == 0 & pub02 == 1 & (COLGC_02 == 1 | COLGB_02 == 1 ) & COLGD_02 != 1
gen WDApubTOT02 = DUR_02 if DIARY_02 == 1 & pub02 == 1 & COLGC_02 == 1 & COLGB_02 == 1
gen WEApubTOT02 = DUR_02 if DIARY_02 == 0 & pub02 == 1 & COLGC_02 == 1 & COLGB_02 == 1
replace WDMpubTOT02 = 0 if WDMpubTOT02 == .
replace WEMpubTOT02 = 0 if WEMpubTOT02 == .
replace WDFpubTOT02 = 0 if WDFpubTOT02 == .
replace WEFpubTOT02 = 0 if WEFpubTOT02 == .
replace WDNpubTOT02 = 0 if WDNpubTOT02 == .
replace WENpubTOT02 = 0 if WENpubTOT02 == .
replace WDApubTOT02 = 0 if WDApubTOT02 == .
replace WEApubTOT02 = 0 if WEApubTOT02 == .
bysort ER30001 ER30002: egen MpubTOT02 = total((5*WDMpubTOT02+ 2*WEMpubTOT02)/3600)
bysort ER30001 ER30002: egen FpubTOT02 = total((5*WDFpubTOT02+ 2*WEFpubTOT02)/3600)
bysort ER30001 ER30002: egen NpubTOT02 = total((5*WDNpubTOT02+ 2*WENpubTOT02)/3600)
bysort ER30001 ER30002: egen ApubTOT02 = total((5*WDApubTOT02+ 2*WEApubTOT02)/3600)
gen BpubTOT02 = FpubTOT02 + MpubTOT02
gen OpubTOT02 = BpubTOT02 - ApubTOT02
gen BTOT02 = FTOT02 + MTOT02
gen OTOT02 = BTOT02 - ATOT02
gen Onotravel02 = Bnotravel02 - Anotravel02
gen OSUM02 = BSUM02 - ASUM02
/*drop WDMTOT02 WEMTOT02 WDFTOT02 WEFTOT02 WDNTOT02 WENTOT02 WDATOT02 WEATOT02*/
/*drop COL* DIARY_02 DUR_02 */
duplicates drop ER30001 ER30002, force
save tdag02.dta, replace
* End 2002 time diary aggregation
**********************************************************************
