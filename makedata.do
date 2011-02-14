*********************************************************************
*
  * Begin time diary aggregation
clear
clear matrix
set mem 200m
set more off
set maxvar 10000

global outputlist "ER30001"
local personlist "M F B N O"
/*Dictionary of people*/
/*M - Mother*/
/*F - Father*/
/*B - Father or mother, double counting*/
/*N - Father or mother, single counting but no siblings*/
/*O - Father or mother, single counting*/

/*local categorylist "rec97 educ97 basic97 travel97 notravel97 SUM97 TOT97 488 439 943 rec02 educ02 basic02 travel02 notravel02 SUM02 TOT02 rec197 rec297 rec397 rec497 "*/
/*local yearlist "rec97 educ97 basic97 travel97 notravel97 SUM97 TOT97 488 439 943 rec02 educ02 basic02 travel02 notravel02 SUM02 TOT02 rec197 rec297 rec397 rec497 "*/

local categorylist "pubrec pubeduc pubbasic pubtravel pubTOT rec educ basic travel notravel SUM TOT rec1 rec2 rec3 rec4 pub priv"
local yearlist "97 02 "
/*
2002 time codes

recreation:
9090 9190 9290 9390 5020 5030 6310 6311 6312 6313 6320 6321 6322 6323 6330 6331 6332 6333 6340 6341 6342 6343 6350 6351 6352 6353 6510 6520 6410 6420 6430 6440 6010 6020 6710 6720 6610 6620 6210 6220 6110 6120 6130 6131 6132 6133 6134 6135 6136 6137 6138 6890 7090 7091 7092 7093 7094 7095 7096 7097 7098 7099 70290 7390 7490 7491 7190 7520 7690 7710 7720 7890 8810 8850 8851 8852 8853 8854 8860 8861 8862 8863 8864 8865 8870 8871 8872 8880 8830 8831 8832 8833 8834 8835 8836 8837 8838 8839 8840 8841 8842 8843 8844 8845 8846 8847 8010 8011 8012 8013 8014 8015 8016 8017 8020 8021 8022 8023 8024 8030 8040 8041 8042 8050 8051 8052 8053 8054 8055 8060 8061 8062 8032 8070 8080 8090 8091 8092 8100 8650 8110 8120 8130 8140 8150 8160 8170 8180 8240 8241 8242 8250 8260 8210 8220 8230 8310 8320 8330 8340 8350 8410 8420 8430 8440 8510 8511 8512 8513 8514 8520 8521 8522 8523 8610 8611 8612 8620 8630 8640 8660 8710 8720 8730 8740 8750 8760 8770 8780 8790 8820 8890 9610 9620 9810 9820 9830 9890

basic:
4880 9660 9661 9662 9670 9630

education:
5010 5040 5050 5010 5110 5120 5190 5191 5192 5490 5491 5492 5493 5494 9410 9590 9420 9430

travel:
4980 4990 5390 5970 5980 5990 6980 6990 7990 8990 9990

*/

/*
Dictionary of time codes:

943 - being read to

*/
/*

This file creates a variable called "M###" which indicates the number
of seconds that the kid would have spent had the weekend and weekday
sampled been repeated for an entire week.  The output data is saved as
one M variable for each code in extralist97, plus the identifiers ER30001
and ER30002.  

*/
/*
basic:

209 219 218 248 221 258 222 236 237 239 249 259 238 269 278 277 279 439 449 484 488 489 487 485 486 966 967

travel:

498 499 597 598 599 698 699 799 899 999

recreation:

502 503  631 632 633 634 635 651 652 641 643 644 601 602 671 672 661 662 611 612 689 709 729 739 749 719 752 769 771 772 789 883 884 801 802 803 804 805 806 807 808 809 810 865 811 812 813 814 815 816 817 818 821 822 823 824 825 826 831 832 833 834 835 841 842 843 844 851 852 861 862 863 864 866 871 872 873 874 875 876 877 878 879 882 909 919 929 961 962 964 965 979 981 982 983 963 989 

education

501 504 505 509 510 511 512 549 569 939 941 959 942 943

*/
/*do make-1997-td.do*/
/*do make-2002-td.do*/
/*do make-2007-td.do*/

  use test-merged, clear
keep if _merge == 3
drop _merge
bysort PCGID_97 PCGPN_97: gen numkids = _N
by PCGID_97 PCGPN_97: gen kidnum = _n

/*reg MTOT97 RT RTXyoungkid RTXonly*/
/*reg MTOT97 RT RTXonly if chagem = maxage*/
/*drop if numkids == 1*/
**********************************************************************
  * Begin main cds work
**********************************************************************
**********************************************************************
* Renaming
rename ER33406 birthyear97
rename ER33906 birthyear07
rename ER33405 birthmonth97
rename ER33905 birthmonth07
/*rename ER33415 mschooling97*/
/*schooling is now done later without renaming*/
rename CHIWMON chintmonth
rename PCGIWMON pcgintmonth
rename Q23IWMTH chintmonth02
rename Q23IWYR chintyear02
rename Q33IWMTH chintmonth07
rename Q33IWYR chintyear07
rename ER10013 ageyoungest
rename ER21021 ageyoungest02
rename CHRACE race
/*rename ER10036 housevalue*/
replace chintmonth = pcgintmonth if chintmonth == 0 | chintmonth > 12
rename ER9248 statefip
rename Q1A8P bwlb
rename Q1A8O bwoz
rename Q3LW_SS score
rename Q3BMA_SS nmathscore
replace nmathscore = . if nmathscore == 999
rename Q3BRE_SS nreadscore
replace nreadscore = . if nreadscore == 999
rename Q24BRSS nreadscore02
rename Q24APSS nappliedscore02
replace nreadscore02 = . if nreadscore02 == 999
replace nappliedscore02 = . if nappliedscore02 == 999
rename ER10008 numfamunit
**********************************************************************
  
**********************************************************************
  * merge in time diary data
merge 1:1 ER30001 ER30002 using tdag97.dta
tab _merge
keep if _merge == 3
rename _merge _merge97
merge 1:1 ER30001 ER30002 using tdag02.dta
tab _merge
rename _merge _merge02
merge 1:1 ER30001 ER30002 using tdag07.dta
tab _merge
rename _merge _merge07
**********************************************************************
  
  save temp.dta, replace
**********************************************************************
  * merge in State data
gen black = race == 2
  sort statefip black
  merge m:1 statefip black using married.dta
**********************************************************************


**********************************************************************
  * Variable Generation
  rename V92 location
  gen urban = location <= 2 | location == 101 | location == 101 | location == 201 | location == 202 | location == 301 | location == 302 
  /*Hundreds digit=2, Units digit=1 or 2: Central cities in the Northeast of*/
  /*large metropolitan areas.*/
  /*Hundreds digit=2, Units digit=3 or 4: Suburban areas of large metropolitan*/
  /*areas in the Northeast.*/
  /*Hundreds digit=1, Units digit=1 or 2: Central cities of large metropolitan*/
  /*areas, North Central.*/
  /*Hundreds digit=1, Units digit=3 or 4: Suburbs of large metropolitan areas,*/
  /*North Central.*/
  /*Hundreds digit=3, Units digit=1 or 2: Central cities of large metropolitan*/
  /*areas, South.*/
  /*Hundreds digit=3, Units digit=3 or 4: Suburbs of large metropolitan areas,*/
  /*South.*/
  /*Hundreds digit=0, Units digit=1 or 2: Central cities of large metropolitan*/
  /*areas, West.*/
  /*Hundreds digit=0, Units digit=3 or 4: Suburbs of large metropolitan areas,*/
  /*West.*/
  /*Hundreds digit=9, Units digit=5,6,9 : Northeast, area is a Standard*/
  /*Metropolitan Statistical Area (contains a city of 50,000 or more).*/
  /*Hundreds digit=9, Units digit=7,8: Northeast, not SMSA.*/
  /*Hundreds digit=6,7 Units digit=5,6,9: North central, area contains a SMSA.*/
  /*Hundreds digit=6,7 Units digit=7,8: North central, no SMSA in area.*/
  /*Hundreds digit=3,4,5 Units digit=5,6,9: South, SMSA.*/
  /*Hundreds digit=3,4,5 Units digit=7,8: South, not a SMSA.*/
  /*Hundreds digit=8 Units digit=5,6,9: West, SMSA.*/
  /*Hundreds digit=8 Units digit=7,8: West, not a SMSA.*/

  /*This is coded even for the Census sample, but there is a separate Census*/
  /*Primary Sampling Unit designation (see Variable 132).*/
gen dadworkhours96  = ER12174 
gen momworkhours96  = ER12185 
gen workhours96=dadworkhours96 + momworkhours96
gen dadworkhours02  = ER24080 
gen momworkhours02  = ER24091 
gen workhours02=dadworkhours02 + momworkhours02
gen dworkhours96 = workhours96-workhours02
gen ddadworkhours96 = dadworkhours96-dadworkhours02
gen dmomworkhours96 = momworkhours96-momworkhours02
gen dadwage96  = ER12217 
gen momwage96  = ER12218 
gen wage96=dadwage96 + momwage96
gen dadwage02  = ER24137 
gen momwage02  = ER24138 
gen wage02=dadwage02 + momwage02
gen dwage96 = wage96-wage02
gen ddadwage96 = dadwage96-dadwage02
gen dmomwage96 = momwage96-momwage02
rename ER12082 mlaborincome
rename ER12080 flaborincome
gen laborincome = mlaborincome + flaborincome
rename ER12223 mschooling97
rename ER12222 fschooling97
/*replace mschooling97 = mschooling97 - 1 + ER11780 */
/*replace fschooling97 = fschooling97 - 1 + ER11868*/
gen kidhsdip = TA070548 == 1 if TA070548 != .
gen arrested = (TA070841 == 2 | TA070841  == 3) if TA070841 != .
label variable arrested "Arrested"
label variable kidhsdip "HS Grad"
gen chagem = 12*(1997 - birthyear97) + (chintmonth - birthmonth97)
gen chagem02 = 12*(chintyear02 - birthyear97) + (chintmonth02 - birthmonth97)
gen chagem07 = 12*(chintyear07 - birthyear97) + (chintmonth07 - birthmonth97)
bysort PCGID_97 PCGPN_97: egen maxage = max(chagem)
bysort PCGID_97 PCGPN_97: egen minage = min(chagem)
gen minageyears = round(minage / 12)
gen maxageyears = round(maxage / 12)
gen devstage97 = 1 if chagem < 13
replace devstage97 = 2 if chagem >= 13 & chagem < 36
replace devstage97 = 3 if chagem >= 36 & chagem < 60
replace devstage97 = 4 if chagem >= 60 
bysort PCGID_97 PCGPN_97: egen maxdevstage97 = max(devstage97)
bysort PCGID_97 PCGPN_97: egen mindevstage97 = min(devstage97)
gen devstage02 = 1 if chagem02 < 13
replace devstage02 = 2 if chagem02 >= 13 & chagem02 < 36
replace devstage02 = 3 if chagem02 >= 36 & chagem02 < 60
replace devstage02 = 4 if chagem02 >= 60 
bysort PCGID_97 PCGPN_97: egen maxdevstage02 = max(devstage02)
bysort PCGID_97 PCGPN_97: egen mindevstage02 = min(devstage02)
gen devstage07 = 1 if chagem07 < 13
replace devstage07 = 2 if chagem07 >= 13 & chagem07 < 36
replace devstage07 = 3 if chagem07 >= 36 & chagem07 < 60
replace devstage07 = 4 if chagem07 >= 60 
bysort PCGID_97 PCGPN_97: egen maxdevstage07 = max(devstage07)
bysort PCGID_97 PCGPN_97: egen mindevstage07 = min(devstage07)
local stagecut 6
gen stagediff = minageyears <= `stagecut' & maxageyears > `stagecut'
gen stage1 = minageyears <= `stagecut' & maxageyears <= `stagecut'
gen stage2 = minageyears > `stagecut' & maxageyears > `stagecut'
gen chage = round(chagem/12)
gen chage2 = chage^2
gen chage3 = chage^3
gen chage4 = chage^4
gen chage5 = chage^5
gen chagem2 = chagem^2/1000
gen chagem3 = chagem^3
gen chagem4 = chagem^4
gen chagem5 = chagem^5
gen agediff = abs(maxage - minage)
gen yearsdiff = round(agediff/12)
gen agediff2 = agediff^2
gen agediff3 = agediff^3
* Note: there are a bunch of yearsdiff = 0 guys, of whom I think many are twins but others are coding errors which I THINK is due to not actually having two different kids in the sample
/*gen homeowner = housevalue != 0*/
gen tempchage = -1 * chagem
sort PCGID_97 PCGPN_97 tempchage
by PCGID_97 PCGPN_97: gen birthordercds = _n
drop tempchage
gen RTXbirthorder = RT * birthorder
gen RTXyoungkid = RT
gen RTXyoungkid2 = RT
gen RTX2 = RT
gen RT3 = 1-(1/RT)
replace RTXyoungkid = 0 if maxage == chagem
replace RTXyoungkid2 = 0 if maxage == chagem & numkids > 1
replace RTX2 = 0 if numkids == 1
gen RTXonly = RT
replace RTXonly = 0 if numkids > 1
label variable RTXyoungkid "RT * Not Older"
label variable RTXyoungkid2 "RT * Younger of 2"
label variable RTXonly "RT * Only Child"
gen dadeverdrinks = 1 if ER15550 == 1
replace dadeverdrinks = 0 if ER15550 == 5
gen momeverdrinks = 1 if ER15658 == 1
replace momeverdrinks = 0 if ER15658 == 5
gen dadeversmoked = 1 if ER15546 == 1
replace dadeversmoked = 0 if ER15546 == 5
gen momeversmoked = 1 if ER15654 == 1
replace momeversmoked = 0 if ER15654 == 5
gen dadsmokes = 1 if ER15543 == 1
replace dadsmokes = 0 if ER15543 == 5
gen momsmokes = 1 if ER15651 == 1
replace momsmokes = 0 if ER15651 == 5
gen businessowner = 1 if ER8193 == 1
replace businessowner = 0 if ER8193 == 5
gen married = ER10016 == 1 if ER33403 < 30
replace married = 0 if ER32049 == 2
replace married = 1 if ER32049 == 1 & ER32039 > 1997 & ER32036 <= 1997
replace married = 1 if ER32049 == 1 & ER32046 > 1997 & ER32043 <= 1997
gen parentage = 1997 - parentbirthyear07
gen parentage2 = parentage^2
gen parentage3 = parentage^3
gen ageyoungest2 = ageyoungest^2
gen ageyoungest3 = ageyoungest^3
replace bwoz = 0 if bwoz == 96
gen badbw = bwoz > 97 | bwlb > 97
bysort PCGID_97 PCGPN_97: egen onebadbw = max(badbw)
gen bw = (16*bwlb + bwoz)*28.3495231
gen headinlf = ER10081 <= 3 & ER10081 > 0 | (ER10082 <= 3 & ER10082 > 0) | (ER10083 <= 3 & ER10083 > 0)
gen wifeinlf = ER10563 <= 3 & ER10563 > 0 | (ER10564 <= 3 & ER10564 > 0) | (ER10565 <= 3 & ER10565 > 0)
replace wifeinlf = . if married == 0
gen wifeoutlf = 1 if wifeinlf == 0
replace wifeoutlf = 0 if wifeinlf == 1
gen wifehrs = momworkhours96
replace wifehrs = wifehrs / 520
gen intact = RT != . & married == 1 & RELPCG97 == 1
gen intact2 = (RELPCG97 == 1 & (RELOCG97 == 1 | RELOCG97 == 4)) | (RELPCG97 == 4 & (RELOCG97 == 1 | RELOCG97 == 4))  
gen intact3 = RT != . & ER33303 <= 22
gen momhead = RT != . & ER33303 == 10 & RELPCG97 == 1
gen dadhead = RT != . & (ER33303 == 20 | ER33303 == 22 ) & RELPCG97 == 1
gen tested = score < 900 & score != .
gen mathtested = nmathscore < 900 & nmathscore != .
gen readtested = nreadscore < 900 & nreadscore != .
gen readtested02 = nreadscore02 < 900 & nreadscore02 != .
gen readdiff=nreadscore02 - nreadscore if readtested & readtested02
gen income = ER12079
gen income2 = income^2
gen income3 = income^3
gen income4 = income^4
gen income5 = income^5
gen income6 = income^6
xtile incomequintile = income, nquantiles(10)
quietly sum income 
gen highincome = income > r(mean)
/*gen laborincome = ER12082 + ER12080*/
gen RTI = dadhead*RT
gen RTY = yearsdiff*RT
gen RTA = agediff*RT
gen RTXwifeinlf = wifeinlf * RT
gen RTXwifeoutlf = wifeoutlf * RT
label variable RTI "RT * Father Head"
label variable RTY "RT * Years Age Diff"
label variable RTA "RT * Months Age Diff"
label variable RTXwifeinlf "RT * Wife In LF"
label variable RTXwifeoutlf "RT * Wife Home"
replace income  = income / 100000
gen avgscore = (nmathscore + nreadscore)/2
gen avgscore02 = (nappliedscore02 + nreadscore02)/2
gen avgdiff = avgscore02 - avgscore if avgscore != . & avgscore02 != .
replace avgdiff = 0 if avgdiff > 200 | avgdiff < -200
* create birthweight in grams
/* foreach var in `extralist97'{ */
/*   bysort PCGID_97 PCGPN_97: egen sdM`var' = sd(M`var') */
/*   gen diffM`var' = 1.41421356*sdM`var' */
/*   bysort PCGID_97 PCGPN_97: egen meanM`var' = mean(M`var') */
/*   bysort PCGID_97 PCGPN_97: egen nsdM`var' = sd(meanM`var') */
/*   bysort PCGID_97 PCGPN_97: egen sdF`var' = sd(F`var') */
/*   gen diffF`var' = 1.41421356*sdF`var' */
/*   bysort PCGID_97 PCGPN_97: egen meanF`var' = mean(F`var') */
/*   bysort PCGID_97 PCGPN_97: egen nsdF`var' = sd(meanF`var') */
/*   bysort PCGID_97 PCGPN_97: egen sdB`var' = sd(B`var') */
/*   gen diffB`var' = 1.41421356*sdB`var' */
/*   bysort PCGID_97 PCGPN_97: egen meanB`var' = mean(B`var') */
/*   bysort PCGID_97 PCGPN_97: egen nsdB`var' = sd(meanB`var') */

/* } */
local cut1 10
local cut2 20
local cut3 30
local cut4 40
local cut5 50
local cut6 60
local cut7 70
save temp.dta, replace
foreach person of local personlist{
  foreach category of local categorylist{
  gen self`person'`category'97 = `person'`category'97 - `person'`category'02
  gen self`person'`category'avg = (`person'`category'02 + `person'`category'97)/2
  foreach year of local yearlist{
    sort PCGID_97 PCGPN_97 chagem
    by PCGID_97 PCGPN_97: gen old`person'`category'`year' = `person'`category'`year' if _n == 2
    by PCGID_97 PCGPN_97: replace old`person'`category'`year' = old`person'`category'`year'[2]  if _n == 1
    by PCGID_97 PCGPN_97: gen young`person'`category'`year' = `person'`category'`year' if _n == 1
    by PCGID_97 PCGPN_97: replace young`person'`category'`year' = young`person'`category'`year'[1]  if _n == 2
    /*This code above to generate older and younger time investments hasn't been tested*/
    by PCGID_97 PCGPN_97: egen mean`person'`category'`year' = mean(`person'`category'`year')
    gen mean`person'`category'`year'2 = mean`person'`category'`year'^2
    gen mean`person'`category'`year'3 = mean`person'`category'`year'^3
    by PCGID_97 PCGPN_97: egen sd`person'`category'`year' = sd(`person'`category'`year')
    gen var`person'`category'`year' = sd`person'`category'`year'^2
    by PCGID_97 PCGPN_97: egen sum`person'`category'`year' = sum(`person'`category'`year')
    by PCGID_97 PCGPN_97: egen nsd`person'`category'`year' = sd(`person'`category'`year'/mean`person'`category'`year')
    by PCGID_97 PCGPN_97: gen diff`person'`category'`year' = 1.41421356*sd`person'`category'`year'
    xtile ddiff`person'`category'`year' = diff`person'`category'`year', nquantiles(10)
    xtile dold`person'`category'`year' = old`person'`category'`year', nquantiles(5)
    xtile dyoung`person'`category'`year' = young`person'`category'`year', nquantiles(5)
    /*gen ddiff`person'`category'`year' = 0 if diff`person'`category'`year' <=`cut1'*/
    /*[>replace ddiff`person'`category'`year' = 1 if diff`person'`category'`year' >`cut1'<]*/
    /*replace ddiff`person'`category'`year' = 1 if diff`person'`category'`year' > `cut1' & diff`person'`category'`year' <= `cut2'*/
    /*replace ddiff`person'`category'`year' = 2 if diff`person'`category'`year' > `cut2' & diff`person'`category'`year' <= `cut3'*/
    /*replace ddiff`person'`category'`year' = 3 if diff`person'`category'`year' > `cut3' & diff`person'`category'`year' <= `cut4' */
    /*replace ddiff`person'`category'`year' = 4 if diff`person'`category'`year' >= `cut4' */
    /*gen dold`person'`category'`year' = 0 if old`person'`category'`year' <= `cut1'*/
    /*replace dold`person'`category'`year' = 1 if old`person'`category'`year' > `cut1' & old`person'`category'`year' <= `cut2'*/
    /*replace dold`person'`category'`year' = 2 if old`person'`category'`year' > `cut2' & old`person'`category'`year' <= `cut3'*/
    /*replace dold`person'`category'`year' = 3 if old`person'`category'`year' > `cut3' & old`person'`category'`year' <= `cut4'*/
    /*replace dold`person'`category'`year' = 4 if old`person'`category'`year' > `cut4' & old`person'`category'`year' <= `cut5'*/
    /*replace dold`person'`category'`year' = 5 if old`person'`category'`year' > `cut5' & old`person'`category'`year' <= `cut6'*/
    /*replace dold`person'`category'`year' = 6 if old`person'`category'`year' > `cut6' & old`person'`category'`year' <= `cut7'*/
    /*replace dold`person'`category'`year' = 7 if old`person'`category'`year' > `cut7' */
    /*gen dyoung`person'`category'`year' = 0 if young`person'`category'`year' <= `cut1'*/
    /*replace dyoung`person'`category'`year' = 1 if young`person'`category'`year' > `cut1' & young`person'`category'`year' <= `cut2'*/
    /*replace dyoung`person'`category'`year' = 2 if young`person'`category'`year' > `cut2' & young`person'`category'`year' <= `cut3'*/
    /*replace dyoung`person'`category'`year' = 3 if young`person'`category'`year' > `cut3' & young`person'`category'`year' <= `cut4'*/
    /*replace dyoung`person'`category'`year' = 4 if young`person'`category'`year' > `cut4' & young`person'`category'`year' <= `cut5'*/
    /*replace dyoung`person'`category'`year' = 5 if young`person'`category'`year' > `cut5' & young`person'`category'`year' <= `cut6'*/
    /*replace dyoung`person'`category'`year' = 6 if young`person'`category'`year' > `cut6' & young`person'`category'`year' <= `cut7'*/
    /*replace dyoung`person'`category'`year' = 7 if young`person'`category'`year' > `cut7' */
    by PCGID_97 PCGPN_97: gen ndiff`person'`category'`year' = 1.41421356*sd`person'`category'`year'/mean`person'`category'`year'
    by PCGID_97 PCGPN_97: gen ldiff`person'`category'`year' = log(diff`person'`category'`year')
    by PCGID_97 PCGPN_97: gen lsd`person'`category'`year' = log(sd`person'`category'`year')
    *bysort PCGID_97 PCGPN_97 chagem: egen maxt = max(`person'`category'`year')
    sort PCGID_97 PCGPN_97 chagem
    by PCGID_97 PCGPN_97: gen sibval = `person'`category'`year'[_n-1]
    by PCGID_97 PCGPN_97: gen tempN = _N
    by PCGID_97 PCGPN_97: gen tempn = _n
    by PCGID_97 PCGPN_97: gen odiff`person'`category'`year' =  sibval - `person'`category'`year'
    by PCGID_97 PCGPN_97: replace odiff`person'`category'`year' = odiff`person'`category'`year'[_n+1] if odiff`person'`category'`year' == .
    /*sum odiff`person'`category'`year' sibval temp**/
    count if sibval == .
    count if odiff`person'`category'`year' == .
    count
    drop sibval temp*
    ****************
    * Begin generating difference by WJ score
    sort PCGID_97 PCGPN_97 avgscore02
    by PCGID_97 PCGPN_97: gen sibval = `person'`category'`year'[_n-1] if avgscore02[_n-1] <= 399 & avgscore02[_n-1] <= . & avgscore02 != 399 & avgscore02 != .
    by PCGID_97 PCGPN_97: gen tempN = _N
    by PCGID_97 PCGPN_97: gen tempn = _n
    by PCGID_97 PCGPN_97: gen sodiff`person'`category'`year' = `person'`category'`year' - sibval
    by PCGID_97 PCGPN_97: replace sodiff`person'`category'`year' = sodiff`person'`category'`year'[_n+1] if sodiff`person'`category'`year' == .
    /*sum sodiff`person'`category'`year' sibval temp**/
    count if sibval == .
    count if odiff`person'`category'`year' == .
    count
    drop sibval temp*
    * End generating difference by WJ score
    ****************
    /******************/
    /** Begin generating difference by sex*/
    /*sort PCGID_97 PCGPN_97 male*/
    /*by PCGID_97 PCGPN_97: gen sibval = bw[_n-1] if avgscore[_n-1] <= 399 & avgscore[_n-1] <= . & avgscore != 399 & avgscore != .*/
    /*by PCGID_97 PCGPN_97: gen tempN = _N*/
    /*by PCGID_97 PCGPN_97: gen tempn = _n*/
    /*by PCGID_97 PCGPN_97: gen bwodiff`person'`category'`year' = `person'`category'`year' - sibval*/
    /*by PCGID_97 PCGPN_97: replace bwodiff`person'`category'`year' = bwodiff`person'`category'`year'[_n+1] if bwodiff`person'`category'`year' == .*/
    /*sum bwodiff`person'`category'`year' sibval temp**/
    /*count if sibval == .*/
    /*count if odiff`person'`category'`year' == .*/
    /*count*/
    /*drop sibval temp**/
    /** End generating difference by WJ score*/
    /******************/
    ****************
    * Begin generating difference by birthweight
    sort PCGID_97 PCGPN_97 chagem
    by PCGID_97 PCGPN_97: gen sibval = bw[_n-1] 
    by PCGID_97 PCGPN_97: gen tempN = _N
    by PCGID_97 PCGPN_97: gen tempn = _n
    by PCGID_97 PCGPN_97: gen bwodiff`person'`category'`year' = `person'`category'`year' - sibval
    by PCGID_97 PCGPN_97: replace bwodiff`person'`category'`year' = bwodiff`person'`category'`year'[_n+1] if bwodiff`person'`category'`year' == .
    /*sum bwodiff`person'`category'`year' sibval temp**/
    count if sibval == .
    count if odiff`person'`category'`year' == .
    count
    drop sibval temp*
    * End generating difference by birthweight
    ****************
    xtile dodiff`person'`category'`year' = odiff`person'`category'`year', nquantiles(5)
  }
  gen vardiff`person'`category'97 = var`person'`category'97 - var`person'`category'02
  }
}
foreach person of local personlist{
    foreach year of local yearlist{
        gen `person'dev`year' = `person'basic`year' if devstage`year' == 1
        replace `person'dev`year' = `person'rec`year' if devstage`year' == 2
        replace `person'dev`year' = `person'educ`year' if devstage`year' == 3
        replace `person'dev`year' = `person'travel`year' if devstage`year' == 4
        gen young`person'dev`year' = `person'basic`year' if mindevstage`year' == 1
        replace young`person'dev`year' = `person'rec`year' if mindevstage`year' == 2
        replace young`person'dev`year' = `person'educ`year' if mindevstage`year' == 3
        replace young`person'dev`year' = `person'travel`year' if mindevstage`year' == 4
        gen old`person'dev`year' = `person'basic`year' if maxdevstage`year' == 1
        replace old`person'dev`year' = `person'rec`year' if maxdevstage`year' == 2
        replace old`person'dev`year' = `person'educ`year' if maxdevstage`year' == 3
        replace old`person'dev`year' = `person'travel`year' if maxdevstage`year' == 4
        gen odiff`person'dev`year' = young`person'dev`year' - old`person'dev`year'
    }
    gen self`person'dev97 = `person'dev97 - `person'dev02
}
gen Mfracbasic97 = Mpubbasic97/Mbasic97
gen Mfracrec97 = Mpubrec97/Mrec97
gen Mfraceduc97 = Mpubeduc97/Meduc97
gen Mfractravel97 = Mpubtravel97/Mtravel97
gen MfracTOT97 = MpubTOT97/MTOT97
gen Bfracbasic97 = Bpubbasic97/Bbasic97
gen Bfracrec97 = Bpubrec97/Brec97
gen Bfraceduc97 = Bpubeduc97/Beduc97
gen Bfractravel97 = Bpubtravel97/Btravel97
gen BfracTOT97 = BpubTOT97/BTOT97
gen Ffracbasic97 = Fpubbasic97/Fbasic97
gen Ffracrec97 = Fpubrec97/Frec97
gen Ffraceduc97 = Fpubeduc97/Feduc97
gen Ffractravel97 = Fpubtravel97/Ftravel97
gen FfracTOT97 = FpubTOT97/FTOT97
gen Mlobasic97 = log(Mfracbasic97/(1-Mfracbasic97))
gen Mlorec97 = log(Mfracrec97/(1-Mfracrec97))
gen Mloeduc97 = log(Mfraceduc97/(1-Mfraceduc97))
gen Mlotravel97 = log(Mfractravel97/(1-Mfractravel97))
gen MloTOT97 = log(MfracTOT97/(1-MfracTOT97))
gen Blobasic97 = log(Bfracbasic97/(1-Bfracbasic97))
gen Blorec97 = log(Bfracrec97/(1-Bfracrec97))
gen Bloeduc97 = log(Bfraceduc97/(1-Bfraceduc97))
gen Blotravel97 = log(Bfractravel97/(1-Bfractravel97))
gen BloTOT97 = log(BfracTOT97/(1-BfracTOT97))
gen Flobasic97 = log(Ffracbasic97/(1-Ffracbasic97))
gen Florec97 = log(Ffracrec97/(1-Ffracrec97))
gen Floeduc97 = log(Ffraceduc97/(1-Ffraceduc97))
gen Flotravel97 = log(Ffractravel97/(1-Ffractravel97))
gen FloTOT97 = log(FfracTOT97/(1-FfracTOT97))
gen oldmale = ER32000 == 1 & chagem == maxage
gen youngmale = ER32000 == 1 & chagem == minage
sort PCGID_97 PCGPN_97 chagem
by PCGID_97 PCGPN_97: egen sdwday = sd(wday)
by PCGID_97 PCGPN_97: egen sdwend = sd(wend)
by PCGID_97 PCGPN_97: replace oldmale = oldmale[_N]
by PCGID_97 PCGPN_97: replace youngmale = youngmale[1]
by PCGID_97 PCGPN_97: egen sdbw = sd(bw)
by PCGID_97 PCGPN_97: gen diffbw = 1.41421356*sdbw
gen diffbwkg = diffbw/1000
sort PCGID_97 PCGPN_97 chagem
by PCGID_97 PCGPN_97: gen sibval = bw[_n-1]
by PCGID_97 PCGPN_97: gen sibscore = score[_n-1]
by PCGID_97 PCGPN_97: gen sibnmathscore = nmathscore[_n-1]
by PCGID_97 PCGPN_97: gen sibnreadscore = nreadscore[_n-1]
by PCGID_97 PCGPN_97: gen sibavgscore = avgscore[_n-1]
by PCGID_97 PCGPN_97: replace sibavgscore = avgscore[_n+1] if sibavgscore == .
by PCGID_97 PCGPN_97: replace sibscore = score[_n+1] if sibscore == .
by PCGID_97 PCGPN_97: replace sibnmathscore = nmathscore[_n+1] if sibnmathscore == .
by PCGID_97 PCGPN_97: replace sibnreadscore = nreadscore[_n+1] if sibnreadscore == .
sort PCGID_97 PCGPN_97 avgscore02
by PCGID_97 PCGPN_97: gen sibavgscore02 = avgscore02[_n-1]
by PCGID_97 PCGPN_97: replace sibavgscore02 = avgscore02[_n+1] if sibavgscore02 == .
by PCGID_97 PCGPN_97: gen tempN = _N
by PCGID_97 PCGPN_97: gen tempn = _n
by PCGID_97 PCGPN_97: gen odiffbw = sibval - bw
by PCGID_97 PCGPN_97: gen odiffscore = sibscore - score
by PCGID_97 PCGPN_97: gen odiffnmathscore = nmathscore - sibnmathscore
by PCGID_97 PCGPN_97: gen odiffnreadscore = nreadscore - sibnreadscore
by PCGID_97 PCGPN_97: gen odiffavgscore = avgscore - sibavgscore
by PCGID_97 PCGPN_97: gen odiffavgscore02 = avgscore02 - sibavgscore02
by PCGID_97 PCGPN_97: replace odiffbw = odiffbw[_n+1] if odiffbw == .
by PCGID_97 PCGPN_97: replace odiffscore = odiffscore[_n+1] if odiffscore == .
by PCGID_97 PCGPN_97: replace odiffnmathscore = odiffnmathscore[_n+1] if odiffnmathscore == .
by PCGID_97 PCGPN_97: replace odiffnreadscore = odiffnreadscore[_n+1] if odiffnreadscore == .
by PCGID_97 PCGPN_97: replace odiffavgscore = odiffavgscore[_n+1] if odiffavgscore == .
drop sibscore sibval sibnmathscore sibnreadscore sibavgscore temp*
gen odiffbwkg = odiffbw / 1000
gen RTXnmathdiff = odiffnmathscore * RT
gen RTXnreaddiff = odiffnreadscore * RT
gen RTXavgdiff = odiffavgscore * RT
gen RTXodiffbwkg = odiffbwkg * RT
label variable oldmale "Older Boy"
label variable youngmale "Younger Boy"
label variable odiffbwkg "B. Weight Diff."
label variable odiffscore "Score Diff."
label variable odiffnmathscore "Math Score Diff."
label variable odiffnreadscore "Read Score Diff."
label variable odiffavgscore "Avg. Score Diff."
label variable avgscore "Avg. WJ Score"
label variable RTXnmathdiff "RT * (Y-O) Math Score"
label variable RTXnreaddiff "RT * (Y-O) Read Score"
label variable RTXavgdiff "RT * (Y-O) Avg. Score"
label variable RTXodiffbwkg "RT * birthweight difference"
by PCGID_97 PCGPN_97: egen sttested = total(tested) 
by PCGID_97 PCGPN_97: egen stmathtested = total(mathtested) 
by PCGID_97 PCGPN_97: egen streadtested = total(readtested) 
gen bothtested = sttested == 2 
gen bothmathtested = stmathtested == 2 
gen bothreadtested = streadtested == 2 
by PCGID_97 PCGPN_97: egen mixedsex = sd(ER32000) 
gen males = ER32000 == 1 & mixedsex == 0
gen females = ER32000 == 2 & mixedsex == 0
rename ER32000 sex
gen male = sex == 1
gen maxageXmale = maxage * male
gen minageXmale = minage * male
gen inschool = Q1G26 == 1 | Q1G2A == 1
by PCGID_97 PCGPN_97: egen mschool = sd(inschool) 
gen mixedschool = mschool > 0
gen noschool = mixedschool == 0 & inschool == 0
gen bothschool = mixedschool == 0 & inschool == 1
gen hsdip = mschooling97 >= 12 & mschooling < 97
gen cdip = mschooling97 >= 16 & mschooling < 97
gen fhsdip = fschooling97 >= 12 & fschooling < 97
gen fcdip = fschooling97 >= 16 & fschooling < 97
gen RTXcdip = RT * cdip
gen RTXhsdip = RT * hsdip
gen sdbw2 = sdbw^2
gen sdbw3 = sdbw^3
gen maleXchage = male * chage
gen maleXchagem = male * chagem
gen RTXyoung = RT * (ageyoungest < 5)
gen RTXyoungest = RT * ageyoungest
gen RTXym = RT * youngmale
gen RTXom = RT * oldmale
gen RTXyearsdiff = RT * yearsdiff
gen RTXchage = RT * chage
gen RTXchagem = RT * chagem
gen RTXminageyears = RT * minageyears
gen RTXmaxageyears = RT * maxageyears
gen RTXmales = males * RT
gen RTXmale = male * RT
gen RTXfemales = females * RT
gen RTXmixedsex = mixedsex * RT
egen family = group(PCGID_97 PCGPN_97)
gen sub2 = 2 * SUBSELWT
gen agegroup02 = 1 if Q21IWAGE < 6
replace agegroup02 = 2 if Q21IWAGE >= 6 & Q21IWAGE <= 9 
replace agegroup02 = 3 if Q21IWAGE > 9
local homerestriction "dadhead & mschooling97 < 70 & fschooling97 < 70 & selfBTOT97 != ."
local homerestriction " mschooling97 < 70 & fschooling97 < 70 "
bysort Q1B17: egen home97mean = mean(HOME1) if `homerestriction'
/*bysort chage: egen cog97mean = mean(COGSCALE) if `homerestriction'*/
/*bysort chage: egen em197mean = mean(EMSCALE1) if `homerestriction'*/
/*bysort chage: egen em297mean = mean(EMSCALE2) if `homerestriction'*/
by Q1B17: egen home97sd = sd(HOME1) if `homerestriction'
/*by chage: egen cog97sd = sd(COGSCALE) if `homerestriction'*/
/*by chage: egen em197sd = sd(EMSCALE1) if `homerestriction'*/
/*by chage: egen em297sd = sd(EMSCALE2) if `homerestriction'*/
gen home97 = (HOME1-home97mean)/home97sd
/*gen cog97 = (COGSCALE-cog97mean)/cog97sd*/
/*gen em197 = (EMSCALE1-em197mean)/em197sd*/
/*gen em297 = (EMSCALE2-em297mean)/em297sd*/
/*egen home97 = std(HOME1) if `homerestriction'*/
egen cog97 = std(COGSCALE) if `homerestriction'
gen home02temp = HT2_02 if HT3_02 == 0 & HT1_02 == 0 & `homerestriction'
gen cog02temp = CG2_02 if CG3_02 == 0 & CG1_02 == 0 & `homerestriction'
replace home02temp = HT3_02 if HT2_02 == 0 & HT1_02 == 0 & `homerestriction'
replace cog02temp = CG3_02 if CG2_02 == 0 & CG1_02 == 0 & `homerestriction'
replace home02temp = HT1_02 if HT2_02 == 0 & HT3_02 == 0 & `homerestriction'
replace cog02temp = CG1_02 if CG2_02 == 0 & CG3_02 == 0 & `homerestriction'
bysort agegroup02: egen home02mean = mean(home02temp) if `homerestriction'
by agegroup02: egen home02sd = sd(home02temp) if `homerestriction'
gen home02 = (home02temp -home02mean)/home02sd
by agegroup02: egen cog02mean = mean(cog02temp) if `homerestriction'
by agegroup02: egen cog02sd = sd(cog02temp) if `homerestriction'
gen cog02 = (cog02temp -cog02mean)/cog02sd
/*bysort chage: egen home02mean = mean(home02temp) if `homerestriction'*/
/*bysort chage: egen cog02mean = mean(cog02temp) if `homerestriction'*/
/*by chage: egen home02sd = sd(home02temp) if `homerestriction'*/
/*by chage: egen cog02sd = sd(cog02temp) if `homerestriction'*/
/*egen home02 = std(home02temp) if `homerestriction'*/
/*egen cog02 = std(cog02temp) if `homerestriction'*/
/*gen home02 = (home02temp -home02mean)/home02sd*/
/*gen cog02 = (cog02temp -cog02mean)/cog02sd*/
/*drop home02temp home02sd home02mean home97mean home97sd cog02temp cog02sd cog02mean cog97mean cog97sd */
drop home02temp cog02temp 
gen selfhome97 = home97 - home02
gen selfcog97 = cog97 - cog02
/*sort PCGID_97 PCGPN_97 chagem*/
/*by PCGID_97 PCGPN_97: gen sibhome97 = home97[_n-1]*/
/*by PCGID_97 PCGPN_97: gen sibhome02 = home02[_n-1]*/
/*by PCGID_97 PCGPN_97: gen odiffhome97 = home97 - sibhome97*/
/*by PCGID_97 PCGPN_97: gen odiffhome02 = home02 - sibhome02*/

    sort PCGID_97 PCGPN_97 chagem
    by PCGID_97 PCGPN_97: gen sibval = home97[_n-1]
    by PCGID_97 PCGPN_97: gen odiffhome97 =  sibval - home97
    by PCGID_97 PCGPN_97: replace odiffhome97 = odiffhome97[_n+1] if odiffhome97 == .
    drop sibval

gen agegroup97 = Q1B17
gen youngagegroup97temp = agegroup97 if kidnum == 1 & numkids == 2
gen oldagegroup97temp = agegroup97 if kidnum == 2 & numkids == 2
by PCGID_97 PCGPN_97: egen oldagegroup97 = sum(oldagegroup97temp)
by PCGID_97 PCGPN_97: egen youngagegroup97 = sum(youngagegroup97temp)
replace oldagegroup97 = . if oldagegroup97 == 0
replace youngagegroup97 = . if youngagegroup97 == 0
/*by PCGID_97 PCGPN_97: gen sibvalage = youngagegroup97[_n-1]*/
/*by PCGID_97 PCGPN_97: gen oldsibvalage = oldagegroup97[_n+1]*/
/*replace youngagegroup97 = sibvalage if kidnum == 2 & youngagegroup97 == .*/
/*replace oldagegroup97 = oldsibvalage if kidnum == 1 & oldagegroup97 == .*/
/*drop sibvalage oldsibvalage*/
/*count if sdrace > 0*/
gen nonwhite = race != 1
gen hispanic = race == 3
gen asian = race == 4
gen sibs = SBLNUM97 if SBLNUM97 < 15
replace sibs = SBLNUM01 if SBLNUM01 > SBLNUM97 & SBLNUM01 != . & SBLNUM01 < 15
replace sibs = SBLNUM03 if SBLNUM03 > SBLNUM97 & SBLNUM03 != . & SBLNUM03 < 15
gen famsize = sibs + 1
egen famsize_count = rownonmiss(kid*sex)
egen fracboystemp = rowmean(kid*sex)
gen fracboys = 2 - fracboystemp
drop fracboystemp
egen age_oldest = rowmax(kid*agem)
egen age_youngest = rowmin(kid*agem)
gen agerangem = age_oldest-ageyoungest

/*replace famsize = famsize_count*/
/*I think for these purposes, taking biological siblings in 97 is not a bad*/
/*plan, since these are the guys likely to be sharing time with siblings.  We*/
/*could do a little better by finding step siblings, but I think in the*/
/*in-tact households that I'm finding, there are likely to be fairly few*/
/*step-siblings.*/

/*
gen lsdMTOT = ln(sdMTOT)
gen lsdFTOT = ln(sdFTOT)
gen lsdBTOT = ln(sdBTOT)
foreach var in `extralist97'{
  gen lsdM`var' = ln(sdM`var')
  gen ldiffM`var' = ln(diffM`var')
  gen lsdF`var' = ln(sdF`var')
  gen ldiffF`var' = ln(diffF`var')
  gen lsdB`var' = ln(sdB`var')
  gen ldiffB`var' = ln(diffB`var')
}
gen lsdMeduc = ln(sdMeduc)
gen lsdMrec = ln(sdMrec)
gen lsdMtravel = ln(sdMtravel)
gen lsdMnotravel = ln(sdMnotravel)
gen lsdMbasic = ln(sdMbasic)
gen lsdMSUM = ln(sdMSUM)
gen lsdFeduc = ln(sdFeduc)
gen lsdFrec = ln(sdFrec)
gen lsdFtravel = ln(sdFtravel)
gen lsdFnotravel = ln(sdFnotravel)
gen lsdFbasic = ln(sdFbasic)
gen lsdFSUM = ln(sdFSUM)
gen lsdBeduc = ln(sdBeduc)
gen lsdBrec = ln(sdBrec)
gen lsdBtravel = ln(sdBtravel)
gen lsdBnotravel = ln(sdBnotravel)
gen lsdBbasic = ln(sdBbasic)
gen lsdBSUM = ln(sdBSUM)
*/

gen lRT = ln(RT)
gen ORT = 0 if RT > .06 & RT < .07
replace ORT = 1 if RT > .19 & RT < .2
replace ORT = 2 if RT > .3 & RT < .4
replace ORT = 3 if RT > .5 & RT < 1
replace ORT = 4 if RT > 1 & RT < 2
replace ORT = 5 if RT > 8
gen ageatbirth = parentage - chage
gen teenmom = ageatbirth <=20
gen blackXurban = black * urban
gen blackXmarriedrateall = black * marriedrateall
gen blackXmarriedraterace = black * marriedraterace
**********************************************************************

**********************************************************************
  * Labelling
foreach person of local personlist{
  if "`person'" == "M"{
    local pstring "Mom"
  }
  if "`person'" == "F"{
    local pstring "Dad"
  }
  if "`person'" == "B"{
    local pstring "Both"
  }
  if "`person'" == "N"{
    local pstring "One"
  }
  foreach category of local categorylist{
  foreach year of local yearlist{
    if "`category'" == "TOT"{
      local cstring "All"
    }
    if "`category'" == "SUM"{
      local cstring "Sum of Cat's"
    }
    if "`category'" == "educ"{
      local cstring "Educ"
    }
    if "`category'" == "basic"{
      local cstring "Care"
    }
    if "`category'" == "rec"{
      local cstring "Rec"
    }
    /*if "`category'" == "rec1"{*/
      /*local cstring "Rec Org."*/
    /*}*/
    /*if "`category'" == "rec2"{*/
      /*local cstring "Rec Ent."*/
    /*}*/
    /*if "`category'" == "rec3"{*/
      /*local cstring "Active Rec"*/
    /*}*/
    /*if "`category'" == "rec4"{*/
      /*local cstring "Passive Rec"*/
    /*}*/
    if "`category'" == "travel"{
      local cstring "Travel"
    }
    if "`category'" == "notravel"{
      local cstring "Non-Travel"
    }
    
    label variable `person'`category'`year' "`pstring' `cstring' Time (hr)"
    label variable sd`person'`category'`year' "`pstring' `cstring' SD"
    label variable diff`person'`category'`year' "`pstring' `cstring' Diff"
    label variable ddiff`person'`category'`year' "`pstring' `cstring' Disc."
    label variable odiff`person'`category'`year' "`pstring' `cstring' (Y-O)"
    label variable old`person'`category'`year' "`pstring' `cstring' w/ Old"
    label variable dold`person'`category'`year' "`pstring' `cstring' w/ Old (D)"
    label variable young`person'`category'`year' "`pstring' `cstring' w/ Young"
    label variable dyoung`person'`category'`year' "`pstring' `cstring' w/ Young (D)"
    label variable sodiff`person'`category'`year' "`pstring' `cstring' (S-D)"
    label variable mean`person'`category'`year' "`pstring' `cstring' Avg. Time"
    label variable mean`person'`category'`year'2 "`pstring' `cstring' Avg. Time$^2$"
    label variable mean`person'`category'`year'3 "`pstring' `cstring' Avg. Time$^3$"
    label variable lsd`person'`category'`year' "log(`pstring' `cstring' SD)"
    label variable ldiff`person'`category'`year' "log(`pstring' `cstring' Diff. (hr))"
    if "`year'" == "97"{
        label variable self`person'`category'`year' "`pstring' `cstring' 5 yr. $\Delta$"
    }
  }
}
}

label variable dadeverdrinks "Dad Drinks"
label variable dadsmokes "Dad Smokes"
label variable dadeversmoked "Dad Ever Smoked"
label variable momeverdrinks "Mom Drinks"
label variable momsmokes "Mom Smokes"
label variable momeversmoked "Mom Ever Smoked"
label variable RTXom "RT * Old Boy"
label variable RTXym "RT * Young Boy"
label variable RTXyoung "RT * Youngest $< 5$"
label variable RTXyoungest "RT * Age Youngest"
label variable noschool "Neither in School"
label variable bothschool "Both in School"
label variable income "Total Income / \\$100k"
label variable wifehrs "Wife Hrs/Week/10"
label variable wifeinlf "Wife in LF"
label variable wifeoutlf "Wife Home"
label variable headinlf "Head in LF"
label variable RT "Risk Tolerance"
label variable RT1 "Risk Tol. (Alternative)"
label variable sdbw "Birthweight SD"
label variable bw "Birthweight (gr.)"
label variable hsdip "Mom HS Grad"
label variable cdip "Mom Col. Grad"
label variable fhsdip "Dad HS Grad"
label variable fcdip "Dad Col. Grad"
label variable famsize "Biological Kids in HH"
label variable married "Married"
label variable inschool "In School"
label variable chage "Age of Child"
label variable parentage "Age of Mother"
label variable parentage2 "Age of Mother Sq."
label variable parentage3 "Age of Mother Cu."
label variable yearsdiff "Age Difference (yr)"
label variable agediff "Age Diff. (Months)"
label variable agediff2 "Age Diff. Sq."
label variable agediff3 "Age Diff. Cu."
label variable nonwhite "Non-White"
label variable ageyoungest "Age(Youngest) Kid"
label variable males "Two Males"
label variable male "Male"
label variable females "Two Females"
label variable mschooling97 "Mom Years of Schooling"
label variable fschooling97 "Dad Years of Schooling"
label variable diffbwkg "Abs(B-Weight Diff) (kg)"
label variable chagem "Child Age (Months)"
label variable chagem2 "Child Age$^2$/1000"
label variable selfBTOTavg "Average Time with Child"
label variable black "Black"
label variable blackXmarriedraterace "Black * % Married"
label variable marriedraterace "% Married"
label variable Ffracbasic97 "Public/All Time"
label variable Ffracrec97 "Public/All Time"
label variable Ffraceduc97 "Public/All Time"
label variable urban "Urban Area"

**********************************************************************
  * Dropping
/* drop if SBLNUM97 > 10 */
/*drop if famsize > 5 | famsize == 1*/
/*drop if yearsdiff == 0*/
gen famdrop = ( famsize > 5 | famsize == 1 )
replace famdrop = 1 if yearsdiff == 0

**********************************************************************
**********************************************************************
gen psid = 1
gen cps = 0
gen elig = 1
save twoSibsPared.dta, replace
bysort PCGID_97 PCGPN_97:  drop if _n > 1
**********************************************************************
sum odiff*  
save pcgPared.dta, replace
/*do makecps.do*/
/*Make the cps sample.  This takes longer so unless doing something to the CPS*/
/*don't worry about this.*/
