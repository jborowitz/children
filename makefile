test-merged.dta : prepare_psid.sh splitandmerge.do psid.zip
	./prepare_psid.sh
pcgPared.dta : makedata.do test-merged.dta tdag97.dta tdag02.dta tdag07.dta married.dta
	stata-mp -b do makedata.do
twoSibsPared.dta : pcgPared.dta
FfracTOT97ols.txt : jan11.do pcgPared.dta twoSibsPared.dta
	stata-mp -b do jan11.do
tdag97.dta : td97.dta make-1997-td.do
	stata-mp -b do make-1997-td.do
tdag02.dta : td02.dta make-2002-td.do
	stata-mp -b do make-2002-td.do
tdag07.dta : td07.dta make-2007-td.do
	stata-mp -b do make-2007-td.do
married.dta : make-cells.do cps97/cps96-sample.dta
	stata-mp -b do make-cells.do
cps97/cps96-sample.dta : cps97/cps96.dta sample.do
	stata-mp -b do sample.do	

