test-merged.dta : prepare_psid.sh splitandmerge.do psid.zip
	./prepare_psid.sh
pcgPared.dta: makedata.do test-merged.dta tdag97.dta tdag02.dta tdag07.dta married.dta
