clear
clear matrix
set mem 2000m
use cps97/cps96.dta
sample 100000, count
save cps97/cps96-sample.dta, replace
