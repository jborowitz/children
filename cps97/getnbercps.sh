#!/bin/bash
# This file downloads the cps from the NBER website (slow for sure, but I know
# it matches what it's supposed to in the .do files).
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/jan96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/feb96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/mar96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/apr96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/may96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/jun96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/jul96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/aug96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/sep96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/oct96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/nov96pub.cps.gz
wget ftp://www.bls.census.gov/pub/cps/basic/199509-199712/dec96pub.cps.gz
#Downlaod the data from the census burea
gunzip jan96pub.cps.gz
gunzip feb96pub.cps.gz
gunzip mar96pub.cps.gz
gunzip apr96pub.cps.gz
gunzip may96pub.cps.gz
gunzip jun96pub.cps.gz
gunzip jul96pub.cps.gz
gunzip aug96pub.cps.gz
gunzip sep96pub.cps.gz
gunzip oct96pub.cps.gz
gunzip nov96pub.cps.gz
gunzip dec96pub.cps.gz
#unzip the data
stata-mp -b do cpsbsep95.do
#Load the data into stata.  This file generates cps96.dta and cps96-sample.dta
