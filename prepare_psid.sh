#/bin/bash
# usage: ./prepare_psid.sh <downloaded CDS .zip file> <extract name>
#
# This script takes one download from the PSID website: the extract
# should have both the CDS information for kids and the other earlier
# family and individual info for each caregiver of interest.  The
# script separates the data set into two parts: CDS kids and everyone
# else.  Then, it merges panel info about primary care givers with CDS
# kids.  The result is a data set where each record represents a
# single kid, and contains information about his primary caregiver's
# work history.  However, I'd like to further this so that each single
# record has info for two kids, since ultimately I'm just interested
# in differences between kids.
#
# Jeffrey Borowitz
# 2010-05-27
#
# Note: set "mergeyear" to be one of 97 02, or 07.  This specifies
# which parent and other caregivers to use.  It is also possible to
# set 'PCG' to 'OCG' to look at info about the other caregiver
extractname='test'
zipfile='psid.zip'
mkdir tmp
unzip $zipfile -d tmp

cd tmp
cdsvar=`basename *pdf .pdf`
# get the "J23143" part of the file name
mapcdsvar=`echo $cdsvar | sed 's/J/M/'`
# replace the J with an M for the map file
extractnum=`echo $cdsvar | sed 's/J//'`
# get the "23413" part of the file name
cd ..

cd tmp
dir=`pwd`
sed -i 's/\[path\]\\//' ${cdsvar}.do
sed -i 's/\[path\]\\//' ${mapcdsvar}.do
cd ..
#Fix the .do file so that it is actually runable (remove the "[path]" marker)

sed -i "s/global extractnum [0-9]*/global extractnum $extractnum/" splitandmerge.do
# Change the extract number in splitandmerge.do so that it now looks for the new data.
stata-mp -b do splitandmerge.do
# run the .do file

#for t in '.do' '.pdf' ; do
#    mv ${var}${t} ${2}${t}
#done
# This code will rename additional components of the .do file
cd tmp
mv ${cdsvar}.pdf ${extractname}.pdf

cd ..
mv tmp/*.pdf .
mv tmp/$extractname-merged.dta .
mv tmp/$extractname-psid.dta .
mv tmp/$extractname-cds.dta .
mv tmp/$extractname-parents.dta .
rm -r tmp
# Take the created .dta file and the documentation out of the
# temporary folder and clean up.
################################################################################
