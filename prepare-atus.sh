#!/bin/bash
#This file depends on the raw downloads of the 03-09 atus data sets from the bls
#webpage, at http://www.bls.gov/tus/datafiles_0309.htm.  It creates the .dta
#files associated with these, and leaves them in data/

#Jeffrey Borowitz
#University of Maryland
#2011-03-07

cd data
for i in *.zip
do 
   file=`basename $i .zip`
   unzip -n $i
   unzip -o $i $file.do 
   sed -i  's/c:\\//g' $file.do
   rm -f $file-new.do
   echo "clear" >> $file-new.do
   echo "set mem 300m" >> $file-new.do
   cat $file.do >> $file-new.do
   echo "save $file.dta, replace;" >> $file-new.do
   #sed -i  'li clear' $file.do
   #sed -i  'li set mem 300m' $file.do
   stata-mp -b do $file-new.do
   #mv $file.dta ..
   done

cd ..
