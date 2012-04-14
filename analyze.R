#library('estout','zoo','sandwich')
library('zoo')
library('sandwich',lib='/home/jborowitz/R/x86_64-redhat-linux-gnu-library')
library('abind')
library('xtable')
library('stringr')
library('foreign')
#source('estout/R/eststo.R')
#source('estout/R/esttab.R')
source('../estout2/eststo2.R')
source('../estout2/esttab2.R')
source('../estout2/estclear.R')
#source('pdb.R')
sink('/dev/null')
load('twoSibsPared.RData')
sink()
strin <- 'subset=dadhead & mschooling97 < 97 & fschooling97 < 97 & !is.na(selfBTOT97) & SBLNUM03 != 96 & SBLNUM03 != 5 & !is.na(nreadscore) & !is.na(nmathscore) & !is.na(nappliedscore02)'
#r <- subset(data,as.expression(strin))
datasubset <- subset(data,subset=dadhead & mschooling97 < 97 & fschooling97 < 97 &
            !is.na(selfBTOT97) & SBLNUM03 != 96 & SBLNUM03 != 5 & !is.na(nreadscore) & !is.na(nmathscore) & !is.na(nappliedscore02))
#print(NROW(datasubset))
#rm(d)
#attach(datasubset)
#f <- matrix(list(),2,3,dimnames = list(c('row1','row2'),c('c1','c2','c3')))
#f
#summary(d$RT)

myvcov<-function(x){
    m <- meat(x)
    m <- m[!is.na(coef(x)),!is.na(coef(x))]
    b <- bread(x)
    n <- NROW(estfun(x))
    return((1/n * (b %*% m %*% b) ))
    vcovHC(x,type='HC0')

}
datasubset$famsize <- as.factor(datasubset$famsize)
datasubset$ageyoungest <- as.factor(datasubset$ageyoungest)
datasubset$SBLNUM03 <- as.factor(datasubset$SBLNUM03)
datasubset$ageyoungest02 <- as.factor(datasubset$ageyoungest02)
datasubset$BTOT0 <- datasubset$BTOT02 + datasubset$BTOT07
#contrasts(SBLNUM03)<-'contr.sum'
#f1  <-  formula(odiffBTOT97  ~  RT  +  income  +  famsize  +  ageyoungest)
ss  <-   'mschooling97   <   90   &   fschooling97   <   90   &   dadhead)'
interactions <- 'famsize + ageyoungest + SBLNUM03 +  ageyoungest02'

int <- c(rep('',2),rep(interactions,4))
Xs <- c('BTOT97' 
        ,'BTOT97 + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool' 
        ,'BTOT97' 
        ,'BTOT97 + chagem + chagem2 + nonwhite + male + parentage + income + inschool' 
        ,'BTOT97 + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
        ,'BTOT97 + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool')

yread <- 'nreadscore02 '
ymath <- 'nappliedscore02 '
#y <- 'avgscore02 ~'
scores <- c('nmathscore + nreadscore')
#scores <- c('nreadscore')
xmath <- c('nmathscore')
xread <- c('nreadscore')
#scores <- c('avgscore97 ')
wholeX <- paste(int,Xs,sep=' + ')
wholeX <- gsub('\\+  \\+','\\+',wholeX)
wholeX <- gsub('\\+ $','',wholeX)
wholeX <- gsub('^ \\+ ','',wholeX)
#print(wholeX)
#firstformulas <- paste(paste(y,scores,sep=' '),paste(int,Xs,sep=' + '),sep = ' + ')
#firstformulas <- gsub('\\+  \\+','\\+',firstformulas)
#firstformulas <- gsub('\\+ $','',firstformulas)
outputlist <- c('BTOT02','Beduc02','Btravel02','Brec02','Bbasic02')
#outputvar <- 'Btravel02'
#firstformulas <- c(
                   #'nappliedscore02 ~ nmathscore + nreadscore'
#,'nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
#,paste('nappliedscore02 ~ nmathscore + nreadscore',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool',interactions,sep=" + "))

for(outputvar in outputlist){
estclear()
for(f in wholeX){
    math.firststage.formula <- paste(paste(ymath,xmath,sep=" ~ "),f,sep=" + ")
    math.secondstage.formula <- paste(outputvar,'~',paste('mathresids',xmath,f,sep=" + "))
    mathfirststage <- lm(math.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    datasubset$mathresids <- resid(mathfirststage)
    math.secondstage <- lm(math.secondstage.formula, weight=CH97PRWT, data=datasubset)
    sd.effect <- sd(resid(mathfirststage),na.rm=TRUE)*coef(math.secondstage)[['mathresids']]
    eststo2(math.secondstage,list('1 S.D. Effect'=sd.effect))
    datasubset$mathresids <- NULL
    print('_________________________________________')
    print(math.firststage.formula)
    print(math.secondstage.formula)
}
ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='famsize.*')
columns <- rep(outputvar,6)
vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)',
'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad','fcdip'='Dad College','fhsdip'='Dad HS','nreadscore'='1997 Read','nmathscore'='1997 Math','resids'='Unexpected Change','BTOT97'='Time in 1997','mathresids'='Math Resid.','readresids'='Read Resid.')
drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
keep.list  <-  list('mathresids','readresids','nappliedscore02','nreadscore02','nmathscore','nreadscore','BTOT97','chagem',  'chagem2',  'cdip',  'hsdip', 'income')
outfile <- paste(outputvar,'math',sep='')
o<-esttab2(filename=paste(outfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
o<-esttab2(filename=paste(outfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
system(paste('cat ',outfile,'.txt',sep=''))

estclear()
for(f in wholeX){
    read.firststage.formula <- paste(paste(yread,xread,sep=" ~ "),f,sep=" + ")
    readfirststage <- lm(read.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    datasubset$readresids <- resid(readfirststage)
    read.secondstage.formula <- paste(outputvar,'~',paste('readresids',xread,f,sep=" + "))
    read.secondstage <- lm(read.secondstage.formula, weight=CH97PRWT, data=datasubset)
    sd.effect <- sd(resid(readfirststage),na.rm=TRUE)*coef(read.secondstage)[['readresids']]
    eststo2(read.secondstage,list('1 S.D. Effect'=sd.effect))
    datasubset$readresids <- NULL
    print('_________________________________________')
    print(read.firststage.formula)
    print(read.secondstage.formula)
}
ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='famsize.*')
columns <- rep(outputvar,6)
vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)',
'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad','fcdip'='Dad College','fhsdip'='Dad HS','nreadscore'='1997 Read','nmathscore'='1997 Math','resids'='Unexpected Change','BTOT97'='Time in 1997','mathresids'='Math Resid.','readresids'='Read Resid.')
drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
keep.list  <-  list('mathresids','readresids','nappliedscore02','nreadscore02','nmathscore','nreadscore','BTOT97','chagem',  'chagem2',  'cdip',  'hsdip', 'income')
outfile <- paste(outputvar,'read',sep='')
o<-esttab2(filename=paste(outfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
o<-esttab2(filename=paste(outfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
system(paste('cat ',outfile,'.txt',sep=''))
}

#################################################################################
## Begin Time-Type
#################################################################################
#f <- 'RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem +
#chagem2 + nonwhite + male + parentage + inschool + cdip + hsdip + fcdip +
#fhsdip'
#categories <- c('TOT97','basic97','educ97','rec97','travel97')
#estclear()
#for(b in categories){
    #fstring <- paste('B',b,' ~ ',f,sep='')
    #res1 <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    #sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    #iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    #eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
#}
#columns <- c('All','Basic','Educ.','Rec.','Travel')
#o<-esttab2(filename='temp.txt',indicate=ind.list, keep=keep.list,
           #col.width=15, var.rename=vrn, se.func=myvcov,
           #col.headers=columns)
#system('cat temp.txt')
#################################################################################
## End Time-Type
#################################################################################

#################################################################################
## Begin Old-Young
#################################################################################
#Xs <- c(
#'RT + famsize + ageyoungest + SBLNUM03 +  ageyoungest02'
#,'RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem + chagem2 + nonwhite + male + parentage + inschool + cdip + hsdip + fcdip + fhsdip'
#)
#outcomes <- c('selfBTOT97','BTOT97','BTOT02')
#estclear()
#for(i in Xs){
    #for(j in outcomes){
    #fstring <- paste(j,' ~ ',i,sep='')
    #res <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    #sd.effect1 <- sd(RT)*coef(res)[['RT']]
    #iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res)[['RT']]
    #eststo2(res, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
    #}
#}
#columns <- c('yi - oi','yi','oi','yi - oi','yi','oi')
#o<-esttab2(filename='temp.txt',indicate=ind.list, keep=keep.list,
           #col.width=19, var.rename=vrn, se.func=myvcov,
           #col.headers=columns)
#system('cat temp.txt')
#################################################################################
## End Old-Young
#################################################################################

#################################################################################
## Start Housewives
#################################################################################
## Note: uses the Xs from Old-Young
#estclear()
#for(i in Xs){
    #fstring <- paste('selfBTOT97 ~ ',i,sep='')
    #res1 <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    #res2 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=wifeinlf == 0)
    #res3 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=wifeinlf == 1)
    ##print(c(dim(bread(res1)),dim(meat(res1))))
    ##print(c(dim(bread(res2)),dim(meat(res2))))
    ##print(c(dim(bread(res3)),dim(meat(res3))))
    ##print(setdiff(dimnames(meat(res1)),dimnames(bread(res1))))
    ##print(setdiff(dimnames(meat(res2)),dimnames(bread(res2))))
    ##print(setdiff(dimnames(meat(res3)),dimnames(bread(res3))))
    #sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    #sd.effect2 <- sd(RT)*coef(res2)[['RT']]
    #sd.effect3 <- sd(RT)*coef(res3)[['RT']]
    #iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    #iqr.effect2 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res2)[['RT']]
    #iqr.effect3 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res3)[['RT']]
    #eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
    #eststo2(res2, list('1 S.D. Effect'=sd.effect2, 'I.Q.R.  Effect)'=iqr.effect2))
    #eststo2(res3, list('1 S.D. Effect'=sd.effect3, 'I.Q.R.  Effect)'=iqr.effect3))
#}
#columns <- c('All','Not in LF','In LF','All','Not in LF','In LF')
#o<-esttab2(filename='temp.txt',indicate=ind.list, keep=keep.list,
           #col.width=19, var.rename=vrn, se.func=myvcov,
           #col.headers=columns)
#system('cat temp.txt')
#################################################################################
## End Housewives
#################################################################################

#################################################################################
## Start Age Level
#################################################################################
#estclear()
#stagecut <- 6
#for(i in Xs){
    #fstring <- paste('selfBTOT97 ~ ',i,sep='')
    #res1 <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    #res2 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=chage <= stagecut)
    #res3 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=chage > stagecut)
    ##print(c(dim(bread(res1)),dim(meat(res1))))
    ##print(c(dim(bread(res2)),dim(meat(res2))))
    ##print(c(dim(bread(res3)),dim(meat(res3))))
    #sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    #sd.effect2 <- sd(RT)*coef(res2)[['RT']]
    #sd.effect3 <- sd(RT)*coef(res3)[['RT']]
    #iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    #iqr.effect2 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res2)[['RT']]
    #iqr.effect3 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res3)[['RT']]
    #eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
    #eststo2(res2, list('1 S.D. Effect'=sd.effect2, 'I.Q.R.  Effect)'=iqr.effect2))
    #eststo2(res3, list('1 S.D. Effect'=sd.effect3, 'I.Q.R.  Effect)'=iqr.effect3))
#}
#columns <- c('All','Not in LF','In LF','All','Not in LF','In LF')
#o<-esttab2(filename='temp.txt',indicate=ind.list, keep=keep.list,
           #col.width=19, var.rename=vrn, se.func=myvcov,
           #col.headers=columns)
#system('cat temp.txt')
