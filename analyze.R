#library('estout','zoo','sandwich')
library('zoo')
library('sandwich')
library('abind')
library('xtable')
library('stringr')
library('foreign')
#source('estout/R/eststo.R')
#source('estout/R/esttab.R')
source('/home/jborowitz/R-estout2/eststo2.R')
source('/home/jborowitz/R-estout2/esttab2.R')
source('/home/jborowitz/R-estout2/estclear.R')
#source('pdb.R')
#load('/home/jborowitz/Dropbox/R/pcgPared.RData')
sink('/dev/null')
load('/home/jborowitz/R/twoSibsPared.RData')
sink()
#data<-read.dta('/home/jborowitz/R/twoSibsPared.dta')
q <- subset(data,subset=dadhead & mschooling97 < 97 & fschooling97 < 97 &
            !is.na(selfBTOT97) & SBLNUM03 != 96 & SBLNUM03 != 5)
#rm(d)
#attach(q)
#f <- matrix(list(),2,3,dimnames = list(c('row1','row2'),c('c1','c2','c3')))
#f
#summary(d$RT)

myvcov<-function(x){
    m <- meat(x)
    m <- m[!is.na(coef(x)),!is.na(coef(x))]
    b <- bread(x)
    n <- NROW(estfun(x))
    return((1/n * (b %*% m %*% b) ))
    #vcov(x)
    #X <- estfun(x)[,!is.na(coef(x))]
    #n <- length(X[,1])
    vcovHC(x,type='HC0')

}
q$famsize <- as.factor(q$famsize)
q$ageyoungest <- as.factor(q$ageyoungest)
q$SBLNUM03 <- as.factor(q$SBLNUM03)
q$ageyoungest02 <- as.factor(q$ageyoungest02)
#contrasts(SBLNUM03)<-'contr.sum'
#f1  <-  formula(odiffBTOT97  ~  RT  +  income  +  famsize  +  ageyoungest)
ss  <-   'mschooling97   <   90   &   fschooling97   <   90   &   dadhead)'
formulas <- c(
'selfBTOT97 ~ RT'
,'selfBTOT97 ~ RT + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
,'selfBTOT97 ~ RT + famsize + ageyoungest + SBLNUM03 +  ageyoungest02'
,'selfBTOT97 ~ RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem + chagem2 + nonwhite + male + parentage + inschool'
,'selfBTOT97 ~ RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem + chagem2 + nonwhite + male + parentage + inschool + cdip + hsdip + fcdip + fhsdip + income'
,'selfBTOT97 ~ RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem + chagem2 + nonwhite + male + parentage + inschool + cdip + hsdip + fcdip + fhsdip'
)

estclear()
attach(q)
for(f in formulas){
    res1 <- lm(as.formula(f), weight=CH97PRWT, data=q)
    sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
}
ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='famsize.*')
vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)',
'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad')
drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
keep.list  <-  list('RT','chagem',  'chagem2',  'cdip',  'hsdip')
#columns <- c('Model 1','OLS','IV')
#o<-esttab2()
o<-esttab2(filename='Bols.txt',indicate=ind.list, keep=keep.list,
          col.width=15, var.rename=vrn, se.func=myvcov)
o<-esttab2(filename='Bols.tex',indicate=ind.list, keep=keep.list,
          col.width=15, var.rename=vrn, se.func=myvcov)
system('cat Bols.txt')

################################################################################
# Begin Time-Type
################################################################################
f <- 'RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem +
chagem2 + nonwhite + male + parentage + inschool + cdip + hsdip + fcdip +
fhsdip'
categories <- c('TOT97','basic97','educ97','rec97','travel97')
estclear()
for(b in categories){
    fstring <- paste('B',b,' ~ ',f,sep='')
    res1 <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
}
columns <- c('All','Basic','Educ.','Rec.','Travel')
o<-esttab2(filename='Btypeols.txt',indicate=ind.list, keep=keep.list,
           col.width=15, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
o<-esttab2(filename='Btypeols.tex',indicate=ind.list, keep=keep.list,
           col.width=15, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
system('cat Btypeols.txt')
################################################################################
# End Time-Type
################################################################################

################################################################################
# Begin Old-Young
################################################################################
Xs <- c(
'RT + famsize + ageyoungest + SBLNUM03 +  ageyoungest02'
,'RT + famsize + ageyoungest + SBLNUM03 + ageyoungest02 + chagem + chagem2 + nonwhite + male + parentage + inschool + cdip + hsdip + fcdip + fhsdip'
)
outcomes <- c('selfBTOT97','BTOT97','BTOT02')
estclear()
for(i in Xs){
    for(j in outcomes){
    fstring <- paste(j,' ~ ',i,sep='')
    res <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    sd.effect1 <- sd(RT)*coef(res)[['RT']]
    iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res)[['RT']]
    eststo2(res, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
    }
}
columns <- c('yi - oi','yi','oi','yi - oi','yi','oi')
o<-esttab2(filename='Bage-breakdown.txt',indicate=ind.list, keep=keep.list,
           col.width=19, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
o<-esttab2(filename='Bage-breakdown.tex',indicate=ind.list, keep=keep.list,
           col.width=19, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
system('cat Bage-breakdown.txt')
################################################################################
# End Old-Young
################################################################################

################################################################################
# Start Housewives
################################################################################
# Note: uses the Xs from Old-Young
estclear()
for(i in Xs){
    fstring <- paste('selfBTOT97 ~ ',i,sep='')
    res1 <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    res2 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=wifeinlf == 0)
    res3 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=wifeinlf == 1)
    #print(c(dim(bread(res1)),dim(meat(res1))))
    #print(c(dim(bread(res2)),dim(meat(res2))))
    #print(c(dim(bread(res3)),dim(meat(res3))))
    #print(setdiff(dimnames(meat(res1)),dimnames(bread(res1))))
    #print(setdiff(dimnames(meat(res2)),dimnames(bread(res2))))
    #print(setdiff(dimnames(meat(res3)),dimnames(bread(res3))))
    sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    sd.effect2 <- sd(RT)*coef(res2)[['RT']]
    sd.effect3 <- sd(RT)*coef(res3)[['RT']]
    iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    iqr.effect2 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res2)[['RT']]
    iqr.effect3 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res3)[['RT']]
    eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
    eststo2(res2, list('1 S.D. Effect'=sd.effect2, 'I.Q.R.  Effect)'=iqr.effect2))
    eststo2(res3, list('1 S.D. Effect'=sd.effect3, 'I.Q.R.  Effect)'=iqr.effect3))
}
columns <- c('All','Not in LF','In LF','All','Not in LF','In LF')
o<-esttab2(filename='Bhousewivesols.txt',indicate=ind.list, keep=keep.list,
           col.width=19, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
o<-esttab2(filename='Bhousewivesols.tex',indicate=ind.list, keep=keep.list,
           col.width=19, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
system('cat Bhousewivesols.txt')
################################################################################
# End Housewives
################################################################################

################################################################################
# Start Age Level
################################################################################
estclear()
stagecut <- 6
for(i in Xs){
    fstring <- paste('selfBTOT97 ~ ',i,sep='')
    res1 <- lm(as.formula(fstring), weight=CH97PRWT, data=q)
    res2 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=chage <= stagecut)
    res3 <- lm(as.formula(fstring), weight=CH97PRWT, data=q, subset=chage > stagecut)
    #print(c(dim(bread(res1)),dim(meat(res1))))
    #print(c(dim(bread(res2)),dim(meat(res2))))
    #print(c(dim(bread(res3)),dim(meat(res3))))
    sd.effect1 <- sd(RT)*coef(res1)[['RT']]
    sd.effect2 <- sd(RT)*coef(res2)[['RT']]
    sd.effect3 <- sd(RT)*coef(res3)[['RT']]
    iqr.effect1 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res1)[['RT']]
    iqr.effect2 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res2)[['RT']]
    iqr.effect3 <- (summary(RT)[['3rd Qu.']] - summary(RT)[['1st Qu.']])*coef(res3)[['RT']]
    eststo2(res1, list('1 S.D. Effect'=sd.effect1, 'I.Q.R.  Effect)'=iqr.effect1))
    eststo2(res2, list('1 S.D. Effect'=sd.effect2, 'I.Q.R.  Effect)'=iqr.effect2))
    eststo2(res3, list('1 S.D. Effect'=sd.effect3, 'I.Q.R.  Effect)'=iqr.effect3))
}
columns <- c('All','Not in LF','In LF','All','Not in LF','In LF')
o<-esttab2(filename='Bstageols.txt',indicate=ind.list, keep=keep.list,
           col.width=19, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
o<-esttab2(filename='Bstageols.tex',indicate=ind.list, keep=keep.list,
           col.width=19, var.rename=vrn, se.func=myvcov,
           col.headers=columns)
system('cat Bstageols.txt')
################################################################################
# End Age Level
################################################################################
