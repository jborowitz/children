#library('estout','zoo','sandwich')
library('zoo')
library('sandwich')
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
#load('pcgPared.RData')
sink()
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
#contrasts(SBLNUM03)<-'contr.sum'
#f1  <-  formula(odiffBTOT97  ~  RT  +  income  +  famsize  +  ageyoungest)
ss  <-   'mschooling97   <   90   &   fschooling97   <   90   &   dadhead)'
interactions <- 'SBLNUM03 +  ageyoungest02'

int <- c(rep('',2),rep(interactions,4))
Xs <- c('1' 
        ,' chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool' 
        ,'1' 
        ,' chagem2 + nonwhite + male + parentage + income + inschool' 
        ,' chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
        ,' chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool')

yread <- 'nreadscore02 '
ymath <- 'nappliedscore02 '
#y <- 'avgscore02 ~'
scores <- c('nmathscore + nreadscore')
#scores <- c('nreadscore')
xmath <- c('nappliedscore02*chagem')
xread <- c('nreadscore02*chagem')
investment <- c('BTOT02')
#scores <- c('avgscore97 ')
wholeX <- paste(int,Xs,sep=' + ')
wholeX <- gsub('\\+  \\+','\\+',wholeX)
wholeX <- gsub('\\+ $','',wholeX)
wholeX <- gsub('^ \\+ ','',wholeX)
wholeX <- gsub('^\\+ ','',wholeX)
#print(wholeX)
#firstformulas <- paste(paste(y,scores,sep=' '),paste(int,Xs,sep=' + '),sep = ' + ')
#firstformulas <- gsub('\\+  \\+','\\+',firstformulas)
#firstformulas <- gsub('\\+ $','',firstformulas)
outputlist <- c('BTOT02')
#outputvar <- 'Btravel02'
#firstformulas <- c(
                   #'nappliedscore02 ~ nmathscore + nreadscore'
#,'nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
#,paste('nappliedscore02 ~ nmathscore + nreadscore',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool',interactions,sep=" + "))

for(outputvar in outputlist){
estclear('math')
estclear('read')
for(f in wholeX){
    math.firststage.formula <- paste(paste(investment,xmath,sep=" ~ "),f,sep=" + ")
    read.firststage.formula <- paste(paste(investment,xread,sep=" ~ "),f,sep=" + ")
    #math.secondstage.formula <- paste(outputvar,'~',paste('mathresids*chagem',xmath,sep=" + "))
    mathfirststage <- lm(math.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    readfirststage <- lm(read.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    #datasubset$mathresids <- resid(mathfirststage)
    #math.secondstage <- lm(math.secondstage.formula, weight=CH97PRWT, data=datasubset)
    #sd.effect <- sd(resid(mathfirststage),na.rm=TRUE)*coef(math.secondstage)[['mathresids']]
    #eststo2(math.secondstage,list('1 S.D. Effect'=sd.effect), tableName='second')
    eststo2(mathfirststage, tableName='math')
    eststo2(readfirststage, tableName='read')
    print('_________________________________________')
    print(math.firststage.formula)
    print(read.firststage.formula)
}
ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='SBLNUM*')
columns <- rep(outputvar,6)
vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)',
'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad','fcdip'='Dad College','fhsdip'='Dad HS','nreadscore'='1997 Read','nmathscore'='1997 Math','resids'='Unexpected Change','BTOT97'='Time in 1997','mathresids'='Math Resid.','readresids'='Read Resid.','nappliedscore02:chagem'='Math * Age','nreadscore02:chagem'='Read * Age')
drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
keep.list  <-  list('mathresids','readresids','nappliedscore02','nreadscore02','nreadscore02:chagem','nappliedscore02:chagem','home97','nmathscore','nreadscore','BTOT97','chagem',  'chagem2',  'cdip',  'hsdip', 'income')
mathoutfile <- paste(outputvar,'math',sep='')
readoutfile <- paste(outputvar,'read',sep='')
o<-esttab2(filename=paste(mathoutfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='math')
o<-esttab2(filename=paste(mathoutfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='math')
o<-esttab2(filename=paste(readoutfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='read')
o<-esttab2(filename=paste(readoutfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='read')

#estclear()
#estclear('first')
#for(f in wholeX){
    #read.firststage.formula <- paste(paste(yread,xread,sep=" ~ "),f,sep=" + ")
    ##readfirststage <- lm(read.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    ##print(summary(readfirststage))
    ##datasubset$readresids <- resid(readfirststage)
    ##read.secondstage.formula <- paste(outputvar,'~',paste('readresids*chagem',xread,sep=" + "))
    ##read.secondstage <- lm(read.secondstage.formula, weight=CH97PRWT, data=datasubset)
    ##sd.effect <- sd(resid(readfirststage),na.rm=TRUE)*coef(read.secondstage)[['readresids']]
    ##eststo2(read.secondstage,list('1 S.D. Effect'=sd.effect))
    #eststo2(mathfirststage, tableName='first')
    #datasubset$readresids <- NULL
    #print('_________________________________________')
    #print(read.firststage.formula)
    ##print(read.secondstage.formula)
#}
#ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='famsize.*')
#columns <- rep(outputvar,6)
#vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)',
#'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad','fcdip'='Dad College','fhsdip'='Dad HS','nreadscore'='1997 Read','nmathscore'='1997 Math','resids'='Unexpected Change','BTOT97'='Time in 1997','mathresids'='Math Resid.','readresids'='Read Resid.')
#drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
#keep.list  <-  list('mathresids','readresids','nappliedscore02','nreadscore02','readresids:chagem','mathresids:chagem','home97','nmathscore','nreadscore','BTOT97','chagem',  'chagem2',  'cdip',  'hsdip', 'income')
#readoutfile <- paste(outputvar,'read',sep='')
#o<-esttab2(filename=paste(readoutfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
#o<-esttab2(filename=paste(readoutfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
#columns <- rep(yread,6)
#o<-esttab2(filename=paste('tempread','.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='first')
print('Math Second Stage')
system(paste('cat ',mathoutfile,'.txt',sep=''))
print('Reading Second Stage')
system(paste('cat ',readoutfile,'.txt',sep=''))
}

