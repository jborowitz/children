#library('estout','zoo','sandwich')
library('zoo')
library('sandwich',lib='/home/jborowitz/R/x86_64-redhat-linux-gnu-library')
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
sink('/dev/null')
#data <- read.dta('/home/jborowitz/R/pcgPared.dta')
data <- read.dta('/home/jborowitz/R/twoSibsPared.dta')
#data<-load('/home/jborowitz/R/pcgPared.RData')
#data<-load('/home/jborowitz/R/twoSibsPared.dta')
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
ss  <-   'mschooling97   <   90   &   fschooling97   <   90   &   dadhead)'
interactions <- 'famsize + ageyoungest + SBLNUM03 +  ageyoungest02'

int <- c(rep('',2),rep(interactions,4))
Xs <- c( 'BTOT97 ' 
        ,'BTOT97 +chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool' 
        ,'BTOT97 ' 
        ,'BTOT97 +chagem + chagem2 + nonwhite + male + parentage + income + inschool' 
        ,'BTOT97 +chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
        ,'BTOT97 +chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool')

yread <- 'nreadscore02'
ymath <- 'nappliedscore02'
#y <- 'avgscore02 ~'
scores <- c('nmathscore + nreadscore')
#scores <- c('nreadscore')
xmath <- c('nmathscore')
mathinteract <- 'nmathscore*BTOT97'
readinteract <- 'nreadscore*BTOT97'
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
outputlist <- c('nreadscore02','nappliedscore02')
#outputvar <- 'Btravel02'
#firstformulas <- c(
                   #'nappliedscore02 ~ nmathscore + nreadscore'
#,'nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
#,paste('nappliedscore02 ~ nmathscore + nreadscore',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('nappliedscore02 ~ nmathscore + nreadscore + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool',interactions,sep=" + "))

ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='famsize.*')
columns <- rep('Math 2002',6)
vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)', 'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad','fcdip'='Dad College','fhsdip'='Dad HS','nreadscore'='1997 Read','nmathscore'='1997 Math','resids'='Unexpected Change','BTOT97'='Time in 1997','readresids'='read Resid.','readresids'='Read Resid.','nmathscore:BTOT97'='Math*Time 97','nreadscore:BTOT97'='Read*Time 97')
drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
keep.list  <-  list('readresids','readresids','nappliedscore02','nreadscore02','nreadscore','nmathscore','BTOT97','chagem',  'chagem2',  'cdip',  'hsdip', 'income','nmathscore:BTOT97','nreadscore:BTOT97')

estclear()
for(f in wholeX){
    print(paste(ymath,mathinteract,sep=" ~ "))
    print(f)
    math.firststage.formula <- paste(paste(ymath,mathinteract,sep=" ~ "),f,sep=" + ")
    mathfirststage <- lm(math.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    eststo2(mathfirststage)
    print('_________________________________________')
    print(math.firststage.formula)
}
outfile <- paste(ymath,'prod',sep='')
print(outfile)
o<-esttab2(filename=paste(outfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
o<-esttab2(filename=paste(outfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
system(paste('cat ',outfile,'.txt',sep=''))

estclear()
for(f in wholeX){
    read.firststage.formula <- paste(paste(yread,readinteract,sep=" ~ "),f,sep=" + ")
    readfirststage <- lm(read.firststage.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    eststo2(readfirststage)
    print('_________________________________________')
    print(read.firststage.formula)
}
outfile <- paste(yread,'prod',sep='')
print(outfile)
o<-esttab2(filename=paste(outfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
o<-esttab2(filename=paste(outfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list)
system(paste('cat ',outfile,'.txt',sep=''))
