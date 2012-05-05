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

xmath <- c('stdmath02*chagem')
xread <- c('stdread02*chagem')
investment <- c('home97')
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
#outputlist <- c('BTOT02')
outputlist <- c('BTOT02','BTOT97','stdhome97','stdhome02')
#outputvar <- 'Btravel02'
#firstformulas <- c(
                   #'stdmath02 ~ stdmath97 + stdread97'
#,'stdmath02 ~ stdmath97 + stdread97 + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool'
#,paste('stdmath02 ~ stdmath97 + stdread97',interactions,sep=" + ")
#,paste('stdmath02 ~ stdmath97 + stdread97 + chagem + chagem2 + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('stdmath02 ~ stdmath97 + stdread97 + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + income + inschool',interactions,sep=" + ")
#,paste('stdmath02 ~ stdmath97 + stdread97 + chagem + chagem2 + cdip + hsdip + fcdip + fhsdip + nonwhite + male + parentage + inschool',interactions,sep=" + "))

for(outputvar in outputlist){
estclear('math')
estclear('read')
for(f in wholeX){
    math.formula <- paste(paste(outputvar,xmath,sep=" ~ "),f,sep=" + ")
    read.formula <- paste(paste(outputvar,xread,sep=" ~ "),f,sep=" + ")
    math.result <- lm(math.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    read.result <- lm(read.formula, weight=CH97PRWT, data=datasubset,na.action=na.exclude)
    #datasubset$mathresids <- resid(math)
    #math.secondstage <- lm(math.secondstage.formula, weight=CH97PRWT, data=datasubset)
    #sd.effect <- sd(resid(math),na.rm=TRUE)*coef(math.secondstage)[['mathresids']]
    #eststo2(math.secondstage,list('1 S.D. Effect'=sd.effect), tableName='second')
    eststo2(math.result, tableName='math')
    eststo2(read.result, tableName='read')
    print('_________________________________________')
    print(math.formula)
    print(read.formula)
}
ind.list <- list('Age of Youngest'='ageyoungest.*','Family Size'='SBLNUM03.*')
columns <- rep(outputvar,6)
vrn <- list('RT'='Risk Tolerance','income'='Income /100k', 'chagem'='Age (Months)',
'chagem2'='Age^2', 'cdip'='College Degree', 'hsdip'='HS Grad','fcdip'='Dad College','fhsdip'='Dad HS','stdread97'='1997 Read','stdmath97'='1997 Math','resids'='Unexpected Change','BTOT97'='Time in 1997','mathresids'='Math Resid.','readresids'='Read Resid.','stdmath02:chagem'='2002 Math * Age','stdread02:chagem'='2002 Read * Age','stdmath02'='2002 Math','stdread02'='2002 Read','stdread97:chagem'='1997 Read * Age','stdmath97:chagem'='1997 Math * Age')
drop.list <- list('income','RT','ageyoungest02','SBLNUM03')
keep.list  <-  list('mathresids','readresids','stdmath02','stdread02','stdread02:chagem','stdmath02:chagem','home97','stdmath97','stdread97','BTOT97','chagem',  'chagem2',  'cdip',  'hsdip', 'income','stdread97:chagem','stdmath97:chagem')
mathoutfile <- paste(outputvar,'math',sep='')
readoutfile <- paste(outputvar,'read',sep='')
o<-esttab2(filename=paste(mathoutfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='math')
o<-esttab2(filename=paste(mathoutfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='math')
o<-esttab2(filename=paste(readoutfile,'.txt',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='read')
o<-esttab2(filename=paste(readoutfile,'.tex',sep=''),indicate=ind.list,  col.width=15, var.rename=vrn, col.headers=columns, keep=keep.list, tableName='read')

print('Math Second Stage')
system(paste('cat ',mathoutfile,'.txt',sep=''))
print('Reading Second Stage')
system(paste('cat ',readoutfile,'.txt',sep=''))
}

