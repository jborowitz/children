library('plyr')
library('foreign')
data <- read.dta('twoSibsPared.dta')
datasubset <- subset(data,subset=dadhead & mschooling97 < 97 & fschooling97 < 97 &
            !is.na(BTOT02) & SBLNUM03 != 96 & SBLNUM03 != 5 & !is.na(nreadscore02) &  !is.na(nappliedscore02))
datasubset$famsize <- as.factor(datasubset$famsize)
datasubset$ageyoungest <- as.factor(datasubset$ageyoungest)
datasubset$SBLNUM03 <- as.factor(datasubset$SBLNUM03)
datasubset$ageyoungest02 <- as.factor(datasubset$ageyoungest02)
datasubset$BTOT0 <- datasubset$BTOT02 + datasubset$BTOT07

normscores <- function(x){
    #print(dim(x))
    #if(is.null(x)) return(NA)
    x$stdmath02 <- (x$nappliedscore02 -mean(x$nappliedscore02,na.rm=TRUE))/sd(x$nappliedscore02,na.rm=TRUE)
    x$stdread02 <- (x$nreadscore02 -mean(x$nreadscore02,na.rm=TRUE))/sd(x$nreadscore02,na.rm=TRUE)
    x$stdread02 <- (x$nreadscore02 -mean(x$nreadscore02,na.rm=TRUE))/sd(x$nreadscore02,na.rm=TRUE)
    x$stdmath97 <- (x$nmathscore -mean(x$nmathscore,na.rm=TRUE))/sd(x$nmathscore,na.rm=TRUE)
    x$stdread97 <- (x$nreadscore -mean(x$nreadscore,na.rm=TRUE))/sd(x$nreadscore,na.rm=TRUE)
    return(x)
}
datasubset <- ddply(.data=datasubset,.variables=.(chage),.fun=normscores)
save(datasubset,file='twoSibsPared.RData')
