##############################################################################################################################
##############################################################################################################################
##R CODE FOR US MIGRATION (MOVERS) BY AGE REVIEW - ACS AND CPS DATA
##
##EDDIE HUNSINGER (AFFILIATION: ALASKA DEPARTMENT OF LABOR AND WORKFORCE DEVELOPMENT), DECEMBER 2018
##http://www.demog.berkeley.edu/~eddieh/
##edyhsgr@gmail.com
##
##Using downloaded US Census ACS 1-year and BLS CPS ASEC data, accessed via from IPUMS-USA, University of Minnesota, www.ipums.org.
##############################################################################################################################
##############################################################################################################################

##########
#CALCULATE THE MEDIAN AGE FOR A GIVEN AGE PROFILE (EDDIE HUNSINGER, 2008)
#http://www.demog.berkeley.edu/~eddieh/toolbox.html#MedianAge 
source("https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_MedianAge/master/MedianAge.R")
##########

##########
#Set graph space
par(mfrow=c(2,2))
##########

##########
#Read in ACS mover data
ACSTotalMovers<-read.table(file="https://raw.githubusercontent.com/edyhsgr/USMigrationByAgeOverTimeReview/master/DataFromIPUMSACS/ACSMoversTotal_2001to2017.csv",header=TRUE,sep=",")
ACSTotalTotal<-read.table(file="https://raw.githubusercontent.com/edyhsgr/USMigrationByAgeOverTimeReview/master/DataFromIPUMSACS/ACSTotalTotal_2001to2017.csv",header=TRUE,sep=",")

#Age specific migration rates
ACSTotalMoversRate<-array(0,c(81,19))
for (i in 3:19) {ACSTotalMoversRate[,i]<-ACSTotalMovers[1:81,i]/ACSTotalTotal[1:81,i]}
ACSTotalMoversRateScaled<-array(0,c(81,19))
for (i in 3:19) {ACSTotalMoversRateScaled[,i]<-ACSTotalMoversRate[,i]/sum(ACSTotalMoversRate[,i])}

#Graph age specific migration
plot(ACSTotalMoversRate[2:81,8],ylab="",xlab="",ylim=c(0,.45),axes=F,type="l",lwd="1",panel.first=c(abline(v=c(1,17,50,80),col="grey50",lty=3)))
for (i in 3:7) {points(ACSTotalMoversRate[2:81,i],type="l",lwd="1",col="grey75")}
for (i in 8:19) {points(ACSTotalMoversRate[2:81,i],type="l",lwd="1",col=rainbow(19)[i])}
axis(side=1,at=5*(0:81),las=2,cex.axis=0.75)
axis(side=2,cex.axis=0.8)
title(c("Age Specific Rates of Moved in Last Year","US, 2001 to 2017 Annual (ACS 1-Year)"),cex.main=.8)
legend(40,.42,bg="white",legend=c("2001 to 2005", "2006", "2011", "2017"), 
col=c("grey75",rainbow(19)[8],rainbow(19)[14],rainbow(19)[19]), 
lwd=c(1,1,1,1), cex=.7)
mtext(side=1,line=2,adj=.5,text="Age",font=1,cex=.75,col="black")
mtext(side=2,line=2.5,adj=.5,text="Rate of Moved in Last Year",font=1,cex=.75,col="black")
mtext(side=1,line=-12,adj=.5,text="(2001 to 2005 source data don't appear comparable to 2006 to 2017.)",font=1,cex=.6,col="black")
mtext(side=1,line=3,adj=0,text="Using US Census Bureau American Community Survey data",font=1,cex=.5,col="black")
mtext(side=1,line=3.5,adj=0,text="from IPUMS-USA, University of Minnesota, www.ipums.org.",font=1,cex=.5,col="black")

#Calculate and graph median age of 18 to 49 year old age-adjusted migration rates (2017 standard)
ACS18to49MoverRatesMedianAge<-array(0,19)
for (i in 1:length(ACS18to49MoverRatesMedianAge)) {ACS18to49MoverRatesMedianAge[i]<-medage(data.frame(ACSTotalMoversRate[19:50,i]*ACSTotalTotal[19:50,19]),1)+18}
plot(ACS18to49MoverRatesMedianAge[8:19],ylab="",xlab="",axes=F,type="l",lwd="3",col="orange",ylim=c(25,40),panel.first=c(abline(h=c(30,35),col="grey50",lty=3)))

#Calculate and graph median age of 18 to 79 year old age-adjusted migration rates (2017 standard)
ACS18to79MoverRatesMedianAge<-array(0,19)
for (i in 1:length(ACS18to79MoverRatesMedianAge)) {ACS18to79MoverRatesMedianAge[i]<-medage(data.frame(ACSTotalMoversRate[19:80,i]*ACSTotalTotal[19:80,19]),1)+18}
points(ACS18to79MoverRatesMedianAge[8:19],type="l",lwd="3",col="forestgreen")
axis(side=1,at=1:12,labels=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"),las=2,cex.axis=0.75)
axis(side=2,cex.axis=0.8)
title(c("Median Age of Moved in Last Year, Selected Age Groups","US, 2006 to 2017 Annual (ACS 1-Year)"),cex.main=.8)
legend(7,39,bg="white",legend=c("Ages 18 to 79", "Ages 18 to 49"), 
col=c("forestgreen","orange"), 
lwd=c(3,3), cex=.7)
mtext(side=1,line=2.5,adj=.5,text="Year",font=1,cex=.75,col="black")
mtext(side=1,line=-12,adj=.5,text="(Age-Adjusted, Using 2017 Standard)",font=1,cex=.6,col="black")
mtext(side=2,line=2.5,adj=.5,text="Median Age of Moved in Last Year",font=1,cex=.75,col="black")
mtext(side=1,line=3.25,adj=0,text="Using US Census Bureau American Community Survey data",font=1,cex=.5,col="black")
mtext(side=1,line=3.75,adj=0,text="from IPUMS-USA, University of Minnesota, www.ipums.org.",font=1,cex=.5,col="black")
##########

##########
#Read in CPS mover data - 2001 to 2017
CPSTotalMovers<-read.table(file="https://raw.githubusercontent.com/edyhsgr/USMigrationByAgeOverTimeReview/master/DataFromIPUMSCPS/CPSMoversTotal_2001to2017.csv",header=TRUE,sep=",")
CPSTotalTotal<-read.table(file="https://raw.githubusercontent.com/edyhsgr/USMigrationByAgeOverTimeReview/master/DataFromIPUMSCPS/CPSTotalTotal_2001to2017.csv",header=TRUE,sep=",")

#Age specific migration rates - 2001 to 2017
CPSTotalMoversRate<-array(0,c(86,19))
for (i in 3:19) {CPSTotalMoversRate[,i]<-CPSTotalMovers[,i]/CPSTotalTotal[,i]}
CPSTotalMoversRate[is.nan(CPSTotalMoversRate)] <- 0
CPSTotalMoversRateScaled<-array(0,c(86,19))
for (i in 3:19) {CPSTotalMoversRateScaled[,i]<-CPSTotalMoversRate[,i]/sum(CPSTotalMoversRate[,i])}

#Graph age specific migration - 2001 to 2017
plot(CPSTotalMoversRate[2:81,8],ylab="",xlab="",ylim=c(0,.45),axes=F,type="l",lwd="1",panel.first=c(abline(v=c(1,17,50,80),col="grey50",lty=3)))
for (i in 3:19) {points(CPSTotalMoversRate[2:81,i],type="l",lwd="1",col=rainbow(19)[i])}
axis(side=1,at=5*(0:81),las=2,cex.axis=0.75)
axis(side=2,cex.axis=0.8)
title(c("Age Specific Rates of Moved in Last Year","US, 2001 to 2017 Annual (CPS ASEC)"),cex.main=.8)
legend(45,.42,bg="white",legend=c("2001", "2006", "2011", "2017"), 
col=c(rainbow(19)[3],rainbow(19)[8],rainbow(19)[14],rainbow(19)[19]), 
lwd=c(1,1,1,1), cex=.7)
mtext(side=1,line=2,adj=.5,text="Age",font=1,cex=.75,col="black")
mtext(side=2,line=2.5,adj=.5,text="Rate of Moved in Last Year",font=1,cex=.75,col="black")
mtext(side=1,line=3,adj=0,text="Using US Bureau of Labor Statistics Current Population Survey data",font=1,cex=.5,col="black")
mtext(side=1,line=3.5,adj=0,text="from IPUMS-USA, University of Minnesota, www.ipums.org.",font=1,cex=.5,col="black")

#Calculate and graph median age of 18 to 49 year old age adjusted migration rates (2017 standard) - 2001 to 2017
CPS18to49MoverRatesMedianAge<-array(0,19)
for (i in 1:length(CPS18to49MoverRatesMedianAge)) {CPS18to49MoverRatesMedianAge[i]<-medage(data.frame(CPSTotalMoversRate[19:50,i]*CPSTotalTotal[19:50,19]),1)+18}
plot(CPS18to49MoverRatesMedianAge[3:19],ylab="",xlab="",axes=F,type="l",lwd="3",col="orange",ylim=c(25,40),panel.first=c(abline(h=c(30,35),col="grey50",lty=3)))

#Calculate and graph median age of 18 to 79 year old age adjusted migration rates (2017 standard) - 2001 to 2017
CPS18to79MoverRatesMedianAge<-array(0,19)
for (i in 1:length(CPS18to79MoverRatesMedianAge)) {CPS18to79MoverRatesMedianAge[i]<-medage(data.frame(CPSTotalMoversRate[19:80,i]*CPSTotalTotal[19:80,19]),1)+18}
points(CPS18to79MoverRatesMedianAge[3:19],type="l",lwd="3",col="forestgreen")
axis(side=1,at=1:17,labels=c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"),las=2,cex.axis=0.75)
axis(side=2,cex.axis=0.8)
title(c("Median Age of Moved in Last Year, Selected Age Groups","US, 2001 to 2017 Annual (CPS ASEC)"),cex.main=.8)
legend(10,39,bg="white",legend=c("Ages 18 to 79", "Ages 18 to 49"), 
col=c("forestgreen","orange"), 
lwd=c(3,3), cex=.7)
mtext(side=1,line=2.5,adj=.5,text="Year",font=1,cex=.75,col="black")
mtext(side=1,line=-12,adj=.5,text="(Age-Adjusted, Using 2017 Standard)",font=1,cex=.6,col="black")
mtext(side=2,line=2.5,adj=.5,text="Median Age of Moved in Last Year",font=1,cex=.75,col="black")
mtext(side=1,line=3.25,adj=0,text="Using US Bureau of Labor Statistics Current Population Survey data",font=1,cex=.5,col="black")
mtext(side=1,line=3.75,adj=0,text="from IPUMS-USA, University of Minnesota, www.ipums.org.",font=1,cex=.5,col="black")
##########

##########
#Read in CPS mover data - 1963 to 2017
CPSTotalMovers<-read.table(file="https://raw.githubusercontent.com/edyhsgr/USMigrationByAgeOverTimeReview/master/DataFromIPUMSCPS/CPSMoversTotal_1963to2017.csv",header=TRUE,sep=",")
CPSTotalTotal<-read.table(file="https://raw.githubusercontent.com/edyhsgr/USMigrationByAgeOverTimeReview/master/DataFromIPUMSCPS/CPSTotalTotal_1963to2017.csv",header=TRUE,sep=",")

#Age specific migration rates - 1980 to 2017
CPSTotalMoversRate<-array(0,c(86,57))
for (i in 3:57) {CPSTotalMoversRate[,i]<-CPSTotalMovers[,i]/CPSTotalTotal[,i]}
CPSTotalMoversRate[is.nan(CPSTotalMoversRate)] <- 0
CPSTotalMoversRateScaled<-array(0,c(86,57))
for (i in 3:57) {CPSTotalMoversRateScaled[,i]<-CPSTotalMoversRate[,i]/sum(CPSTotalMoversRate[,i])}

#Graph age specific migration - 1980 to 2017
plot(CPSTotalMoversRate[2:81,20],ylab="",xlab="",ylim=c(0,.45),axes=F,type="l",lwd="1",panel.first=c(abline(v=c(1,17,50,80),col="grey50",lty=3)))
for (i in 20:57) {points(CPSTotalMoversRate[2:81,i],type="l",lwd="1",col=rainbow(60)[i])}
axis(side=1,at=5*(0:81),las=2,cex.axis=0.75)
axis(side=2,cex.axis=0.8)
title(c("Age Specific Rates of Moved in Last Year","US, 1980 to 2017 Annual (CPS ASEC)"),cex.main=.8)
legend(45,.42,bg="white",legend=c("1980", "1992", "2004", "2017"), 
col=c(rainbow(60)[23],rainbow(60)[35],rainbow(60)[47],rainbow(60)[60]), 
lwd=c(1,1,1,1), cex=.7)
mtext(side=1,line=2,adj=.5,text="Age",font=1,cex=.75,col="black")
mtext(side=2,line=2.5,adj=.5,text="Rate of Moved in Last Year",font=1,cex=.75,col="black")
mtext(side=1,line=3,adj=0,text="Using US Bureau of Labor Statistics Current Population Survey data",font=1,cex=.5,col="black")
mtext(side=1,line=3.5,adj=0,text="from IPUMS-USA, University of Minnesota, www.ipums.org.",font=1,cex=.5,col="black")

#Calculate and graph median age of 18 to 49 year old age adjusted migration rates (2017 standard) - 1980 to 2017
CPS18to49MoverRatesMedianAge<-array(0,57)
CPSTotalMoversRate[is.na(CPSTotalMoversRate)] <- 0
for (i in 1:length(CPS18to49MoverRatesMedianAge)) {CPS18to49MoverRatesMedianAge[i]<-medage(data.frame(CPSTotalMoversRate[19:50,i]*CPSTotalTotal[19:50,57]),1)+18}
plot(CPS18to49MoverRatesMedianAge[20:57],ylab="",xlab="",axes=F,type="l",lwd="3",col="orange",ylim=c(25,40),panel.first=c(abline(h=c(30,35),col="grey50",lty=3)))

#Calculate and graph median age of 18 to 79 year old age adjusted migration rates (2017 standard) - 1980 to 2017
CPS18to79MoverRatesMedianAge<-array(0,57)
CPSTotalMoversRate[is.na(CPSTotalMoversRate)] <- 0
for (i in 1:length(CPS18to79MoverRatesMedianAge)) {CPS18to79MoverRatesMedianAge[i]<-medage(data.frame(CPSTotalMoversRate[19:80,i]*CPSTotalTotal[19:80,57]),1)+18}
points(CPS18to79MoverRatesMedianAge[20:57],type="l",lwd="3",col="forestgreen")
axis(side=1,at=1:38,
labels=c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"),
las=2,cex.axis=0.75)
axis(side=2,cex.axis=0.8)
title(c("Median Age of Moved in Last Year, Selected Age Groups","US, 1980 to 2017 Annual (CPS ASEC)"),cex.main=.8)
legend(10,39,bg="white",legend=c("Ages 18 to 79", "Ages 18 to 49"), 
col=c("forestgreen","orange"), 
lwd=c(3,3), cex=.7)
mtext(side=1,line=2.5,adj=.5,text="Year",font=1,cex=.75,col="black")
mtext(side=1,line=-12,adj=.5,text="(Age-Adjusted, Using 2017 Standard)",font=1,cex=.6,col="black")
mtext(side=2,line=2.5,adj=.5,text="Median Age of Moved in Last Year",font=1,cex=.75,col="black")
mtext(side=1,line=3.25,adj=0,text="Using US Bureau of Labor Statistics Current Population Survey data",font=1,cex=.5,col="black")
mtext(side=1,line=3.75,adj=0,text="from IPUMS-USA, University of Minnesota, www.ipums.org.",font=1,cex=.5,col="black")
##########

##########
##Related US Census Bureau reports:
#https://www.census.gov/library/publications/2015/acs/acs-31.html
#https://www.census.gov/programs-surveys/acs/methodology/questionnaire-archive.html
#https://www.census.gov/programs-surveys/cps/technical-documentation/complete.html
#https://www.census.gov/topics/population/migration/guidance/differences-between-the-migration-estimates.html
#https://www.census.gov/topics/population/migration.html
#https://www.census.gov/library/working-papers/2017/demo/SEHSD-WP2017-02.html
##########
