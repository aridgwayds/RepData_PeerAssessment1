#sourcepath = Enter the value for the source file summarySCC_PM25.rds and Source_Classification_Code.rds
#Get Vars
#Load required packages

library(readr)
library(dplyr)
library(tidyr)

#set vars
filename<-params$sourcepath
cols<-list(col_integer(),col_date(format=""),col_integer())
doPlot<-FALSE

#read data
activity<-read_csv(filename, col_types=cols)                 
nrows<-nrow(activity)

if(nrows>0) {
        doPlot<-TRUE
        knitr::kable(summary(activity), caption ="**Summary of activity.zip dataset**")
}

#1 Mean steps taken per day
#1 Mean steps taken per day
steps_per_day<-plyr::ddply(activity,"date",plyr::summarise,total.steps=sum(steps, na.rm=TRUE))

meanSteps<-prettyNum(round(mean(steps_per_day$total.steps, na.rm=TRUE),0),big.mark=",")
medianSteps<-prettyNum(median(steps_per_day$total.steps, na.rm=TRUE),big.mark=",")

hist(steps_per_day$total.steps,main="Total Steps per Day",col="gray",xlab="Total Steps",ylab="Frequency")

#2 Mean steps per interval
steps_per_intval<-plyr::ddply(activity,"interval",plyr::summarise,avg.steps=mean(steps, na.rm=TRUE)) %>% mutate(avg.steps=round(avg.steps,1))
maxIntval<-steps_per_intval[which.max(steps_per_intval$avg.steps),1]
maxSteps<-round(max(steps_per_intval$avg.steps),0)

plot(steps_per_intval,type="l",main="Average Steps per 5-minute Interval",col="blue",lwd=1,xlab="Interval",ylab="Avg. Steps across All Dates")
abline(v=maxIntval,lwd=4,col="gray")
text(maxIntval,maxSteps,paste("Max Avg Steps=",maxSteps,"for interval",maxIntval),pos=4,cex=.8)

#3 Imputing NAs
rowsNA<-prettyNum(nrow(activity)-sum(complete.cases(activity)),big.mark=",")

meanStepsInt<-round(mean(steps_per_intval$avg.steps,na.rm=TRUE),0) 

activity_full<-mutate(activity,steps=replace_na(steps,meanStepsInt))

rowsNA2<-prettyNum(nrow(activity_full)-sum(complete.cases(activity_full)),big.mark=",")
steps_per_day2<-plyr::ddply(activity_full,"date",plyr::summarise,total.steps=sum(steps, na.rm=FALSE))

meanSteps2<-prettyNum(round(mean(steps_per_day2$total.steps, na.rm=TRUE),0),big.mark=",")
medianSteps2<-prettyNum(round(median(steps_per_day2$total.steps, na.rm=TRUE),0),big.mark=",")

hist(steps_per_day2$total.steps,main="Total Steps per Day\n(Imputed NAs using overall mean steps per interval value)",col="gray",xlab="Total Steps",ylab="Frequency")




#4 Difference between weekdays and weekends

library(lattice)
activity_full<-mutate(activity_full,
                      dow=(ifelse(weekdays(activity_full$date) %in% list("Saturday","Sunday"), "weekend","weekday")),dow=factor(dow))

steps_per_intval2<-plyr::ddply(activity_full,c("dow","interval"),plyr::summarise,avg.steps=mean(steps, na.rm=TRUE)) %>% mutate(avg.steps=round(avg.steps,1))

p<-xyplot(steps_per_intval2$avg.steps ~ steps_per_intval2$interval | steps_per_intval2$dow, type="l",layout = c(1, 2),xlab="Interval",ylab="Steps",main="Steps per Interval") ## Plot with 2 panels

print(p)


