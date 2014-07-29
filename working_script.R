
dir<-"C:/Users/Jleach1/Documents/R/reproducible"
setwd(dir)
# load required packages
library(lubridate,quietly=T)
library(dplyr,quietly=T)
library(xtable,quietly=T)
library(ggplot2,quietly=T)
library(sqldf,quietly=T)
library(gridExtra,quietly=T)


# get and read in the data
# set data location
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# create a directory to store the data
if(!file.exists("./data")){dir.create("./data",showWarnings = FALSE)}

# download and unzip the data
ifelse(Sys.info()["sysname"]=="Windows",
       download.file(url=url,"./data/data.zip",quiet=TRUE),
       download.file(url=url,"./data/data.zip",method="curl",quiet=T)
       )
unzip("./data/data.zip",overwrite=TRUE,exdir="./data")

# read in the data
data<-read.csv("./data/activity.csv",
               header=T,
               stringsAsFactors=F,
               colClasses=c("numeric",rep("character",2)))

# questions to address:
# 1. mean and median steps per day
# 2. histogram of total steps per day

# 3. average daily activity - average steps per interval
# 4. - which 5 minute interval, averaged accross all days, has the most steps

# imputing missing values
# 5. total number of missing values (i.e. sum is.na)
# 6. fill in missing values, e.g. mean/median steps


# process the data
data$date<-ymd(data$date)
data$day<-wday(data$date,label=T,abbr=F)
data$week<-factor(x=ifelse(data$day %in% c("Saturday","Sunday")
                           ,"Weekend"
                           ,"Weekday")
                  ,levels=c("Weekend","Weekday")
                  )
data$time<-ifelse(nchar(data$interval)==1,
                  paste("00:00:0",data$interval,sep=""),
                  ifelse(nchar(data$interval)==2,
                         paste("00:00:",data$interval,sep=""),
                  ifelse(nchar(data$interval)==3,
                         paste("0",substr(data$interval,1,1),":",substr(data$interval,2,3),":00",sep=""),
                  ifelse(nchar(data$interval)==4,
                         paste(substr(data$interval,1,2),":",substr(data$interval,3,4),":00",sep=""),
                         paste("")))))



# summarise the data by day
day_summary<-data %.%
             select(-time) %.%
             group_by(date) %.%
             summarise(total_steps=sum(na.omit(steps)))

# make the histogram
(ggplot(day_summary,aes(x=na.omit(total_steps)))
+geom_histogram(binwidth=1000,fill="steelblue",color="grey")
+theme_minimal()
+xlab("Total Steps Per Day (Intervals of 1000)")
+ylab("Count of Days")
+ggtitle("Histogram of Total Steps Taken Per Day"))

# calculate the median and mean total steps per day
# data.frame("Median Total Steps Per Day"=round(median(day_summary$total_steps)),
#            "Mean Total Steps Per Day"=round(mean(day_summary$total_steps)),
#            check.names=F)

## prep for md document - make it a table to print
days<-xtable((data.frame("Median Steps Per Day"=round(median(day_summary[,2])),
           "Mean Total Steps Per Day"=round(mean(day_summary[,2])),
           check.names=F)))
print(days,type="html")


# summarise the data by interval
interval_summary<-data.frame(
                  data %.%
                  group_by(time) %.%
                  summarise(average_steps=median(na.omit(steps))))
# format time as time
interval_summary$time<-parse_date_time(interval_summary$time,orders="H:M:S")
# find the first and last rows (i.e. times) that steps are recorded
interval_summary$row<-seq(1:nrow(interval_summary))
first_data<-min(filter(interval_summary,average_steps!=0)$row)
last_data<-max(filter(interval_summary,average_steps!=0)$row)
interval_summary<-select(interval_summary,-row)

# plot line graph of average steps per interval
ggplot(interval_summary[(first_data-12):(last_data+12),], # +/- 1 hour each end
       aes(x=time,y=average_steps,group=1))+
  geom_line(color="steelblue")+
  xlab("Time of day (5 minute intervals)")+
  ylab("Average steps per 5 minute interval")+
  ggtitle("Average steps per 5 minute interval accross the day")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# find and table the interal with the maximum average steps
max_interval<-filter(interval_summary,average_steps==max(average_steps))
intervals<-xtable(data.frame(
                      "Interval"=substring(as.character(max_interval[,1]),11),
                      "Average Steps"=max_interval[,2],
                      check.names=F))
print(intervals,type="html")

# imputing missing values
# 5. total number of missing values (i.e. sum is.na)
mising_values<-sum(is.na(data$steps))

# 6. fill in missing values, e.g. mean/median steps
# median steps per day given by:
sum(is.na(data %.%
          select(-time) %.%
          group_by(date) %.%
          summarise(total_steps=median(na.omit(steps)))
          )[,2]
    )
## looks like day summary still provides missing values - try interval!
sum(is.na(interval_summary$average_steps))
# no missing values - use this!

# therefore we'll fill in missing values witht the median value for that time
# use sql and the coalesce function to do this?
data2<-sqldf("SELECT
                 data.*
                ,coalesce(data.steps,med.average_steps) as clean_steps
              FROM
                data2 as data
              LEFT JOIN
                interval_summary as med
              ON
                data.time = med.time"
             )
## recalculate median and mean values using this cleaned/imputed data set
day_summary_clean<- data2 %.%
                    select(-time) %.%
                    group_by(date) %.%
                    summarise(total_steps=sum(na.omit(steps)))

# make the histogram using the imputed data
ggplot(day_summary_clean,aes(x=na.omit(total_steps)))+
 geom_histogram(binwidth=1000,fill="firebrick",color="grey")+
 theme_minimal()+
 xlab("Total Steps Per Day (Intervals of 1000)")+
 ylab("Count of Days")+
 ggtitle("Histogram of total steps per day, with imputed values for missing data")

# calculate the median and mean total steps per day with the imputed data
## prep for md document - make it a table to print
days_clean<-xtable(data.frame("Median Steps Per Day with imputed data"=
                                 round(median(day_summary_clean[,2])),
                         "Mean Total Steps Per Day with imputed data"=
                           round(mean(day_summary_clean[,2])),
                         check.names=F))
print(days_clean,type="html")

## create summary that can be split by weekend
interval_summary_weeks<- data.frame(
                        data2 %.%
                        group_by(week, time) %.%
                        summarise(average_steps=median(na.omit(steps))))


# weekend data
weekend<-interval_summary_weeks %.% filter (week=="Weekend")
# format time as time
weekend$time<-parse_date_time(weekend$time,orders="H:M:S")
# find the first and last rows (i.e. times) that steps are recorded
weekend$row<-seq(1:nrow(weekend))
first_data_weekend<-min(filter(weekend,average_steps!=0)$row)
last_data_weekend<-max(filter(weekend,average_steps!=0)$row)
weekend<-select(weekend,-row)


# week data
week<-interval_summary_weeks %.% filter (week=="Weekday")
# format time as time
week$time<-parse_date_time(week$time,orders="H:M:S")
# find the first and last rows (i.e. times) that steps are recorded
week$row<-seq(1:nrow(week))
first_data_week<-min(filter(week,average_steps!=0)$row)
last_data_week<-max(filter(week,average_steps!=0)$row)
week<-select(week,-row)


week_plot<-
  ggplot(week[(first_data_week-12):(last_data_week+12),], # +/- 1 hour each end
         aes(x=time,y=average_steps,group=1))+
  geom_line(color="firebrick")+
  xlab("Time")+
  ylab("Avg. Steps")+
  ggtitle("Weekdays")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

weekend_plot<-
  ggplot(weekend[(first_data_weekend-12):(last_data_weekend+12),], # +/- 1 hour each end
         aes(x=time,y=average_steps,group=1))+
  geom_line(color="darkgreen")+
  xlab("Time")+
  ylab("Avg. Steps")+
  ggtitle("Weekends")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(week_plot,
             weekend_plot,
             ncol=1,
             nrow=2,
             main="Average Steps Per 5 Minute Interval")








