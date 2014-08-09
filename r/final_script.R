################################################################################
# Step 0 - Preparing the R working environment - Packages
################################################################################
# 1. lubridate
if(require("lubridate",quietly=T)){
  print("loading lubridate")
} else {
  print("trying to install lubridate")
  install.packages("lubridate")
  library("lubridate",quietly=T)
  print("lubridate installed and loaded")
  if(require("lubridate",quietly=T)){
  } else {
    stop("could not install lubridate")
  }
}
# 2. dplyr
if(require("dplyr",quietly=T)){
  print("loading dplyr")
} else {
  print("trying to install dplyr")
  install.packages("dplyr")
  library("dplyr",quietly=T)
  print("dplyr installed and loaded")
  if(require("dplyr",quietly=T)){
  } else {
    stop("could not install dplyr")
  }
}
# 3. ggplot2
if(require("ggplot2",quietly=T)){
  print("loading ggplot2")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  library("ggplot2",quietly=T)
  print("ggplot2 installed and loaded")
  if(require("ggplot2",quietly=T)){
  } else {
    stop("could not install ggplot2")
  }
}
# 4. sqldf
if(require("sqldf",quietly=T)){
  print("loading sqldf")
} else {
  print("trying to install sqldf")
  install.packages("sqldf")
  library("sqldf",quietly=T)
  print("sqldf installed and loaded")
  if(require("sqldf",quietly=T)){
  } else {
    stop("could not install sqldf")
  }
}
# 5. gridExtra
if(require("gridExtra",quietly=T)){
  print("loading gridExtra")
} else {
  print("trying to install gridExtra")
  install.packages("gridExtra")
  library("gridExtra",quietly=T)
  print("gridExtra installed and loaded")
  if(require("gridExtra",quietly=T)){
  } else {
    stop("could not install gridExtra")
  }
}
# 6. scales
if(require("scales",quietly=T)){
  print("loading scales")
} else {
  print("trying to install scales")
  install.packages("scales")
  library("scales",quietly=T)
  print("scales installed and loaded")
  if(require("scales",quietly=T)){
  } else {
    stop("could not install scales")
  }
}
################################################################################
# Step 1 - Getting and loading the data
################################################################################
# 1a. Set the URL for the data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# 1b. Conditionally create a directory in the working directory to store the data
if(!file.exists("./data")){dir.create("./data",showWarnings = FALSE)}

# 1c. Download and unzip the data - download method conditional on operating system
if(Sys.info()["sysname"]=="Windows"){setInternet2(use = TRUE)}
if(!file.exists("./data/data.zip"))
{
  ifelse(Sys.info()["sysname"]=="Windows",
         download.file(url=url,"./data/data.zip",quiet=TRUE),
         download.file(url=url,"./data/data.zip",method="curl",quiet=TRUE)
  )
  unzip("./data/data.zip",overwrite=TRUE,exdir="./data")
}

# 1d. Read in the data
data<-read.csv("./data/activity.csv",
               header=T,
               stringsAsFactors=F,
               colClasses=c("numeric",rep("character",2)))

################################################################################
# Step 2 - Process the data
################################################################################
# 2a. Using the lubridate package, format the date as such
data$date<-ymd(data$date)
# 2b. Create a new variable that lists the day of the week
data$day<-wday(data$date,label=T,abbr=F)
# 2c. Create a factor variable for weekday/weekend categorisation
data$week<-factor(x=ifelse(data$day %in% c("Saturday","Sunday")
                           ,"Weekend"
                           ,"Weekday")
                  ,levels=c("Weekend","Weekday")
)
# 2d. Create a new field, "time" that transforms the "interval" field in to the appropriate time
data$time<-ifelse(nchar(data$interval)==1,
                  paste("00:00:0",data$interval,sep=""),
                  ifelse(nchar(data$interval)==2,
                         paste("00:00:",data$interval,sep=""),
                         ifelse(nchar(data$interval)==3,
                                paste("0",substr(data$interval,1,1),":",substr(data$interval,2,3),":00",sep=""),
                                ifelse(nchar(data$interval)==4,
                                       paste(substr(data$interval,1,2),":",substr(data$interval,3,4),":00",sep=""),
                                       paste("")))))

################################################################################
# Step 3 - Summarising and reporting activity by day
################################################################################
# 3a. Using the dplyr package, summarise by day, calculating total steps
day_summary<-data %.%
  select(-time) %.%
  group_by(date) %.%
  summarise(total_steps=sum(na.omit(steps)))

# 3b. Make the histogram
png("./figures/figure1.png",width=720,height=720)
ggplot(day_summary,aes(x=na.omit(total_steps)))+
  geom_histogram(binwidth=1000,fill="steelblue",color="grey")+
  theme_minimal()+
  xlab("Total Steps Per Day (Intervals of 1000)")+
  ylab("Count of Days")+
  ggtitle("Histogram of Total Steps Taken Per Day")
dev.off()

# 3c. Calculate the median and mean total steps per day - used for reporting
options(scipen=100)
median_steps<-round(median(day_summary[,2]))
mean_steps<-round(mean(day_summary[,2]))


################################################################################
# Step 4 - Summarising and reporting activity by interval
################################################################################
# 4a. Summarise the data by interval
interval_summary<-data.frame(
  data %.%
    group_by(time) %.%
    summarise(average_steps=median(na.omit(steps))))
# 4b. Format time as time
interval_summary$time<-parse_date_time(interval_summary$time,orders="H:M:S")

# 4c. Create a row number than can be used as an index
interval_summary$row<-seq(1:nrow(interval_summary))
# 4d. Find the first time steps were recorded
first_data<-min(filter(interval_summary,average_steps!=0)$row)
# 4e. Find the last time steps were recorded
last_data<-max(filter(interval_summary,average_steps!=0)$row)
# 4f. Clean up the row number
interval_summary<-select(interval_summary,-row)

# plot line graph of average steps per interval
png("./figures/figure2.png",height=720,width=720)
ggplot(interval_summary[(first_data-12):(last_data+12),], # +/- 1 hour each end
       aes(x=time,y=average_steps,group=1))+
  geom_line(color="steelblue")+
  xlab("Time")+
  ylab("Average Steps")+
  ggtitle("Average steps per 5 minute interval accross the day")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


################################################################################
# Step 5 - Imputing missing values
################################################################################
# 5a. Calculate number and proportion of missing values
missing_values<-sum(is.na(data$steps))
total_rows<-nrow(data)
prop<-missing_values/total_rows
prop<-percent(prop)

# 5b. Recreate interval summary but leave time as character for the join:
interval_summary2<-data.frame(
  data %.%
    group_by(time) %.%
    summarise(average_steps=median(na.omit(steps))))
# 5c. Perform the join
data2<-sqldf("SELECT
             data.*
             ,coalesce(data.steps,med.average_steps) as clean_steps
             FROM
             data as data
             LEFT JOIN
             interval_summary2 as med
             ON
             data.time = med.time"
)
# 5d. Clean up the interval summary as it's no longer needed
rm(interval_summary2)

# 5e. Perform summarisation by day
day_summary_clean<- data2 %.%
  select(-time) %.%
  group_by(date) %.%
  summarise(total_steps=sum(na.omit(steps)))

# 5f. Calculate the new values
options(scipen=100)
median_steps_clean<-round(median(day_summary_clean[,2]))
mean_steps_clean<-round(mean(day_summary_clean[,2]))
# 5g. Assess the differences - used for reporting later
med_diff<-diff(c(median_steps,median_steps_clean))
mean_diff<-diff(c(mean_steps,mean_steps_clean))
med_diff_text<-if(med_diff>0){"greater"}else if(med_diff<0){"less"}else{"different"}
mean_diff_text<-if(mean_diff>0){"greater"}else if(mean_diff<0){"less"}else{"different"}


# 5h. Create histogram of daily activity with imputed values
png("./figures/figure3.png",width=720,height=720)
ggplot(day_summary_clean,aes(x=na.omit(total_steps)))+
  geom_histogram(binwidth=1000,fill="firebrick",color="grey")+
  theme_minimal()+
  xlab("Total Steps Per Day (Intervals of 1000)")+
  ylab("Count of Days")+
  ggtitle("Histogram of total steps per day, with imputed values for missing data")
dev.off()


################################################################################
# Step 6 - Analysing differences in weekdays and weekends
################################################################################
# 6a. Summary that can be split by weekend
interval_summary_weeks<- data.frame(
  data2 %.%
    group_by(week, time) %.%
    summarise(average_steps=median(na.omit(steps))))

# 6b. Subset to weekend data
weekend<-interval_summary_weeks %.% filter (week=="Weekend")
# Format time as time
weekend$time<-parse_date_time(weekend$time,orders="H:M:S")
# Find the first and last rows (i.e. times) that steps are recorded
weekend$row<-seq(1:nrow(weekend))
first_data_weekend<-min(filter(weekend,average_steps!=0)$row)
last_data_weekend<-max(filter(weekend,average_steps!=0)$row)
weekend<-select(weekend,-row)

# 6c. Construct the plot for weekend data
weekend_plot<-
  ggplot(weekend[(first_data_weekend-12):(last_data_weekend+12),],
         aes(x=time,y=average_steps,group=1))+
  geom_line(color="darkgreen")+
  xlab("Time")+
  ylab("Avg. Steps")+
  ggtitle("Weekends")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6d. Subset to week data
week<-interval_summary_weeks %.% filter (week=="Weekday")
# Format time as time
week$time<-parse_date_time(week$time,orders="H:M:S")
# Find the first and last rows (i.e. times) that steps are recorded
week$row<-seq(1:nrow(week))
first_data_week<-min(filter(week,average_steps!=0)$row)
last_data_week<-max(filter(week,average_steps!=0)$row)
week<-select(week,-row)

# 6e. Construct the plot for the week data
week_plot<-
  ggplot(week[(first_data_week-12):(last_data_week+12),],
         aes(x=time,y=average_steps,group=1))+
  geom_line(color="firebrick")+
  xlab("Time")+
  ylab("Avg. Steps")+
  ggtitle("Weekdays")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6f. Create the plot
png("./figures/figure4.png",width=720,height=720)
grid.arrange(week_plot,
             weekend_plot,
             ncol=1,
             nrow=2,
             main="Average Steps Per 5 Minute Interval")
dev.off()
