# Stepping Through Time
### An analysis of personal movement data
##### Jim Leach 
*******************************************************************************

### Report Overview
This report has been created for an assignment set as part of the *Reproducible Research* MOOC from Johns Hopkins university on [Coursera](https://class.coursera.org/repdata-004). 

The goal was to take two month's of pedometer data for an anonymous individual and perform some brief exploratory analysis using R. 

The following points had to be addressed:

* What is the distribution of total daily steps?
* What is the distribution of steps per 5-minute interval within a day?
* How much of the data is missing?
* Does the distribution of total daily steps change if missing values are imputed?
* What is the difference in activity between weekdays and weekends?

Finally, it was a requirement to produce the analysis in a *reproducible* manner.This means that, given the same data and software, anyone could reproduce the analysis.

This report will address each of these points and is structured as follows:

* An overview of why this analysis is _reproducible_.
* An overview of the analysis methods.
* A presentation of the results of the analysis.

#### Reproducible Analysis

There are a number of factors that demonstrate the *reproducibility* of this report:

* Package installation and loading is conditional - packages are installed and loaded as required.
* The code contains steps to download and read in data. This limits manual steps.
* The code is OS-neutral: wherever possible, it uses conditional logic to ensure compatibility with a range of operating systems.
* Results are generated and stored to variables or external files automatically, so can be recalled later.
* This report has written using RMarkdown and will automatically generate figures and report results - nothing has been hard-coded.


*******************************************************************************
### Methodology

The methodology of the analysis was to break the code up in to discrete sections. These steps were:

1. Obtain and load the data;
2. Process the data;
3. Calculate and chart daily average steps;
4. Calculate and chart the average steps in 5 minute intervals throughout the day;
5. Impute missing values in the data set and create a new, clean data set and repeat step 3 for the cleaned data; and
6. Analyse differences in activity between weekdays and weekends.


The following sections set out the detailed approach for each stage of the analysis.

##### Step 0 - Preparing the R working environment - Packages

A number of packages were used and are therefore required for this analysis. These packages were:

* [lubridate](http://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/)
* [dplyr](http://blog.rstudio.org/2014/01/17/introducing-dplyr/)
* [ggplot2](http://ggplot2.org/)
* [sqldf](http://www.r-bloggers.com/manipulating-data-frames-using-sqldf-a-brief-overview/)
* [gridExtra](http://www.r-bloggers.com/extra-extra-get-your-gridextra/)
* [scales](http://cran.r-project.org/web/packages/scales/index.html)

Conditional installation and subsequent loading of these packages was achieved with:

    if(require("package name here",quietly=T)){
      print("loading "package name here"")
    } else {
      print("trying to install "package name here"")
      install.packages(""package name here"")
      library("package name here",quietly=T)
      print(""package name here" installed and loaded")
      if(require("package name here",quietly=T)){
        } else {
        stop("could not install "package name here"")
      }
    }
    
For more information about any of these packages, see their documentation using `help(package="package name here")`.
        



##### Step 1 - Getting and loading the data

The data for this analysis are stored [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip). The download and loading of the data in to R was automated:


```r
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
```

##### Step 2 - Process the data

The data read in to R were cleaned to facilitate later processing.

```r
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
```

##### Step 3 - Summarising and reporting activity by day

The data were summarised by day.This was done using the fantastic dplyr package:


```r
# 3a. Using the dplyr package, summarise by day, calculating total steps
day_summary<-data %.%
             select(-time) %.%
             group_by(date) %.%
             summarise(total_steps=sum(na.omit(steps)))
```

The median and mean total steps per day were calculated:

```r
# 3c. Calculate the median and mean total steps per day
options(scipen=100)
median_steps<-round(median(day_summary[,2]))
mean_steps<-round(mean(day_summary[,2]))
```

See the results section for these numbers.

##### Step 4 - Summarising and reporting activity by interval

The initial summarisation on time was again done using dplyr. The time interval was then converted to a time data-type, using the lubridate package.

```r
# 4a. Summarise the data by interval
interval_summary<-data.frame(
                  data %.%
                  group_by(time) %.%
                  summarise(average_steps=median(na.omit(steps))))
# 4b. Format time as time
interval_summary$time<-parse_date_time(interval_summary$time,orders="H:M:S")
```

As part of the working for this analysis, it was found that there were significant portions of time where no steps were recorded. To facilitate a cleaner graph, these records were ignored. This was achieved by finding the first and last time steps were recorded:

```r
# 4c. Create a row number than can be used as an index
interval_summary$row<-seq(1:nrow(interval_summary))
# 4d. Find the first time steps were recorded
first_data<-min(filter(interval_summary,average_steps!=0)$row)
# 4e. Find the last time steps were recorded
last_data<-max(filter(interval_summary,average_steps!=0)$row)
# 4f. Clean up the row number
interval_summary<-select(interval_summary,-row)
```

Subsequently, the five minute interval that contained, on average, the most steps of any interval across the day was calculated:

```r
max_interval<-filter(interval_summary,average_steps==max(average_steps))
interval<-substring(as.character(max_interval[,1]),11)
steps<-max_interval[,2]
rm(max_interval)
```
This value is reported in the results section below.

##### Step 5 - Imputing missing values

When reading in data, it is often sensible to get an idea of the number of missing values in the variable(s) of interest. In this analysis, it was the steps per interval that was of interest.

The total number of missing values, and the proportion of the total that this represents were calculated:

```r
# 5a. Calculate number and proportion of missing values
missing_values<-sum(is.na(data$steps))
total_rows<-nrow(data)
prop<-missing_values/total_rows
prop<-percent(prop)
```

The total number of missing values in the steps data is 2304. This represents 13.1% of the data. 

A granular approach was taken to impute missing values. The median value for a given five minute interval was used to replace missing values in the data. 

This was done using the sqldf package, specifically the *coalesce* function. Coalesce takes the first non-NULL value in a vector of values. It is highly useful for filling in missing data.

The median value of steps for a given 5 minute interval was joined to the original data, and *coalesce* used to replace missing values.

```r
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
# 5d. Remove the second interval summary as it's no longer needed
rm(interval_summary2)
```

Values for the median and mean steps using the imputed data set were calculated. This was done with the same day-wise summary performed previously:

```r
# 5e. Perform summarisation by day
day_summary_clean<- data2 %.%
                    select(-time) %.%
                    group_by(date) %.%
                    summarise(total_steps=sum(na.omit(steps)))
```

The median and mean values for daily steps were recalculated using the imputed data:

```r
# 5f. Calculate the new values
options(scipen=100)
median_steps_clean<-round(median(day_summary_clean[,2]))
mean_steps_clean<-round(mean(day_summary_clean[,2]))
# 5g. Assess the differences
med_diff<-diff(c(median_steps,median_steps_clean))
mean_diff<-diff(c(mean_steps,mean_steps_clean))
med_diff_text<-if(med_diff>0){"greater than"}else if(med_diff<0){"less than"}else{"different to"}
mean_diff_text<-if(mean_diff>0){"greater than"}else if(mean_diff<0){"less than"}else{"different to"}
```

##### Step 6 - Analysing differences in weekdays and weekends

The final step of this analysis was to plot the average steps per 5 minute interval across the day, split by weekends and weekdays. 

The weekend/weekday factor was already created and is present in the imputed data set that was used for this step. 

A further summary by interval was performed, including the weekday/weekend factor into the grouping.


```r
# 6a. Summary that can be split by weekend
interval_summary_weeks<- data.frame(
                        data2 %.%
                        group_by(week, time) %.%
                        summarise(average_steps=median(na.omit(steps))))
```

Two new data sets were then created, the first containing step data for weekends only:

```r
# 6b. Subset to weekend data
weekend<-interval_summary_weeks %.% filter (week=="Weekend")
# Format time as time
weekend$time<-parse_date_time(weekend$time,orders="H:M:S")
# Find the first and last rows (i.e. times) that steps are recorded
weekend$row<-seq(1:nrow(weekend))
first_data_weekend<-min(filter(weekend,average_steps!=0)$row)
last_data_weekend<-max(filter(weekend,average_steps!=0)$row)
weekend<-select(weekend,-row)
```

The second data set contained step data for weekdays only:

```r
# 6d. Subset to week data
week<-interval_summary_weeks %.% filter (week=="Weekday")
# Format time as time
week$time<-parse_date_time(week$time,orders="H:M:S")
# Find the first and last rows (i.e. times) that steps are recorded
week$row<-seq(1:nrow(week))
first_data_week<-min(filter(week,average_steps!=0)$row)
last_data_week<-max(filter(week,average_steps!=0)$row)
week<-select(week,-row)
```

Finally, these two plots were brought together using the gridExtra package to create the final plot to assess differences in weekend and weekday activity. This plot can be seen as figure 4 in the results section below.

*******************************************************************************


### Results

#### Day-Wise Analysis with *Raw* Data:
##### Figure 1 - Distribution of total steps per day

```r
# 3b. Make the histogram
ggplot(day_summary,aes(x=na.omit(total_steps)))+
geom_histogram(binwidth=1000,fill="steelblue",color="grey")+
theme_minimal()+
xlab("Total Steps Per Day (Intervals of 1000)")+
ylab("Count of Days")+
ggtitle("Histogram of Total Steps Taken Per Day")
```

![plot of chunk figure1](./PA1_template_files/figure-html/figure1.png) 


Figure 1 shows that the majority of days have total steps of between approximately 7,500 and 15,000 steps - an impressive count!

It's also interesting to note the number of days with a very low count (0 to 1000) of steps - it would be interesting to know if this is due to measurement error, or just a few lazy days.

Further, the computed median number of total steps per day is *10395*. Similarly, the computed mean number of total steps taken per day is *9354*.

#### Interval-Wise Analaysis with *Raw* Data:
##### Figure 2 - Distribution of median steps within a day

```r
# plot line graph of average steps per interval
ggplot(interval_summary[(first_data-12):(last_data+12),], # +/- 1 hour each end
       aes(x=time,y=average_steps,group=1))+
  geom_line(color="steelblue")+
  xlab("Time")+
  ylab("Average Steps")+
  ggtitle("Average steps per 5 minute interval accross the day")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![plot of chunk figure2](./PA1_template_files/figure-html/figure2.png) 

Figure 2 shows that there are two peaks of activity, one in the morning and one in the evening with a small spike of activity around lunch time. N.B. The data for the chart were subsetted using the first and last times with activity calculated above.

The most number of steps is recorded in the five minute interval ending at :45:00, where it appears that, on average, the participant took _60_ steps.

### Missing Data
The total number of missing values in the steps data is *2304*. This represents *13.1%* of the data. 

Once missing values had been imputed using median activity, the median steps per interval is *10395*. This is *0* steps different to the value using missing data.

Using the imputed data set, it is the case that the mean steps per interval is *9354*. This is *0* steps different to the value using missing data.

##### Figure 3 - Distribution of total steps per day using *imputed* data

```r
# 5h. Create histogram of daily activity with imputed values
ggplot(day_summary_clean,aes(x=na.omit(total_steps)))+
 geom_histogram(binwidth=1000,fill="firebrick",color="grey")+
 theme_minimal()+
 xlab("Total Steps Per Day (Intervals of 1000)")+
 ylab("Count of Days")+
 ggtitle("Histogram of total steps per day, with imputed values for missing data")
```

![plot of chunk figure3](./PA1_template_files/figure-html/figure3.png) 

### Interval-wise Analysis with *Imputed* Data - Weekends vs Weekdays
##### Figure 4 - Distribution of median steps within a day, weekends vs weekdays

```r
# Construct the plot for weekend data
weekend_plot<-
ggplot(weekend[(first_data_weekend-12):(last_data_weekend+12),],
       aes(x=time,y=average_steps,group=1))+
  geom_line(color="darkgreen")+
  xlab("Time")+
  ylab("Avg. Steps")+
  ggtitle("Weekends")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Construct the plot for the week data
week_plot<-
  ggplot(week[(first_data_week-12):(last_data_week+12),], 
         aes(x=time,y=average_steps,group=1))+
  geom_line(color="firebrick")+
  xlab("Time")+
  ylab("Avg. Steps")+
  ggtitle("Weekdays")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(week_plot,
             weekend_plot,
             ncol=1,
             nrow=2,
             main="Average Steps Per 5 Minute Interval")
```

![plot of chunk figure4](./PA1_template_files/figure-html/figure4.png) 



It is interesting to note the differences in activity between the weekends and weekdays. Overall, it appears that weekends have more activity, and that that activity is more distributed throughout the day. 

********************************************************************************

### References and Contact

The complete code and documentation for this assignment can be found on the [GitHub](https://github.com/Jim89/RepData_PeerAssessment1) repository. 

The author of this report can be contacted on [twitter](https://twitter.com/leach_jim).
