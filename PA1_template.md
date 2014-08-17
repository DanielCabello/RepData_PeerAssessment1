REPRODUCIBLE RESEARCH: assignment 1  
===================================
  
  


## 1. Loading and preprocessing the Activity Monitoring Data   



**1.1 Downloading data file from repository**


```r
#fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(fileUrl,destfile="./activity.zip")
#unzip("activity.zip")
```
  
  

**1.2 Reading data as dataframe**


```r
activity <- read.table("activity.csv", header = TRUE, sep = ",", 
                       colClasses = c("numeric","Date","integer"))
```
  
  


## 2. Mean Total number of steps taken per day  


**2.1 Removing all observations with missing values (NA) in the variable 'steps'** 


```r
activityNA <- activity[!is.na(activity$steps),]
```


**2.2 Calculating total number of steps per day**


```r
stepsDayRemove <-as.numeric(tapply(activityNA$steps, activityNA$date, sum))
```


**2.3 Histogram of the total number of steps taken each day**


```r
hist(stepsDayRemove, col = "blue", breaks=30, 
     xlab="Steps per day", 
     ylab="Number of days", 
     main="Total number of steps taken each day",
     )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


**2.4 Mean and median total number of steps taken per day**


```r
meanRemove <- mean(stepsDayRemove)
medianRemove <- median(stepsDayRemove)
```

Mean


```
## [1] 10766
```

Median


```
## [1] 10765
```
   


## 3. Average daily activity pattern


**3.1 Calculating average number of steps taken per interval, averaged across  all days**


```r
ySteps<-(tapply(activityNA$steps, activityNA$interval, mean))
xInterval<- as.numeric(rownames(ySteps))
StepsInterval<-as.data.frame(cbind(xInterval,ySteps))
    row.names(StepsInterval)<- NULL
    colnames(StepsInterval)<- c("interval","steps")
```


**3.2 Time series plot of the average number of steps taken, averaged across all day, in 5-minute intervals** 


```r
plot(xInterval, ySteps, 
     type = "l",
     col = "blue", 
     xlab = "5-minute intervals in a day", 
     ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


**3.3 5-minute interval that, on average, contains the maximum number of steps (averaged across all days)**


```r
intervalMax <- StepsInterval[StepsInterval$steps == max(StepsInterval$steps),]
print(intervalMax)
```

```
##     interval steps
## 104      835 206.2
```



## 4. Imputing missing values

     
the strategy to impute the missing values is to use the average number of steps taken, averaged across all day, in 5-minute intervals obtained in the section 3.1   

     
**4.1 Total number of missing values (NA) in the dataset**


```r
na <- sum(is.na(activity$steps))
print(na)
```

```
## [1] 2304
```
    

**4.2 Filling in all observations with missing values (NA) with the means for each 5-minute interval**




```r
activityFillNA <- activity 
observations <- length(activityFillNA[,1])

for (i in 1:observations) {
        if (is.na(activityFillNA[i,1]) == TRUE) {
        text <- round(StepsInterval[StepsInterval$interval == activityFillNA[i,3],2],0)
        activityFillNA[i,1]<- text
        }
    }    
rm(i)
```
  

**4.3 Calculating total number of steps per day**


```r
stepsDayFill <- as.numeric(tapply(activityFillNA$steps, activityFillNA$date, sum))
```


**4.4 Histogram of the total number of steps taken each day**


```r
par(mfcol = c(2,1))

hist(stepsDayFill, col = "blue", breaks=30, 
         xlab="Steps per day", 
         ylab="Frecuencia", 
         main="Total number of steps taken each day (imputing missing values)",
    )

hist(stepsDayRemove, col = "blue", breaks=30, 
     xlab="Steps per day", 
     ylab="Number of days", 
     main="Total number of steps taken each day (imputing missing values)",
     )
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```r
par(mfcol = c(1,1))
```

As can be seen in the graphs above, the average number of steps is greater when the missing values are removed than when they are imputed with the average for that interval. Both graphs show a normal distribution, with mean and median almost identical.


**4.5 Mean and median of the total number of steps taken per day**


```r
meanFill <- mean(stepsDayFill)
medianFill <- median(stepsDayFill)
```

The values of mean and median are almost the same.


Mean


```
## [1] 10766
```

Median


```
## [1] 10762
```
   
   

**4.6 Comparative table of the mean and median of the total number of steps taken per day: removing and filling missing values (NA)**


```r
comparativeTable <- data.frame(RemovingNA = c(meanRemove,medianRemove),
                    ImputingNA = c(meanFill, medianFill), 
                    row.names = c("mean","median"))

print(comparativeTable)
```

```
##        RemovingNA ImputingNA
## mean        10766      10766
## median      10765      10762
```

The means are the same while medians differ little between them


## 5. Differences in activity patterns between weekdays and weekends


**5.1 Creating a new factor variable ('weekday') in the dataset with two levels: "weekday" and "weekend"**

It has been used database with missing values imputed (activityFillNA), generated in the section 4.


```r
observations <- length(activityFillNA[,1])
weekday <- NULL

for (i in 1:observations) {
    ifelse(weekdays(activityFillNA[i,2]) == "Saturday" | weekdays(activityFillNA[i,2]) == "Sunday",
       text <- "weekend", text<-"weekday")

    weekday <- c(weekday, text)
}

activityFillNA <- cbind(activityFillNA,weekday)

rm(i, observations, weekday)
```


**5.2 Calculating average number of steps taken grouped by weekday (weekday and weekend) and interval**


```r
activity2 <- tapply(activityFillNA$steps, paste(activityFillNA$weekday, activityFillNA$interval, sep=","), mean)
activity2 <- as.data.frame(activity2, stringsAsFactors = FALSE)   

groups <- as.character(row.names(activity2)) 
groups <- strsplit(groups, split = ",") 
groups <- t(as.data.frame(groups,stringsAsFactors = FALSE)) 

activity2 <- data.frame(groups,activity2,stringsAsFactors = FALSE)

colnames(activity2) <- c("weekday", "interval", "steps")

activity2$interval <- as.integer(activity2$interval)
```


**5.3 Time series plot of the average number of steps taken, averaged across all day, in 5-minute intervals for weekday and weekend**


```r
library(ggplot2)
qplot(interval, steps, data = activity2, 
        facets = weekday~., 
        geom = c("line"), 
        se=FALSE, 
        xlab = "5-minute intervals", 
        ylab = "Number of steps", 
        )
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 
