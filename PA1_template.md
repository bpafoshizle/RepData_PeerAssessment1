# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
readZipCSV <- function(zipFilePath, ...){
  # Function extracts a csv file in a zip file assuming the same name. 
  # Get the file name without extension
  fileNameNoExt = gsub(pattern = "(.*)\\..*$", "\\1", basename(zipFilePath))
  read.csv(unz(zipFilePath, paste(fileNameNoExt, ".csv", sep="")))
}
activity = readZipCSV("./activity.zip")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
