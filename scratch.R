readZipCSV <- function(zipFilePath, ...){
   # Function extracts a csv file in a zip file assuming the same name. 
   # Get the file name without extension
   fileNameNoExt = gsub(pattern = "(.*)\\..*$", "\\1", basename(zipFilePath))
   read.csv(unz(zipFilePath, paste(fileNameNoExt, ".csv", sep="")))
}
activity = readZipCSV("./activity.zip", row.names=null)

library(dplyr)
activityByDay <- activity %>%
   group_by(date) %>%
   summarize(
      sumSteps=sum(steps)
   ) %>%
   select(date, sumSteps)

activityByInterval <- activity %>%
   group_by(interval) %>%
   summarize(meanSteps=mean(steps, na.rm=TRUE))

activityWithMeans <- left_join(activity, activityByInterval, by="interval")

idxNa = which(is.na(activityWithMeans$steps))
activityWithMeans[idxNa, ]$steps = round(activityWithMeans[idxNa, ]$meanSteps)

gsub("Sunday|Saturday", "Weekend", weekdays(as.Date(activityWithMeans$date)))