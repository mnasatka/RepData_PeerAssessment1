library(ggplot2)

if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
activity <- read.csv("activity.csv", colClasses = c("numeric","Date","numeric"))

steps.total <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
mean(steps.total)
median(steps.total)

##print(qplot(totsteps, xlab='Total steps', ylab='Frequency'))

steps.int.mean <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
pattern <- data.frame(time=as.numeric(names(steps.int.mean)),steps.int.mean=steps.int.mean)
print(ggplot(pattern, aes(x=time, y=steps.int.mean)) + geom_line(aes(group=1)) +
        xlab('Interval') + ylab('Mean number of steps') +   
        scale_x_continuous(breaks=c(seq(0,2500,by=500))))

most <- which.max(pattern$steps.int.mean)
print(sprintf("The interval with the highest mean number of steps is: %3.0f", pattern[most,'time']))

print(summary(activity$steps))