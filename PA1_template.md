---
title: "Reproducible Research: Peer Assessment 1"
author: "John Goodwin"
output:
  html_document: 
    keep_md: true
---


## Loading and preprocessing the data


```r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.2     v dplyr   0.7.4
## v tidyr   0.7.2     v stringr 1.2.0
## v readr   1.1.1     v forcats 0.2.0
```

```
## -- Conflicts ---------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```


```r
unzip("activity.zip")

activity <- read_csv('activity.csv')
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

```r
file.remove('activity.csv') ##clean up
```

```
## [1] TRUE
```

Let's make more detailed time variables. I am going to turn the interval column into a more attractive date field and extract the day of the week from the date column.


```r
activity <- activity %>%
  mutate(time = gsub('^([0-9]{2})([0-9]+)$', '\\1:\\2', 
                     str_pad(interval,4,'left','0')),
         weekday = weekdays(date))
```
And here is a look at a sample of the final dataset:


```r
activity %>% sample_n(20)
```

```
## # A tibble: 20 x 5
##    steps date       interval time  weekday  
##    <int> <date>        <int> <chr> <chr>    
##  1     0 2012-11-22     2205 22:05 Thursday 
##  2    NA 2012-10-08     1525 15:25 Monday   
##  3     0 2012-10-05     2355 23:55 Friday   
##  4     0 2012-10-11     1440 14:40 Thursday 
##  5     0 2012-11-24      225 02:25 Saturday 
##  6    NA 2012-11-01     1925 19:25 Thursday 
##  7     0 2012-11-15      820 08:20 Thursday 
##  8     0 2012-10-15     1115 11:15 Monday   
##  9   190 2012-11-25      915 09:15 Sunday   
## 10    NA 2012-11-01     1020 10:20 Thursday 
## 11     0 2012-10-16     2315 23:15 Tuesday  
## 12     0 2012-10-25      535 05:35 Thursday 
## 13    NA 2012-10-08     2200 22:00 Monday   
## 14     0 2012-10-25     1150 11:50 Thursday 
## 15    NA 2012-11-01      410 04:10 Thursday 
## 16    36 2012-10-21     1320 13:20 Sunday   
## 17     0 2012-10-03     2305 23:05 Wednesday
## 18    NA 2012-11-10     1925 19:25 Saturday 
## 19    NA 2012-10-08      700 07:00 Monday   
## 20     0 2012-10-19     1615 16:15 Friday
```


## What is mean total number of steps taken per day?


```r
daily_tot_steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
```


```r
ggplot(daily_tot_steps, aes(total_steps)) + 
  geom_histogram(bins = 15, fill = 'white', color = 'black') +
  theme_bw() +
  ggtitle('Histogram of Total Steps Per Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
mean_steps <- mean(daily_tot_steps$total_steps)
med_steps <- median(daily_tot_steps$total_steps)
```

The mean number of steps taken per day is 10766.19 steps.
The median number of steps taken per day is 10765 steps.

## What is the average daily activity pattern?


```r
int_avg_steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(time) %>%
  summarise(avg_steps = mean(steps)) %>% 
  ungroup %>%
  mutate(time_factor = 1:nrow(.)) 

max_avg <- int_avg_steps %>%
  filter(avg_steps == max(avg_steps))

int_avg_steps$time <- as_datetime(strptime(int_avg_steps$time, "%H:%M", tz = 'UTC'))

ggplot(int_avg_steps, aes(time, avg_steps)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H:%M") +
  theme_bw() +
  ggtitle('Average Steps by Time Interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The time interval with the maximum average number of steps is at 08:35 with 206.17 steps on average.

## Imputing missing values

There are 2304 missing steps records.

When are there missing steps by date, and how many steps records are missing at those dates?


```r
activity %>%
  group_by(date) %>%
  summarise(na_count = sum(is.na(steps))) %>%
  ggplot(aes(date, na_count)) +
  geom_point()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Looks like a few dates are just missing entirely--all 288 time intervals are missing at a few dates. 

Let's quickly fit a smoothed curve to the average steps time series and use that to replace NAs.

I am going to use a LOESS curve with degree two. In order to find a good bandwidth we need to look at a few of the potential options:


```r
bw<-seq(.1, .9, by = .1)

loess_pred <- purrr::map_df(seq_len(9), ~int_avg_steps[,c(3,2)])

loess_pred$bw <- replicate(288, bw) %>% as.vector %>% sort

pred <- NULL

for (i in 1:9) {
  
  curve <- loess(avg_steps ~ time_factor, data= int_avg_steps, span = bw[i])
  
  pred <- c(pred,predict(curve))
  
}

loess_pred$pred <- pred

loess_pred %>%
  mutate(bw = factor(bw)) %>%
  ggplot(aes(x = time_factor)) +
  geom_point(aes(y = avg_steps), alpha = .4, color = 'grey') +
  geom_line(aes(y=pred)) +
  facet_wrap(~bw, ncol = 3 ) +
  theme_bw() +
  ggtitle("LOESS Curve Fit by Bandwidth")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

It looks like a span of of 0.3 picks up on some of the large spikes without overfitting. Now let's use that curve to fill in the missing values. It would probably be better practice to use cross validation to choose a span, but this is just a quick fit!


```r
avg_steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(time) %>%
  summarise(avg_steps = mean(steps)) %>% 
  ungroup %>%
  mutate(time_factor = 1:nrow(.))

loess_steps <- loess(avg_steps ~ time_factor, data = avg_steps, span = 0.3)
predictions <- predict(loess_steps)

smooth_steps <- avg_steps %>% 
  mutate(smoothed_avg_steps = predictions) %>%
  select(time, smoothed_avg_steps)

activity_nas_replaced <- activity %>% left_join(smooth_steps, by = "time") %>%
  mutate(steps = coalesce(as.double(steps), smoothed_avg_steps))
```

Now that we have replaced the missing values, lets see how things have changed.


```r
daily_tot_steps <- activity_nas_replaced %>% group_by(date) %>%
  summarise(total_steps = sum(steps))

ggplot(daily_tot_steps, aes(total_steps)) + 
  geom_histogram(bins = 15, fill = 'white', color = 'black') +
  theme_bw() +
  ggtitle('Histogram of Total Steps Per Day After Replacing Missing')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean_steps <- mean(daily_tot_steps$total_steps)
med_steps <- median(daily_tot_steps$total_steps)
```

After replacing the missing values, the mean number of steps taken per day is 10764.37 steps and the median number of steps taken per day is 10752.35 steps. It doesn't look like the results have changed too much!

## Are there differences in activity patterns between weekdays and weekends?

Using that weekday variable we created earlier, let's create an indicator of whether the observed value occured on a weekday or weekend.


```r
activity <- activity %>%
  mutate(part_of_week = if_else(weekday %in% c('Saturday', 'Sunday'), 
                                'Weekend', 'Weekday'))

weekend_ts_data <- activity %>%
  group_by(part_of_week, time) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE)) %>%
  ungroup

weekend_ts_data$time <- as_datetime(strptime(weekend_ts_data$time, "%H:%M", tz = 'UTC'))

ggplot(weekend_ts_data, aes(time, avg_steps)) +
  geom_line() +
  facet_wrap(~part_of_week) +
  scale_x_datetime(date_labels = "%H:%M") +
  theme_bw() +
  ggtitle('Average Steps by Time Interval and Part of Week')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

It looks like there may be a slight difference between the step patterns during the weekdays compared to weekends. The weekend results are volatile, but it would appear that the steps are higher on average and evenly distributed throughout the middle of the day. During the week, the steps appear to be lower on average and are more concentrated early in the day.
