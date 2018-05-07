---
title: "Reproducible Research: Peer Assessment 1"
author: "John Goodwin"
output:
  html_document: 
    keep_md: true
---


## Loading and preprocessing the data




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

Let's make more detailed time variables. Since the interval variable is just the number of minutes, I am going to combine the date and interval fields into a complete datetime variable.  While we are messing around with dates, I am going to extract a prettier time of day field and add a weekday variable.


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
##  1     0 2012-10-24      415 04:15 Wednesday
##  2    15 2012-11-22     1450 14:50 Thursday 
##  3     0 2012-10-15     1625 16:25 Monday   
##  4    NA 2012-11-10      635 06:35 Saturday 
##  5     0 2012-10-16       35 00:35 Tuesday  
##  6    NA 2012-10-08      325 03:25 Monday   
##  7    NA 2012-11-04     1515 15:15 Sunday   
##  8     0 2012-11-08     1055 10:55 Thursday 
##  9     0 2012-11-02      230 02:30 Friday   
## 10     0 2012-10-27     2335 23:35 Saturday 
## 11     0 2012-10-02     1245 12:45 Tuesday  
## 12     0 2012-10-03      320 03:20 Wednesday
## 13    NA 2012-11-04      715 07:15 Sunday   
## 14     0 2012-11-22     2220 22:20 Thursday 
## 15   203 2012-10-28      805 08:05 Sunday   
## 16    NA 2012-11-30     1105 11:05 Friday   
## 17     0 2012-10-11     1720 17:20 Thursday 
## 18   413 2012-10-03      600 06:00 Wednesday
## 19    NA 2012-11-09     2230 22:30 Friday   
## 20     0 2012-11-24      720 07:20 Saturday
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
  summarise(avg_steps = mean(steps)) %>%
  ungroup

weekend_ts_data$time <- strptime(weekend_ts_data$time, "%H:%M", tz = 'UTC')

ggplot(weekend_ts_data, aes(time, avg_steps)) +
  geom_line() +
  facet_wrap(~part_of_week) +
  scale_x_datetime(date_labels = "%H:%M") +
  theme_bw() +
  ggtitle('Average Steps by Time Interval and Part of Week')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

It looks like there may be a slight difference between the step patterns during the weekdays compared to weekends. The weekend results are volatile, but it would appear that the steps are higher on average and evenly distributed throughout the middle of the day. During the week, the steps appear to be lower on average and are more concentrated early in the day.
