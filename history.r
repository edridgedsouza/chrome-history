##My personal Google Chrome history analysis
##Data file created 6/6/16. File not included.

library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(ggthemes)
library(Cairo)

round_minute <- function(x,precision){  #Credit for this function goes to https://stackoverflow.com/questions/16803867/round-a-date-in-r-to-an-arbitrary-level-of-precision
  m <- minute(x)+second(x)/60
  m.r <- round(m/precision)*precision
  minute(x) <- m.r
  second(x) <- 0
  x
}

setwd("C:\\Users\\edridge\\Downloads\\Kaggle\\History")   #Sorry, not including my original CSV file for privacy reasons.
history <- read.csv("my-history.csv")                     #However, instructions on how to make your own are at https://superuser.com/questions/602252/can-chrome-browser-history-be-exported-to-an-html-file
#This file, for whatever reason, didn't include my actual whole history, which goes back to late 2014. 
#It includes everything from March 2016 until 6/6/16, as well as one page from 2015 and some bookmark files stored as if they were from 1960.

names(history) <- c("Time", "url")


history$ParseTime <- ymd_hms(history$Time)
history$Date <- date(history$ParseTime)
history$Hour <- sapply(as.character(history$Time), substr, 12, 1000)
history$RoundHour <- strptime(history$Hour, "%H:%M:%S") %>% round_minute(15) %>% as.character.Date()

# `sum` gives you total activity per day over time.
sum <- history %>%
  filter(month(Date) > 2 & year(Date) >= 2016) %>%
  group_by(Date) %>%
  summarize(activity = length(Date)) %>%
  ungroup()

# `freq` gives you total activity over time for any given time of the day, binned by 15 minute intervals
freq <- history %>%
  filter(month(Date) > 2 & year(Date) >= 2016) %>%
  group_by(RoundHour) %>%
  summarize(frequency = length(RoundHour)) %>%
  ungroup()
freq$RoundHour <- ymd_hms(freq$RoundHour)
names(freq) <- c("Hour", "Activity")


# Create a plot for `sum`
CairoPNG(filename = "ActivityOverTime.png", height=500, width=1000) # svg() or CairoWin(), optionally
ggplot(sum, aes(Date,activity)) + 
  geom_line(aes(Date,activity), color = "#FF5050", size=1.5) + 
  geom_smooth() + 
  theme_fivethirtyeight()
dev.off()


# Create a plot for `freq`. This is honestly more informative than the `sum` plot.
CairoPNG(filename="WebUsage.png", height=500, width=1000)
usagePlot <- ggplot(freq, aes(Hour, Activity)) + 
  geom_line(color="#FF5050", size=1.5, alpha=0.7) + 
  geom_point() + 
  geom_smooth(alpha=0.5) +
  theme_fivethirtyeight() +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
  ggtitle("Total Internet usage since March")
usagePlot
dev.off()

### These are my first two exploratory analyses. More will come in the future when I get time to analyze this file in more depth.
