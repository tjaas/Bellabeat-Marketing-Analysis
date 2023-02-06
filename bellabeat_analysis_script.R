#load libraries
library(readr)
library(plyr)
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(plotly)
library(treemapify)

#set environment
setwd("~/R/BellaBeat_Case_Study/Data Capstone Project/Fitabase Data 4.12.16-5.12.16")

#read csv data
dailyActivity <- read.csv("dailyActivity_merged.csv")
dailySleep <- read.csv("sleepDay_merged.csv")
Intensities <- read.csv("hourlyIntensities_merged.csv")
calories <- read.csv("hourlyCalories_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
heartRate <- read.csv("heartrate_seconds_merged.csv")

#check data with glimpse
glimpse(dailyActivity)
glimpse(dailySleep)
glimpse(Intensities)
glimpse(weight)
glimpse(heartRate)

#covert date/time format for a standard
Intensities$ActivityHour=as.POSIXct(Intensities$ActivityHour, format="%m/%d/%Y %H:%M:%S %p", tz=Sys.timezone())
Intensities$time <- format(Intensities$ActivityHour, format = "%H:%M:%S")
Intensities$date <- format(Intensities$ActivityHour, format = "%m/%d/%y")

calories$ActivityHour = as.POSIXct(calories$ActivityHour, format = "%m/%d/%Y %H:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

dailyActivity$ActivityDate = as.POSIXct(dailyActivity$ActivityDate, format = "%m/%d/%Y", tz=Sys.timezone())
dailyActivity$date <- format(dailyActivity$ActivityDate, format = "%m/%d/%y")

#sleep
dailySleep$SleepDay = as.POSIXct(dailySleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
dailySleep$date <- format(dailySleep$SleepDay, format = "%m/%d/%y")

#First statisticcs
# activity
dailyActivity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# explore num of active minutes per category
dailyActivity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
dailySleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()

#Check what the devices are being used and for what purpose. We are grouping the dailyActivity, dailySleep and weight, to find distinct users, the frequency of use (days logged) and the distribuition. 

dailyActivity_Summary <- dailyActivity %>% 
  group_by(Id) %>% 
  dplyr::summarise(n = n(), steps = round(mean(TotalSteps)), calories = round(mean(Calories)),
            VeryActiveMinutes = round(mean(VeryActiveMinutes)), 
            FairlyActiveMinutes = round(mean(FairlyActiveMinutes)), 
            LightlyActiveMinutes = round(mean(LightlyActiveMinutes)),
            SedentaryMinutes = round(mean(SedentaryMinutes)))

#Group dailySleep by user
#avg time sleep and avg time in bed
dailySleep_Summary <- dailySleep %>% 
  group_by(Id) %>% 
  dplyr::summarise(n = n(), avgTimeAsleep = round(mean(TotalMinutesAsleep)),
            avgTimeInBed = round(mean(TotalTimeInBed)))

#Group weight by user
weight_Summary <- weight %>% 
  group_by(Id) %>% 
  dplyr::summarize(n = n(), avg_weight_kg = round(mean(WeightKg)),
            max_weight = max(WeightKg),
            min_weight = min(WeightKg), 
            averageBMI = mean(BMI))

n_distinct(dailyActivity_Summary$Id)
n_distinct(dailySleep_Summary$Id)
n_distinct(weight_Summary$Id)


#Assign the range/categories based on the number of days logged (n)
dailyActivity_Summary$Usage <- ifelse(dailyActivity_Summary$n <= 10, "Low", 
                                      ifelse(dailyActivity_Summary$n < 21, "Medium", "Regular"))   

dailySleep_Summary$Usage <- ifelse(dailySleep_Summary$n <=10, "Low",
                                   ifelse(dailySleep_Summary < 21, "Medium", "Regular"))

weight_Summary$Usage <- ifelse(weight_Summary$n <= 10, "Low",
                               ifelse(weight_Summary$n < 21, "Medium", "Regular"))

#Group users based on usage/days logged
dailyActivityUsage <- dailyActivity_Summary %>% 
  group_by(Usage) %>% 
  summarise(userCount = n(), average_usage = round(mean(n), 0))

dailySleepUsage <- dailySleep_Summary %>% 
  group_by(Usage) %>% 
  summarise(UserCount = n(), average_usage = round(mean(n), 0))

weightUsage <- weight_Summary %>% 
  group_by(Usage) %>% 
  summarise(UserCount = n(), average_usage = round(mean(n), 0))

#plot Activity Usage per category
ggplot(data = dailyActivityUsage, aes(x = Usage, y = userCount)) +
  geom_bar(stat="identity",position = "stack", color="black",fill="darkorange") +
  labs(title="Activity Feature Usage", x="Category", y="Count") +
  geom_label(aes(label = userCount),color="black")

#plot sleep Usage per category
ggplot(data = dailySleepUsage, aes(x = Usage, y = UserCount)) +
  geom_bar(stat="identity", color="black",fill="darkorange") +
  labs(title="Sleep Feature Usage", x="Category", y="Count") +
  geom_label(aes(label = UserCount),color="black")

#plot Weight Feature Usage per category
ggplot(data = weightUsage, aes(x = Usage, y = UserCount)) +
  geom_bar(stat="identity", color="black",fill="darkorange") +
  labs(title="Weight Feature Usage", x="Category", y="Count") +
  geom_label(aes(label = UserCount),color="black")


dailyActivity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, Calories) %>% 
  summary()

# We can check that the total of sedentary minutes is 1440 minutes or 24hrs. We agree that the device was not used. We can confirm as well that the total step are 0.
#We will filter this data out

dailyActivity <- dailyActivity %>% 
  filter(SedentaryMinutes != 1440 & TotalSteps != 0)

#run again the summary. The average total steps are 8329, which is below recommended average of 10000 steps by "CDC". 

#calculate the percentage that is below this recommended 10000 steps

percentage_usage_recommended <- round(nrow(dailyActivity_Summary[dailyActivity_Summary$steps < 10000,]) / nrow(dailyActivity_Summary) *100)

ggplot(data = dailyActivity_Summary, aes(x = Usage, y = steps)) + 
  geom_point() + 
  labs(title = "Total Steps by Usage Category", x = "Total Steps", y = "Usage Category") +
  geom_smooth(method = "lm") +
  geom_hline(mapping = aes(yintercept = 10000), color = "yellow", lwd = 2.0) +
  geom_text(mapping = aes(x = "Low", y = 11000, label = "10000 steps per day", srt = 0)) +
  geom_text(mapping = aes(x = "Low", y = 7000, label = "79% below yellow line")) +
  # geom_segment(aes(x = "Low", y = 6000, xend = "Medium", yend = 6000), arrow = arrow(length = unit(1.5, 'cm')), color = "orange") +
  annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = 10000, alpha = .2)


# ggplot(data = dailyActivity_Summary) + 
#    geom_point(mapping = aes(x = Usage, y = n)) + 
#    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 1))

#Sorting categories based on no. of users in each category
dailyActivityUsage <- dailyActivityUsage[order(-dailyActivityUsage$average_usage),]
dailySleepUsage <- dailySleepUsage[order(-dailySleepUsage$average_usage),]
weightUsage <- weightUsage[order(-weightUsage&average_usage),]

#Daily Activity Summary

dailyActivity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, Calories) %>% 
  summary()

#mean of Sedentary Minutes is 951 or 15,85 hrs.

#Total Distance vs Calories
ggplot(data = dailyActivity_Summary, aes(x = VeryActiveMinutes, y = calories)) +
  geom_point(aes(color = Usage)) +
  geom_smooth() +
  labs(title = "Total Distance vs Calories") + 
  scale_x_continuous(limits = c (0, 100))

#I found out whats the mean for every hour and group them by hour.
int_new <- Intensities %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data = int_new, aes(x = time, y = mean_total_int)) + 
  geom_histogram(stat = "identity", fill = 'orange') +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Average Total Intensite vs Time", x = "Time of the day", y = "Average Intensity")

#People are more active between 6am and 8pm
#Between these time Bellabeat can advise his users to do a walk or a run. 
# 
# 
# 
# dailyActivity_Summary$sum_Total_Minutes = dailyActivity_Summary$VeryActiveMinutes + dailyActivity_Summary$FairlyActiveMinutes + dailyActivity_Summary$LightlyActiveMinutes + dailyActivity_Summary$SedentaryMinutes
# ggplot(df, aes(area = , fill = value)) +
#   geom_treemap()

#mean steps
mean_steps <- mean(dailyActivity_Summary$steps)
mean_steps

#mean calories
mean_calories <- mean(dailyActivity_Summary$calories)
mean_calories

#plot calories/steps
ggplot(data = dailyActivity, aes(x = TotalSteps, y = Calories)) + 
  geom_point(aes(color = Calories)) +
  labs(title = "Calories burned for step taken", x = "Total steps taken", y = "Calories burned") +
  geom_smooth(method = "lm") +
  geom_hline(mapping = aes(yintercept = mean_calories), color = "yellow", lwd = 1.0) +
  geom_vline(mapping = aes(xintercept = mean_steps), color = "red", lwd = 1.0) +
  geom_text(mapping = aes(x = 10000, y = 500, label = "Average Steps", srt = -90)) +
  geom_text(mapping = aes(x = 32000, y = 2500, label = "Average Calories", srt = 0)) +
  scale_color_gradient(low="orange",high="darkgreen")

#mean Time in Bed
mean_TIB <-  mean(dailySleep$TotalTimeInBed)
mean_TIB

#mean Time Asleep
mean_TAS <- mean(dailySleep$TotalMinutesAsleep)
mean_TAS

#plot time in bed/time asleep
ggplot(data = dailySleep, aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) + 
  geom_point(aes(color = TotalTimeInBed)) + 
  geom_hline(mapping = aes(yintercept = mean_TIB), color = "yellow", lwd = 1.0) +
  geom_vline(mapping = aes(xintercept = mean_TAS), color = "red", lwd = 1.0) +
  labs(title = "Total Time in Bed vs Total Time Asleep", x = "Total Time Asleep", y = "Total Time in bed") + 
  geom_smooth(method = "lm") +
  scale_color_gradient(low="orange",high="darkgreen")

#merge data

merged_data <- merge(dailyActivity, dailySleep, by = "Id")
n_distinct(merged_data$Id)

ggplot(data = merged_data, aes(x = TotalMinutesAsleep, y = TotalSteps)) + 
  labs(title = "Total Steps vs Total Minutes Asleep")
  geom_point(color = alpha("orange", 0.1)) 
  

#mean BMI
mean_BMI <- mean(weight_Summary$averageBMI)
mean_BMI

#mean weight
mean_weight <- mean(weight_Summary$avg_weight_kg)
mean_weight

#plot bmi vs weight
ggplot(data = weight, aes(x = WeightKg, y = BMI)) + 
  geom_point(aes(color = BMI)) +
  labs(title = "BMI vs Weight", x = "Weight", y = "BMI") +
  geom_smooth(method = "lm") +
  geom_hline(mapping = aes(yintercept = mean_BMI), color = "yellow", lwd = 1.0) +
  geom_vline(mapping = aes(xintercept = mean_weight), color = "red", lwd = 1.0) +
  geom_text(mapping = aes(x = 90 , y = 45, label = "Average Weight - 78kg", srt = 0)) +
  geom_text(mapping = aes(x = 120, y = 27, label = "Average BMI - 28kg/m2", srt = 0)) +
  scale_color_gradient(low="orange",high="darkgreen")




  
