install.packages("tidyverse")
install.packages("pacman")
install.packages("STAT")
#install.packages("anytime")
library(pacman)
library(tidyverse)
library(lubridate)
library("STAT")
#library(anytime)
library(dplyr)
rm(list = ls())
df2019 <- read_csv("~/Desktop/NBSampleWork/green_tripdata_2019-01.csv")
df2020 <- read_csv("~/Desktop/NBSampleWork/green_tripdata_2020-01.csv")
print(df2019)
print(df2020)

#January 2019 New York Green Taxi Trip Records
df2019$dropoff_datetime <-  strptime(df2019$lpep_dropoff_datetime, "%m/%d/%y %H:%M") 
df2019$year <-year( df2019$dropoff_datetime) 
df2019$day <-day( df2019$dropoff_datetime) 
df2019$hour <-hour( df2019$dropoff_datetime) 
df2019new<- as.tibble(df2019)%>%
#  mutate(Total_amt_woTip=total_amount-tip_amount) %>%   
filter(year=="2019")

#January 2020 New York Green Taxi Trip Records
df2020$dropoff_datetime <-  strptime(df2020$lpep_dropoff_datetime, "%m/%d/%y %H:%M") 
df2020$year <-year( df2020$dropoff_datetime) 
df2020$day <-day( df2020$dropoff_datetime) 
df2020$hour <-hour( df2020$dropoff_datetime) 
df2020new<- as.tibble(df2020)%>%
#  mutate(Total_amt_woTip=total_amount-tip_amount) %>%   
  filter(year=="2020")  

#2019 Trip Frequency
ggplot(data=df2019new,aes(x=hour))+geom_histogram(binwidth=1, color="black", fill="white")+labs(title='Daily January 2019 trips')+facet_wrap(~day)

#2020 Trip Frequency
ggplot(data=df2020new,aes(x=hour))+geom_histogram(binwidth=1, color="black", fill="white")+labs(title='Daily January 2020 trips')+facet_wrap(~day)

# Total revenue by ratecodeID 
  
    df <- df2020new %>% 
    df <- df2019new %>% 
    group_by(day, RatecodeID) %>% 
    filter(RatecodeID!=99) %>% 
    summarise(mean=round(mean(total_amount,na.rm=TRUE)))
    ggplot(data = df,aes(x = day, y = mean, fill = RatecodeID)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    labs(
      x = "day",
      y = "total_amount ($)",
      title = paste(
        "Daily January 2020 total income "
      ))

# Trip type effects on fare 
#  df_bar <- df2019new%>%
  df_bar <- df2020new%>%
  group_by(trip_type) %>% 
  summarise(mean=round(mean(total_amount,na.rm=TRUE)))
  
ggplot(data = df_bar, aes(x = trip_type, y = mean))+ geom_col(alpha=2,color= 'chartreuse4',fill='chartreuse4',size=0.2) +labs(title='Daily January 2020 total income ',x="Trip type",y="total_amount ($)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),                                                                                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+ geom_text(aes(label=mean,vjust=-.5,))

# passenger number effects on fare 

df_bar <- df2019new%>%
  group_by(passenger_count) %>% 
  summarise(mean=round(mean(total_amount,na.rm=TRUE)))

ggplot(data = df_bar, aes(x = passenger_count, y = mean))+ geom_col(alpha=2,color= 'chocolate',fill='chocolate',size=0.2) +labs(title='Daily January 2019 total income ',x="number of passenger",y="total_amount ($)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),                                                                                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+ geom_text(aes(label=mean,vjust=-.5,))

#Daily trip distance effects of fare

df_bar <- df2020new%>%
#  df_bar <- df2020new%>%
  group_by(day) %>% 
  summarise(mean=round(sum(trip_distance,na.rm=TRUE)/1000))

ggplot(data = df_bar, aes(x = day, y = mean))+ geom_col(alpha=2,color= 'blue3',fill='blue3',size=0.2) +labs(title='Daily January 2020 total trip distance ',x="days",y="trip distance (1,000 miles)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),                                                                                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Regression 2019

df1<- lm(total_amount~day+hour+trip_distance+passenger_count+trip_type+RatecodeID,data=df2019new)
summary(df1)

#Regression 2020

df2<- lm(total_amount~day+hour+trip_distance+passenger_count+trip_type+RatecodeID,data=df2020new)
summary(df2)