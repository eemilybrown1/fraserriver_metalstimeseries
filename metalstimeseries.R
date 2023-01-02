library(tidyverse)
library(here)
setwd("C:/Users/emmie/OneDrive/Desktop")

##Loading datasheets

metals1 <- read.csv("metals1.csv") %>%
  rename("sample_time" = "ï..sample_time")
metals2 <- read.csv("metals2.csv")%>%
  rename("sample_time" = "ï..Sample.time",
         "sample_number" = "Sample.number",
         "sample_type" = "Sample.type")
metals3 <- read.csv("metals3.csv") %>%
  rename("sample_time" = "ï..Sample.time",
         "sample_number" = "Sample.number",
         "sample_type" = "Sample.type")
metals4 <- read.csv("metals4.csv") %>%
  rename("sample_time" = "ï..Sample.time",
         "sample_number" = "Sample.number",
         "sample_type" = "Sample.type")
metals5 <- read.csv("metals5.csv") %>%
  rename("sample_time" = "ï..Sample.time",
         "sample_number" = "Sample.number",
         "sample_type" = "Sample.type")
metals6 <- read.csv("metals6.csv") %>%
  rename("sample_time" = "ï..Sample.time",
         "sample_number" = "Sample.number",
         "sample_type" = "Sample.type")

##Joining to one sheet

metals <- metals1 %>%
  full_join(metals2, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(metals3, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(metals4, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(metals5, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(metals6, 
             by = c("sample_time", "sample_number", "sample_type"))

##Removing an error variable
metals <- metals[1:441,]


##Creating mean dataframes for weeks and months
meanweeks <-
  metals %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)")

meanmonths <-
  metals %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)")

##Plotting all elements by week
meanweeks %>%
  pivot_wider() #Need to pivot the dataframe to get weeks on left
#  mutate(week = factor(week)) %>%
ggplot(aes(x=week)) +
  geom_line(aes(y=aluminum_total_ugL.1))

##Plotting all elements by month


##Plotting by individual element
Al_weeks <-
ggplot() +
  geom_point(data = metals,
             aes(x = week,
                 y=aluminum_total_ugL.1,
                 group = week))+
  geom_line(data = meanweeks,
            aes(x = week,
                y=aluminum_total_ugL.1))

Al_months <-
  ggplot() +
  geom_point(data = metals,
             aes(x = month,
                 y=aluminum_total_ugL.1,
                 group = week))+
  geom_line(data = meanmonths,
            aes(x = month,
                y=aluminum_total_ugL.1))
Al_months
