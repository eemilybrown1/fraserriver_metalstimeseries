
##Looking at specific conductance to figure out why the gravesend values are so much lower
library(tidyverse)
library(dplyr)
library(ggpubr)


#Gravesend
gravesendspecificconductance <- read.csv("gravesendspecificconductance.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)

gravesendspecificconductance <- gravesendspecificconductance %>%
  mutate(week = lubridate::isoweek(sample_time),
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))

gravesend_specificconductance_meanweeks <-
  gravesendspecificconductance %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at("specificconductance_useicm.1",
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

gravesend_specificconductance_meanmonths <-
  gravesendspecificconductance %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at("specificconductance_useicm.1",
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

gravesend_specificconductance_week <-
  ggplot() +
  geom_line(data = gravesend_specificconductance_meanweeks,
            aes(x = week,
                y= specificconductance_useicm.1,
                group = 1)) +
  geom_hline(yintercept = 1500) +
  scale_x_discrete(breaks=seq(1,52,13)) +
  theme_light()

gravesend_specificconductance_week

gravesend_specificconductance_month <-
  ggplot() +
  geom_line(data = gravesend_specificconductance_meanmonths,
            aes(x = month,
                y= specificconductance_useicm.1,
                group = 1)) +
  geom_hline(yintercept = 1500) +
  scale_x_discrete(breaks=seq(1,12,4)) +
  theme_light()
gravesend_specificconductance_month

specificconductance_gravesend <-
  ggarrange(gravesend_specificconductance_week, 
            gravesend_specificconductance_month,
            nrow= 2, ncol=1)
specificconductance_gravesend


#Hope

hopespecificconductance <- read.csv("hopespecificconductance.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)

hopespecificconductance <- hopespecificconductance %>%
  mutate(week = lubridate::isoweek(sample_time),
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))

hope_specificconductance_meanweeks <-
  hopespecificconductance %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at("specificconductance_useicm.1",
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

hope_specificconductance_meanmonths <-
  hopespecificconductance %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at("specificconductance_useicm.1",
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

hope_specificconductance_week <-
  ggplot() +
  geom_line(data = hope_specificconductance_meanweeks,
            aes(x = week,
                y= specificconductance_useicm.1,
                group = 1)) +
  geom_hline(yintercept = 1500) +
  scale_x_discrete(breaks=seq(1,52,13)) +
  theme_light()

hope_specificconductance_week

hope_specificconductance_month <-
  ggplot() +
  geom_line(data = hope_specificconductance_meanmonths,
            aes(x = month,
                y= specificconductance_useicm.1,
                group = 1)) +
  geom_hline(yintercept = 1500) +
  scale_x_discrete(breaks=seq(1,12,4)) +
  theme_light()
hope_specificconductance_month

specificconductance_hope <-
  ggarrange(hope_specificconductance_week, 
            hope_specificconductance_month,
            nrow= 2, ncol=1)
specificconductance_hope

###Combining graphs to compare between sites
hopespecificconductance <- hopespecificconductance %>%
  mutate(site = "hope") %>%
  relocate(site, .after = month)

gravesendspecificconductance <- gravesendspecificconductance %>%
  mutate(site = "gravesend") %>%
  relocate(site, .after = month)  

specificconductance <- hopespecificconductance %>%
  full_join(gravesendspecificconductance, 
            by = c("sample_time", "week", "month", "sample_number", "sample_type", "site", "specificconductance_useicm.1"))

specificconductance_meanweeks <-
  specificconductance %>%
  group_by(lubridate::isoweek(sample_time), site) %>%
  summarize_at("specificconductance_useicm.1",
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

specificconductance_meanmonths <-
  specificconductance %>%
  group_by(lubridate::month(sample_time), site) %>%
  summarize_at("specificconductance_useicm.1",
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

#plotting together

specificconductance_week <-
  ggplot() +
  geom_line(data = specificconductance_meanweeks,
            aes(x = week,
                y= specificconductance_useicm.1,
                group = site,
                colour = site)) +
  geom_hline(yintercept = 1500) +
  scale_x_discrete(breaks=seq(1,52,13)) +
  theme_light()

specificconductance_week

specificconductance_month <-
  ggplot() +
  geom_line(data = specificconductance_meanmonths,
            aes(x = month,
                y= specificconductance_useicm.1,
                group = site,
                colour = site)) +
  geom_hline(yintercept = 1500) +
  scale_x_discrete(breaks=seq(1,12,4)) +
  theme_light()
specificconductance_month

specificconductance <-
  ggarrange(specificconductance_week, 
            specificconductance_month,
            nrow= 2, ncol=1)
specificconductance