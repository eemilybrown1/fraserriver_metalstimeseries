#DOC time series


gravesend <- read.csv("gravesend_DOC_specificconductance_20102022.csv")

hope <- read.csv("hope_DOC_specificconductance_20102022.csv")

hopemetals <- hopemetals %>%
  mutate(week = lubridate::isoweek(sample_time),
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))


##Creating mean dataframes for weeks and months
hope_meanweeks <-
  hope %>%
  group_by(lubridate::isoweek(Sample.time)) %>%
  summarize_at(vars("DOC_mgL.1":"specificconductance_uSeiCm.1"),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(Sample.time)") %>%
  mutate(week = factor(week))

hope_meanmonths <-
  hope %>%
  group_by(lubridate::month(Sample.time)) %>%
  summarize_at(vars("DOC_mgL.1":"specificconductance_uSeiCm.1"),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(Sample.time)") %>%
  mutate(month = factor(month))

hope_weeklineplots(DOC_mgL.1)

hope_monthlineplots(DOC_mgL.1)

max(hope$DOC_mgL.1, na.rm = TRUE)
min(hope$DOC_mgL.1, na.rm = TRUE)
mean(hope$DOC_mgL.1, na.rm = TRUE)

#Gravesend
gravesend_meanweeks <-
  gravesend %>%
  group_by(lubridate::isoweek(Sample.time)) %>%
  summarize_at(vars("DOC_mgL.1":"specificconductance_uSeiCm.1"),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(Sample.time)") %>%
  mutate(week = factor(week))

gravesend_meanmonths <-
  gravesend %>%
  group_by(lubridate::month(Sample.time)) %>%
  summarize_at(vars("DOC_mgL.1":"specificconductance_uSeiCm.1"),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(Sample.time)") %>%
  mutate(month = factor(month))

gravesend_weeklineplots(DOC_mgL.1)

gravesend_monthlineplots(DOC_mgL.1)

max(gravesend$DOC_mgL.1, na.rm = TRUE)
min(gravesend$DOC_mgL.1, na.rm = TRUE)
mean(gravesend$DOC_mgL.1, na.rm = TRUE)

