library(lubridate)

head(hopemetals)

#adding a year column
hopemetalstbl <- 
  as.tibble(
hopemetals %>%
  mutate(year = isoyear(sample_time),
         day = yday(sample_time)) %>%
  relocate("year", .after = "month") %>%
  relocate("day", .before = "week")
  )


#First getting an idea of annual variability in means
hopemetals_byyear <-
hopemetalstbl %>%
  group_by(year) %>%
  summarize(across(aluminum_total_ugL.1:zirconium_total_ugL.1, list(mean = mean, 
                                                                    sd = sd)
  ))

#Plotting one element

ggplot(hopemetals_byyear, aes(x= year, 
                              y= aluminum_total_ugL.1_mean)
      ) +
  geom_point() +
  geom_errorbar(aes(ymin = aluminum_total_ugL.1_mean-aluminum_total_ugL.1_sd,
                    ymax = aluminum_total_ugL.1_mean+aluminum_total_ugL.1_sd))



#Now looking at peak time in each year
extractmax <- function(data, x) {
  data %>%
    group_by(year) %>%
    slice(which.max({{x}})) %>%
    select(c('day', 'month', 'year', {{x}}))
}

#Plotting day of the year with maximum value
extractmax(hopemetalstbl, aluminum_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


###Need to repeat this for all of the metals!

###Need to make time series for all years, then maybe overlay them. Could do it by rainbow colour to see differences




#To get column names
colnames(hopemetalstbl %>% group_by(year) %>%summarize(across(aluminum_total_ugL.1:zirconium_total_ugL.1, list(mean = mean, sd = sd))))[2]
