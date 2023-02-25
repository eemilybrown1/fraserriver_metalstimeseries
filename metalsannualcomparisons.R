library(tidyverse)
library(dplyr)
library(ggpubr)
library(lubridate)

head(hopemetals)

#adding a year column
hopemetalstbl <- 
  as_tibble(
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

elementbyyear <- function(data, y, sd) {
  ggplot(data, aes(x= year, 
                   y= {{y}})
  ) +
    geom_col(fill = "gray65") +
    geom_errorbar(aes(ymin = {{y}}-{{sd}},
                      ymax = {{y}}+{{sd}})) +
    geom_hline(yintercept = 0) +
    theme_light()
}

elementbyyear(data = hopemetals_byyear, 
              y = aluminum_total_ugL.1_mean,
              sd = aluminum_total_ugL.1_sd)


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

#Aluminum
elementbyyear(data = hopemetals_byyear, 
              y = aluminum_total_ugL.1_mean,
              sd = aluminum_total_ugL.1_sd)

extractmax(hopemetalstbl, aluminum_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#Antimony
elementbyyear(data = hopemetals_byyear, 
              y = antimony_total_ugL.1_mean,
              sd = antimony_total_ugL.1_sd)

extractmax(hopemetalstbl, antimony_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#Arsenic
elementbyyear(data = hopemetals_byyear, 
              y = arsenic_total_ugL.1_mean,
              sd = arsenic_total_ugL.1_sd)

extractmax(hopemetalstbl, arsenic_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)  #declining

#barium
elementbyyear(data = hopemetals_byyear, 
              y = barium_total_ugL.1_mean,
              sd = barium_total_ugL.1_sd)

extractmax(hopemetalstbl, barium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#beryllium
elementbyyear(data = hopemetals_byyear, 
              y = beryllium_total_ugL.1_mean,
              sd = beryllium_total_ugL.1_sd)

extractmax(hopemetalstbl, beryllium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm) #declining

#bismuth
elementbyyear(data = hopemetals_byyear, 
              y = bismuth_total_ugL.1_mean,
              sd = bismuth_total_ugL.1_sd)

extractmax(hopemetalstbl, bismuth_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm) #declining

#boron
elementbyyear(data = hopemetals_byyear, 
              y = boron_total_ugL.1_mean,
              sd = boron_total_ugL.1_sd)

extractmax(hopemetalstbl, boron_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm) #increasing?

#cadmium- interesting
elementbyyear(data = hopemetals_byyear, 
              y = cadmium_total_ugL.1_mean,
              sd = cadmium_total_ugL.1_sd) #Decreasing dramatically after 2002

extractmax(hopemetalstbl, cadmium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm) #decreasing


#cerium
elementbyyear(data = hopemetals_byyear, 
              y = cerium_total_ugL.1_mean,
              sd = cerium_total_ugL.1_sd)

extractmax(hopemetalstbl, cerium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm) #decreasing

#cesium
elementbyyear(data = hopemetals_byyear, 
              y = cesium_total_ugL.1_mean,
              sd = cesium_total_ugL.1_sd)

extractmax(hopemetalstbl, cesium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#chromium
elementbyyear(data = hopemetals_byyear, 
              y = chromium_total_ugL.1_mean,
              sd = chromium_total_ugL.1_sd)

extractmax(hopemetalstbl, chromium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#cobalt
elementbyyear(data = hopemetals_byyear, 
              y = cobalt_total_ugL.1_mean,
              sd = cobalt_total_ugL.1_sd)

extractmax(hopemetalstbl, cobalt_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#copper
elementbyyear(data = hopemetals_byyear, 
              y = copper_total_ugL.1_mean,
              sd = copper_total_ugL.1_sd)

extractmax(hopemetalstbl, copper_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#gallium
elementbyyear(data = hopemetals_byyear, 
              y = gallium_total_ugL.1_mean,
              sd = gallium_total_ugL.1_sd)

extractmax(hopemetalstbl, gallium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#iron
elementbyyear(data = hopemetals_byyear, 
              y = iron_total_ugL.1_mean,
              sd = iron_total_ugL.1_sd)

extractmax(hopemetalstbl, iron_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#lanthanum
elementbyyear(data = hopemetals_byyear, 
              y = lanthanum_total_ugL.1_mean,
              sd = lanthanum_total_ugL.1_sd)

extractmax(hopemetalstbl, lanthanum_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#lead
elementbyyear(data = hopemetals_byyear, 
              y = lead_total_ugL.1_mean,
              sd = lead_total_ugL.1_sd)

extractmax(hopemetalstbl, lead_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#lithium
elementbyyear(data = hopemetals_byyear, 
              y = lithium_total_ugL.1_mean,
              sd = lithium_total_ugL.1_sd)

extractmax(hopemetalstbl, lithium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#manganese
elementbyyear(data = hopemetals_byyear, 
              y = manganese_total_ugL.1_mean,
              sd = manganese_total_ugL.1_sd)

extractmax(hopemetalstbl, manganese_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#molybdenum
elementbyyear(data = hopemetals_byyear, 
              y = molybdenum_total_ugL.1_mean,
              sd = molybdenum_total_ugL.1_sd)

extractmax(hopemetalstbl, molybdenum_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#nickel
elementbyyear(data = hopemetals_byyear, 
              y = nickel_total_ugL.1_mean,
              sd = nickel_total_ugL.1_sd)

extractmax(hopemetalstbl, nickel_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#niobium
elementbyyear(data = hopemetals_byyear, 
              y = niobium_total_ugL.1_mean,
              sd = niobium_total_ugL.1_sd)

extractmax(hopemetalstbl, niobium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#platinum
elementbyyear(data = hopemetals_byyear, 
              y = platinum_total_ugL.1_mean,
              sd = platinum_total_ugL.1_sd)

extractmax(hopemetalstbl, platinum_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)


#rubidium
elementbyyear(data = hopemetals_byyear, 
              y = rubidium_total_ugL.1_mean,
              sd = rubidium_total_ugL.1_sd)

extractmax(hopemetalstbl, rubidium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#selenium
elementbyyear(data = hopemetals_byyear, 
              y = selenium_total_ugL.1_mean,
              sd = selenium_total_ugL.1_sd)

extractmax(hopemetalstbl, selenium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#silver
elementbyyear(data = hopemetals_byyear, 
              y = silver_total_ugL.1_mean,
              sd = silver_total_ugL.1_sd) #decreases after 2002

extractmax(hopemetalstbl, silver_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm) #increasing

#strontium
elementbyyear(data = hopemetals_byyear, 
              y = strontium_total_ugL.1_mean,
              sd = strontium_total_ugL.1_sd)

extractmax(hopemetalstbl, strontium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#thallium
elementbyyear(data = hopemetals_byyear, 
              y = thallium_total_ugL.1_mean,
              sd = thallium_total_ugL.1_sd)

extractmax(hopemetalstbl, thallium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#tin
elementbyyear(data = hopemetals_byyear, 
              y = tin_total_ugL.1_mean,
              sd = tin_total_ugL.1_sd)

extractmax(hopemetalstbl, tin_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#uranium
elementbyyear(data = hopemetals_byyear, 
              y = uranium_total_ugL.1_mean,
              sd = uranium_total_ugL.1_sd)

extractmax(hopemetalstbl, uranium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#vanadium
elementbyyear(data = hopemetals_byyear, 
              y = vanadium_total_ugL.1_mean,
              sd = vanadium_total_ugL.1_sd)

extractmax(hopemetalstbl, vanadium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#yttrium
elementbyyear(data = hopemetals_byyear, 
              y = yttrium_total_ugL.1_mean,
              sd = yttrium_total_ugL.1_sd)

extractmax(hopemetalstbl, yttrium_total_ugL.1) %>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)

#zinc
elementbyyear(data = hopemetals_byyear, 
              y = zinc_total_ugL.1_mean,
              sd = zinc_total_ugL.1_sd)

extractmax(hopemetalstbl, zinc_total_ugL.1) #%>%
  ggplot(aes(x = year, 
             y = day)) +
  geom_point() +
  geom_smooth(method = lm)



#Select day number from extract max, make a tbl with 
peakdays <- select(extractmax(hopemetalstbl, aluminum_total_ugL.1), year, day) %>%
  rename(aluminum_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, antimony_total_ugL.1), year, day)) %>%
  rename(antimony_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, arsenic_total_ugL.1), year, day)) %>%
  rename(arsenic_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, barium_total_ugL.1), year, day)) %>%
  rename(barium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, beryllium_total_ugL.1), year, day)) %>%
  rename(beryllium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, bismuth_total_ugL.1), year, day)) %>%
  rename(bismuth_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, boron_total_ugL.1), year, day)) %>%
  rename(boron_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, cadmium_total_ugL.1), year, day)) %>%
  rename(cadmium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, cerium_total_ugL.1), year, day)) %>%
  rename(cerium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, cesium_total_ugL.1), year, day)) %>%
  rename(cesium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, chromium_total_ugL.1), year, day)) %>%
  rename(chromium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, cobalt_total_ugL.1), year, day)) %>%
  rename(cobalt_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, copper_total_ugL.1), year, day)) %>%
  rename(copper_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, europium_total_ugL.1), year, day)) %>%
  rename(europium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, gadolinium_totalrecoverable_ugL.1), year, day)) %>%
  rename(gadolinium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, gallium_total_ugL.1), year, day)) %>%
  rename(gallium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, germanium_total_ugL.1), year, day)) %>%
  rename(germanium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, hafnium_total_ugL.1), year, day)) %>%
  rename(hafnium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, holmium_total_ugL.1), year, day)) %>%
  rename(holmium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, indium_total_ugL.1), year, day)) %>%
  rename(indium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, iron_total_ugL.1), year, day)) %>%
  rename(iron_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, lanthanum_total_ugL.1), year, day)) %>%
  rename(lanthanum_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, lead_total_ugL.1), year, day)) %>%
  rename(lead_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, lithium_total_ugL.1), year, day)) %>%
  rename(lithium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, lutetium_total_ugL.1), year, day)) %>%
  rename(lutetium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, manganese_total_ugL.1), year, day)) %>%
  rename(manganese_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, molybdenum_total_ugL.1), year, day)) %>%
  rename(molybdenum_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, neodymium_total_ugL.1), year, day)) %>%
  rename(neodymium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, nickel_total_ugL.1), year, day)) %>%
  rename(nickel_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, niobium_total_ugL.1), year, day)) %>%
  rename(niobium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, palladium_total_ugL.1), year, day)) %>%
  rename(palladium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, platinum_total_ugL.1), year, day)) %>%
  rename(platinum_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, praseodymium_total_ugL.1), year, day)) %>%
  rename(praseodymium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, rubidium_total_ugL.1), year, day)) %>%
  rename(rubidium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, ruthenium_total_ugL.1), year, day)) %>%
  rename(ruthenium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, samarium_total_ugL.1), year, day)) %>%
  rename(samarium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, scandium_total_ugL.1), year, day)) %>%
  rename(scandium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, selenium_total_ugL.1), year, day)) %>%
  rename(selenium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, silver_total_ugL.1), year, day)) %>%
  rename(silver_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, strontium_total_ugL.1), year, day)) %>%
  rename(strontium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, tellurium_total_ugL.1), year, day)) %>%
  rename(tellurium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, terbium_total_ugL.1), year, day)) %>%
  rename(terbium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, thallium_total_ugL.1), year, day)) %>%
  rename(thallium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, tin_total_ugL.1), year, day)) %>%
  rename(tin_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, titanium_total_ugL.1), year, day)) %>%
  rename(titanium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, tungsten_total_ugL.1), year, day)) %>%
  rename(tungsten_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, uranium_total_ugL.1), year, day)) %>%
  rename(uranium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, vanadium_total_ugL.1), year, day)) %>%
  rename(vanadium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, ytterbium_total_ugL.1), year, day)) %>%
  rename(ytterbium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, yttrium_total_ugL.1), year, day)) %>%
  rename(yttrium_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, zinc_total_ugL.1), year, day)) %>%
  rename(zinc_day = day) %>%
  full_join(select(extractmax(hopemetalstbl, zirconium_total_ugL.1), year, day)) %>%
  rename(zirconium_day = day)
  
sapply(peakdays, function(col) length(unique(col)))
peakdays <- peakdays[, sapply(peakdays, function(col) length(unique(col))) > 3]


#Correlation matrix
library(Hmisc)
library(corrplot)

cor_peakdays <-
  cor(x = peakdays$year, y= peakdays[2:36], use="pairwise.complete.obs")
rownames(cor_peakdays) <- 'year'

corrplot(cor_peakdays,
         type = 'upper',
         title = "Day of Peak")
#need to make it look better but got the info there


###Need to make time series for all years, then maybe overlay them. Could do it by rainbow colour to see differences




#To get column names
colnames(hopemetalstbl %>% group_by(year) %>%summarize(across(aluminum_total_ugL.1:zirconium_total_ugL.1, list(mean = mean, sd = sd))))[2]
