library(tidyverse)
library(dplyr)
library(ggpubr)

##Loading datasheets

metals1 <- read.csv("metals1.csv") %>%
  rename("sample_time" = 1)

metals2 <- read.csv("metals2.csv")%>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
metals3 <- read.csv("metals3.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
metals4 <- read.csv("metals4.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
metals5 <- read.csv("metals5.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
metals6 <- read.csv("metals6.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)

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
metals <- metals %>%
  filter(if_all(everything(), ~!grepl(-999999.000, .)))

##Adding week and month variable to metals df
metals <- metals %>%
  mutate(week = lubridate::isoweek(sample_time),
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))
  

##Creating mean dataframes for weeks and months
meanweeks <-
  metals %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

meanmonths <-
  metals %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

##Trying to make a function for this plot but not working
weektimeseries <- function(datametals, datameanweeks,
                           varmetals, varmeanweeks) {
  ggplot() +
    geom_point(data = datametals,
               aes(x = week,
                   y= varmetals),
               alpha = 1/5)+
    geom_line(data = datameanweeks,
              aes(x = week,
                  y= varmeanweeks,
                  group = 1)) +
    theme_light()
}


##Plotting all elements by week (average lines only for facetting)
weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = meanweeks,
              aes(x = week,
                  y={{y}},
                  group = 1)) +
    theme_light()
}

Al_week_line <- weeklineplots(aluminum_total_ugL.1)
Sb_week_line <- weeklineplots(antimony_total_ugL.1)
As_week_line <- weeklineplots(arsenic_total_ugL.1)
Ba_week_line <- weeklineplots(barium_total_ugL.1)
Be_week_line <- weeklineplots(beryllium_total_ugL.1)
Bi_week_line <- weeklineplots(bismuth_total_ugL.1)
B_week_line <- weeklineplots(boron_total_ugL.1)
Cd_week_line <- weeklineplots(cadmium_total_ugL.1)
Ce_week_line <- weeklineplots(cerium_total_ugL.1)
Cs_week_line <- weeklineplots(cesium_total_ugL.1)
Cr_week_line <- weeklineplots(chromium_total_ugL.1)
Co_week_line <- weeklineplots(cobalt_total_ugL.1)
Cu_week_line <- weeklineplots(copper_total_ugL.1)
Eu_week_line <-weeklineplots(europium_total_ugL.1)
Gd_week_line <- weeklineplots(gadolinium_totalrecoverable_ugL.1)
Ga_week_line <- weeklineplots(gallium_total_ugL.1)
Ge_week_line <- weeklineplots(germanium_total_ugL.1)
Hf_week_line <- weeklineplots(hafnium_total_ugL.1)
Ho_week_line <- weeklineplots(holmium_total_ugL.1)
In_week_line <- weeklineplots(indium_total_ugL.1)
Fe_week_line <- weeklineplots(iron_total_ugL.1)
La_week_line <- weeklineplots(lanthanum_total_ugL.1)
Pb_week_line <- weeklineplots(lead_total_ugL.1)
Li_week_line <- weeklineplots(lithium_total_ugL.1)
Lu_week_line <- weeklineplots(lutetium_total_ugL.1)
Mn_week_line <- weeklineplots(manganese_total_ugL.1)
Mo_week_line <- weeklineplots(molybdenum_total_ugL.1)
Nd_week_line <- weeklineplots(neodymium_total_ugL.1)
Ni_week_line <- weeklineplots(nickel_total_ugL.1)
Nb_week_line <- weeklineplots(niobium_total_ugL.1)
Pd_week_line <-
Pt_week_line <-
Pr_week_line <-
Rb_week_line <-
Ru_week_line <-
Sm_week_line <-
Sc_week_line <-
Se_week_line <-
Ag_week_line <-
Sr_week_line <-
Te_week_line <-
Tb_week_line <-
Tl_week_line <-
Sn_week_line <-
Ti_week_line <-
W_week_line <-
U_week_line <-
V_week_line <- 
Yb_week_line <-
Y_week_line <-
Zn_week_line <-
Zr_week_line <-



##Plotting all elements by month


##Plotting by individual element
Al_week <- 
ggplot() +
  geom_point(data = metals,
             aes(x = week,
                 y=aluminum_total_ugL.1),
             alpha = 1/5)+
  geom_line(data = meanweeks,
            aes(x = week,
                y=aluminum_total_ugL.1,
                group = 1)) +
  theme_light()

Al_month <-
  ggplot() +
  geom_point(data = metals,
             aes(x = month,
                 y=aluminum_total_ugL.1),
             alpha = 1/5)+
  geom_line(data = meanmonths,
            aes(x = month,
                y=aluminum_total_ugL.1,
                group = 1)) +
  theme_light()
Al_month
