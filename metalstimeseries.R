library(tidyverse)
library(dplyr)
library(ggpubr)

##Loading datasheets

hopemetals1 <- read.csv("hopemetals1.csv") %>%
  rename("sample_time" = 1)

hopemetals2 <- read.csv("hopemetals2.csv")%>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
hopemetals3 <- read.csv("hopemetals3.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
hopemetals4 <- read.csv("hopemetals4.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
hopemetals5 <- read.csv("hopemetals5.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
hopemetals6 <- read.csv("hopemetals6.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)

##Joining to one sheet

hopemetals <- hopemetals1 %>%
  full_join(hopemetals2, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(hopemetals3, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(hopemetals4, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(hopemetals5, 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(hopemetals6, 
             by = c("sample_time", "sample_number", "sample_type"))

##Removing an error variable
hopemetals <- hopemetals %>%
  filter(if_all(everything(), ~!grepl(-999999.000, .)))

##Adding week and month variable to metals df
hopemetals <- hopemetals %>%
  mutate(week = lubridate::isoweek(sample_time),
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))
  

##Creating mean dataframes for weeks and months
hope_meanweeks <-
  hopemetals %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

hope_meanmonths <-
  hopemetals %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))


##Plotting all elements by week (average lines only for facetting)
hope_weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = hope_meanweeks,
              aes(x = week,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,52,13)) +
    theme_light()
}

Al_week_line_hope <- hope_weeklineplots(aluminum_total_ugL.1)
Sb_week_line_hope <- hope_weeklineplots(antimony_total_ugL.1)
As_week_line_hope <- hope_weeklineplots(arsenic_total_ugL.1)
Ba_week_line_hope <- hope_weeklineplots(barium_total_ugL.1)
Be_week_line_hope <- hope_weeklineplots(beryllium_total_ugL.1)
Bi_week_line_hope <- hope_weeklineplots(bismuth_total_ugL.1)
B_week_line_hope <- hope_weeklineplots(boron_total_ugL.1)
Cd_week_line_hope <- hope_weeklineplots(cadmium_total_ugL.1)
Ce_week_line_hope <- hope_weeklineplots(cerium_total_ugL.1)
Cs_week_line_hope <- hope_weeklineplots(cesium_total_ugL.1)
Cr_week_line_hope <- hope_weeklineplots(chromium_total_ugL.1)
Co_week_line_hope <- hope_weeklineplots(cobalt_total_ugL.1)
Cu_week_line_hope <- hope_weeklineplots(copper_total_ugL.1)
Eu_week_line_hope <-hope_weeklineplots(europium_total_ugL.1)
Gd_week_line_hope <- hope_weeklineplots(gadolinium_totalrecoverable_ugL.1)
Ga_week_line_hope <- hope_weeklineplots(gallium_total_ugL.1)
Ge_week_line_hope <- hope_weeklineplots(germanium_total_ugL.1)
Hf_week_line_hope <- hope_weeklineplots(hafnium_total_ugL.1)
Ho_week_line_hope <- hope_weeklineplots(holmium_total_ugL.1)
In_week_line_hope <- hope_weeklineplots(indium_total_ugL.1)
Fe_week_line_hope <- hope_weeklineplots(iron_total_ugL.1)
La_week_line_hope <- hope_weeklineplots(lanthanum_total_ugL.1)
Pb_week_line_hope <- hope_weeklineplots(lead_total_ugL.1)
Li_week_line_hope <- hope_weeklineplots(lithium_total_ugL.1)
Lu_week_line_hope <- hope_weeklineplots(lutetium_total_ugL.1)
Mn_week_line_hope <- hope_weeklineplots(manganese_total_ugL.1)
Mo_week_line_hope <- hope_weeklineplots(molybdenum_total_ugL.1)
Nd_week_line_hope <- hope_weeklineplots(neodymium_total_ugL.1)
Ni_week_line_hope <- hope_weeklineplots(nickel_total_ugL.1)
Nb_week_line_hope <- hope_weeklineplots(niobium_total_ugL.1)
Pd_week_line_hope <- hope_weeklineplots(palladium_total_ugL.1)
Pt_week_line_hope <- hope_weeklineplots(platinum_total_ugL.1)
Pr_week_line_hope <- hope_weeklineplots(praseodymium_total_ugL.1)
Rb_week_line_hope <- hope_weeklineplots(rubidium_total_ugL.1)
Ru_week_line_hope <- hope_weeklineplots(ruthenium_total_ugL.1)
Sm_week_line_hope <- hope_weeklineplots(samarium_total_ugL.1)
Sc_week_line_hope <- hope_weeklineplots(scandium_total_ugL.1)
Se_week_line_hope <- hope_weeklineplots(selenium_total_ugL.1)
Ag_week_line_hope <- hope_weeklineplots(silver_total_ugL.1)
Sr_week_line_hope <- hope_weeklineplots(strontium_total_ugL.1)
Te_week_line_hope <- hope_weeklineplots(tellurium_total_ugL.1)
Tb_week_line_hope <- hope_weeklineplots(terbium_total_ugL.1)
Tl_week_line_hope <- hope_weeklineplots(thallium_total_ugL.1)
Sn_week_line_hope <- hope_weeklineplots(tin_total_ugL.1)
Ti_week_line_hope <- hope_weeklineplots(titanium_total_ugL.1)
W_week_line_hope <- hope_weeklineplots(tungsten_total_ugL.1)
U_week_line_hope <- hope_weeklineplots(uranium_total_ugL.1)
V_week_line_hope <- hope_weeklineplots(vanadium_total_ugL.1)
Yb_week_line_hope <- hope_weeklineplots(ytterbium_total_ugL.1)
Y_week_line_hope <- hope_weeklineplots(yttrium_total_ugL.1)
Zn_week_line_hope <- hope_weeklineplots(zinc_total_ugL.1)
Zr_week_line_hope <- hope_weeklineplots(zirconium_total_ugL.1)

allmetals_week_line_hope <-
ggarrange(Ag_week_line_hope,
          Al_week_line_hope,
          As_week_line_hope,
          B_week_line_hope,
          Ba_week_line_hope,
          Be_week_line_hope,
          Bi_week_line_hope,
          Cd_week_line_hope,
          Ce_week_line_hope,
          Co_week_line_hope,
          Cr_week_line_hope,
          Cs_week_line_hope,
          Cu_week_line_hope,
          Eu_week_line_hope,
          Fe_week_line_hope,
          Ga_week_line_hope,
          Gd_week_line_hope,
          Ge_week_line_hope,
          Hf_week_line_hope,
          Ho_week_line_hope,
          In_week_line_hope,
          La_week_line_hope,
          Li_week_line_hope,
          Lu_week_line_hope,
          Mn_week_line_hope,
          Mo_week_line_hope,
          Nb_week_line_hope,
          Nd_week_line_hope,
          Ni_week_line_hope,
          Pb_week_line_hope,
          Pd_week_line_hope,
          Pr_week_line_hope,
          Pt_week_line_hope,
          Rb_week_line_hope,
          Ru_week_line_hope,
          Sb_week_line_hope,
          Sc_week_line_hope,
          Se_week_line_hope,
          Sm_week_line_hope,
          Sn_week_line_hope,
          Sr_week_line_hope,
          Tb_week_line_hope,
          Te_week_line_hope,
          Ti_week_line_hope,
          Tl_week_line_hope,
          U_week_line_hope,
          V_week_line_hope,
          W_week_line_hope,
          Y_week_line_hope,
          Yb_week_line_hope,
          Zn_week_line_hope,
          Zr_week_line_hope,
          ncol = 8, nrow = 7)
          
print(allmetals_week_line_hope) #There are a bunch without any data so making a version without those

metals_week_line_hope <-
  ggarrange(Ag_week_line_hope,
            Al_week_line_hope,
            As_week_line_hope,
            B_week_line_hope,
            Ba_week_line_hope,
            Be_week_line_hope,
            Bi_week_line_hope,
            Cd_week_line_hope,
            Ce_week_line_hope,
            Co_week_line_hope,
            Cr_week_line_hope,
            Cs_week_line_hope,
            Cu_week_line_hope,
            Fe_week_line_hope,
            Ga_week_line_hope,
            La_week_line_hope,
            Li_week_line_hope,
            Mn_week_line_hope,
            Mo_week_line_hope,
            Nb_week_line_hope,
            Ni_week_line_hope,
            Pb_week_line_hope,
            Pt_week_line_hope,
            Rb_week_line_hope,
            Sb_week_line_hope,
            Se_week_line_hope,
            Sn_week_line_hope,
            Sr_week_line_hope,
            Ti_week_line_hope,
            Tl_week_line_hope,
            U_week_line_hope,
            V_week_line_hope,
            W_week_line_hope,
            Y_week_line_hope,
            Zn_week_line_hope,
            ncol = 6, nrow = 6)

print(metals_week_line_hope)

##Plotting all elements by month
hope_monthlineplots <- function(y) {
  ggplot() +
    geom_line(data = hope_meanmonths,
              aes(x = month,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,12,4)) +
    theme_light()
}

Al_month_line_hope <- hope_monthlineplots(aluminum_total_ugL.1)
Sb_month_line_hope <- hope_monthlineplots(antimony_total_ugL.1)
As_month_line_hope <- hope_monthlineplots(arsenic_total_ugL.1)
Ba_month_line_hope <- hope_monthlineplots(barium_total_ugL.1)
Be_month_line_hope <- hope_monthlineplots(beryllium_total_ugL.1)
Bi_month_line_hope <- hope_monthlineplots(bismuth_total_ugL.1)
B_month_line_hope <- hope_monthlineplots(boron_total_ugL.1)
Cd_month_line_hope <- hope_monthlineplots(cadmium_total_ugL.1)
Ce_month_line_hope <- hope_monthlineplots(cerium_total_ugL.1)
Cs_month_line_hope <- hope_monthlineplots(cesium_total_ugL.1)
Cr_month_line_hope <- hope_monthlineplots(chromium_total_ugL.1)
Co_month_line_hope <- hope_monthlineplots(cobalt_total_ugL.1)
Cu_month_line_hope <- hope_monthlineplots(copper_total_ugL.1)
Eu_month_line_hope <-hope_monthlineplots(europium_total_ugL.1)
Gd_month_line_hope <- hope_monthlineplots(gadolinium_totalrecoverable_ugL.1)
Ga_month_line_hope <- hope_monthlineplots(gallium_total_ugL.1)
Ge_month_line_hope <- hope_monthlineplots(germanium_total_ugL.1)
Hf_month_line_hope <- hope_monthlineplots(hafnium_total_ugL.1)
Ho_month_line_hope <- hope_monthlineplots(holmium_total_ugL.1)
In_month_line_hope <- hope_monthlineplots(indium_total_ugL.1)
Fe_month_line_hope <- hope_monthlineplots(iron_total_ugL.1)
La_month_line_hope <- hope_monthlineplots(lanthanum_total_ugL.1)
Pb_month_line_hope <- hope_monthlineplots(lead_total_ugL.1)
Li_month_line_hope <- hope_monthlineplots(lithium_total_ugL.1)
Lu_month_line_hope <- hope_monthlineplots(lutetium_total_ugL.1)
Mn_month_line_hope <- hope_monthlineplots(manganese_total_ugL.1)
Mo_month_line_hope <- hope_monthlineplots(molybdenum_total_ugL.1)
Nd_month_line_hope <- hope_monthlineplots(neodymium_total_ugL.1)
Ni_month_line_hope <- hope_monthlineplots(nickel_total_ugL.1)
Nb_month_line_hope <- hope_monthlineplots(niobium_total_ugL.1)
Pd_month_line_hope <- hope_monthlineplots(palladium_total_ugL.1)
Pt_month_line_hope <- hope_monthlineplots(platinum_total_ugL.1)
Pr_month_line_hope <- hope_monthlineplots(praseodymium_total_ugL.1)
Rb_month_line_hope <- hope_monthlineplots(rubidium_total_ugL.1)
Ru_month_line_hope <- hope_monthlineplots(ruthenium_total_ugL.1)
Sm_month_line_hope <- hope_monthlineplots(samarium_total_ugL.1)
Sc_month_line_hope <- hope_monthlineplots(scandium_total_ugL.1)
Se_month_line_hope <- hope_monthlineplots(selenium_total_ugL.1)
Ag_month_line_hope <- hope_monthlineplots(silver_total_ugL.1)
Sr_month_line_hope <- hope_monthlineplots(strontium_total_ugL.1)
Te_month_line_hope <- hope_monthlineplots(tellurium_total_ugL.1)
Tb_month_line_hope <- hope_monthlineplots(terbium_total_ugL.1)
Tl_month_line_hope <- hope_monthlineplots(thallium_total_ugL.1)
Sn_month_line_hope <- hope_monthlineplots(tin_total_ugL.1)
Ti_month_line_hope <- hope_monthlineplots(titanium_total_ugL.1)
W_month_line_hope <- hope_monthlineplots(tungsten_total_ugL.1)
U_month_line_hope <- hope_monthlineplots(uranium_total_ugL.1)
V_month_line_hope <- hope_monthlineplots(vanadium_total_ugL.1)
Yb_month_line_hope <- hope_monthlineplots(ytterbium_total_ugL.1)
Y_month_line_hope <- hope_monthlineplots(yttrium_total_ugL.1)
Zn_month_line_hope <- hope_monthlineplots(zinc_total_ugL.1)
Zr_month_line_hope <- hope_monthlineplots(zirconium_total_ugL.1)

allmetals_month_line_hope <-
  ggarrange(Ag_month_line_hope,
            Al_month_line_hope,
            As_month_line_hope,
            B_month_line_hope,
            Ba_month_line_hope,
            Be_month_line_hope,
            Bi_month_line_hope,
            Cd_month_line_hope,
            Ce_month_line_hope,
            Co_month_line_hope,
            Cr_month_line_hope,
            Cs_month_line_hope,
            Cu_month_line_hope,
            Eu_month_line_hope,
            Fe_month_line_hope,
            Ga_month_line_hope,
            Gd_month_line_hope,
            Ge_month_line_hope,
            Hf_month_line_hope,
            Ho_month_line_hope,
            In_month_line_hope,
            La_month_line_hope,
            Li_month_line_hope,
            Lu_month_line_hope,
            Mn_month_line_hope,
            Mo_month_line_hope,
            Nb_month_line_hope,
            Nd_month_line_hope,
            Ni_month_line_hope,
            Pb_month_line_hope,
            Pd_month_line_hope,
            Pr_month_line_hope,
            Pt_month_line_hope,
            Rb_month_line_hope,
            Ru_month_line_hope,
            Sb_month_line_hope,
            Sc_month_line_hope,
            Se_month_line_hope,
            Sm_month_line_hope,
            Sn_month_line_hope,
            Sr_month_line_hope,
            Tb_month_line_hope,
            Te_month_line_hope,
            Ti_month_line_hope,
            Tl_month_line_hope,
            U_month_line_hope,
            V_month_line_hope,
            W_month_line_hope,
            Y_month_line_hope,
            Yb_month_line_hope,
            Zn_month_line_hope,
            Zr_month_line_hope,
            ncol = 8, nrow = 7)

print(allmetals_month_line_hope)

metals_month_line_hope <-
  ggarrange(Ag_month_line_hope,
            Al_month_line_hope,
            As_month_line_hope,
            B_month_line_hope,
            Ba_month_line_hope,
            Be_month_line_hope,
            Bi_month_line_hope,
            Cd_month_line_hope,
            Ce_month_line_hope,
            Co_month_line_hope,
            Cr_month_line_hope,
            Cs_month_line_hope,
            Cu_month_line_hope,
            Fe_month_line_hope,
            Ga_month_line_hope,
            La_month_line_hope,
            Li_month_line_hope,
            Mn_month_line_hope,
            Mo_month_line_hope,
            Nb_month_line_hope,
            Ni_month_line_hope,
            Pb_month_line_hope,
            Pt_month_line_hope,
            Rb_month_line_hope,
            Sb_month_line_hope,
            Se_month_line_hope,
            Sn_month_line_hope,
            Sr_month_line_hope,
            Ti_month_line_hope,
            Tl_month_line_hope,
            U_month_line_hope,
            V_month_line_hope,
            W_month_line_hope,
            Y_month_line_hope,
            Zn_month_line_hope,
            ncol = 6, nrow = 6)

print(metals_month_line_hope)

##Plotting by individual element NEED TO CHANGE THIS TO HOPE

weektimeseries <- function(y) {
  ggplot() +
    geom_point(data = metals,
               aes(x = week,
                   y= {{y}}),
               alpha = 1/5)+
    geom_line(data = meanweeks,
              aes(x = week,
                  y= {{y}},
                  group = 1)) +
    theme_light()
}

monthtimeseries <- function(y) {
  ggplot() +
    geom_point(data = metals,
               aes(x = month,
                   y= {{y}}),
               alpha = 1/5)+
    geom_line(data = meanmonths,
              aes(x = month,
                  y= {{y}},
                  group = 1)) +
    theme_light()
}

Al_week <- weektimeseries(aluminum_total_ugL.1)
Al_month <- monthtimeseries(aluminum_total_ugL.1)


##GRAVESEND REACH

gravesendmetals1 <- read.csv("gravesendmetals1.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
gravesendmetals2 <- read.csv("gravesendmetals2.csv")%>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
gravesendmetals3 <- read.csv("gravesendmetals3.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
gravesendmetals4 <- read.csv("gravesendmetals4.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
gravesendmetals5 <- read.csv("gravesendmetals5.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)
gravesendmetals6 <- read.csv("gravesendmetals6.csv") %>%
  rename("sample_time" = 1,
         "sample_number" = 2,
         "sample_type" = 3)

##Joining to one sheet

gravesendmetals <- gravesendmetals1 %>%
  full_join(gravesendmetals2, 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(gravesendmetals3, 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(gravesendmetals4, 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(gravesendmetals5, 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join(gravesendmetals6, 
            by = c("sample_time", "sample_number", "sample_type"))

##Removing an error variable
gravesendmetals <- gravesendmetals %>%
  filter(if_all(everything(), ~!grepl(-999999.000, .)))

##Adding week and month variable to metals df
gravesendmetals <- gravesendmetals %>%
  mutate(week = lubridate::isoweek(sample_time),
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))


##Creating mean dataframes for weeks and months
gravesend_meanweeks <-
  gravesendmetals %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

gravesend_meanmonths <-
  gravesendmetals %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

#Plotting week line plots

gravesend_weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = gravesend_meanweeks,
              aes(x = week,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,52,13)) +
    theme_light()
}

Al_week_line_gravesend <- gravesend_weeklineplots(aluminum_total_ugL.1)
Sb_week_line_gravesend <- gravesend_weeklineplots(antimony_total_ugL.1)
As_week_line_gravesend <- gravesend_weeklineplots(arsenic_total_ugL.1)
Ba_week_line_gravesend <- gravesend_weeklineplots(barium_total_ugL.1)
Be_week_line_gravesend <- gravesend_weeklineplots(beryllium_total_ugL.1)
Bi_week_line_gravesend <- gravesend_weeklineplots(bismuth_total_ugL.1)
B_week_line_gravesend <- gravesend_weeklineplots(boron_total_ugL.1)
Cd_week_line_gravesend <- gravesend_weeklineplots(cadmium_total_ugL.1)
Ce_week_line_gravesend <- gravesend_weeklineplots(cerium_total_ugL.1)
Cs_week_line_gravesend <- gravesend_weeklineplots(cesium_total_ugL.1)
Cr_week_line_gravesend <- gravesend_weeklineplots(chromium_total_ugL.1)
Co_week_line_gravesend <- gravesend_weeklineplots(cobalt_total_ugL.1)
Cu_week_line_gravesend <- gravesend_weeklineplots(copper_total_ugL.1)
Eu_week_line_gravesend <-gravesend_weeklineplots(europium_total_ugL.1)
Gd_week_line_gravesend <- gravesend_weeklineplots(gadolinium_totalrecoverable_ugL.1)
Ga_week_line_gravesend <- gravesend_weeklineplots(gallium_total_ugL.1)
Ge_week_line_gravesend <- gravesend_weeklineplots(germanium_total_ugL.1)
Hf_week_line_gravesend <- gravesend_weeklineplots(hafnium_total_ugL.1)
Ho_week_line_gravesend <- gravesend_weeklineplots(holmium_total_ugL.1)
In_week_line_gravesend <- gravesend_weeklineplots(indium_total_ugL.1)
Fe_week_line_gravesend <- gravesend_weeklineplots(iron_total_ugL.1)
La_week_line_gravesend <- gravesend_weeklineplots(lanthanum_total_ugL.1)
Pb_week_line_gravesend <- gravesend_weeklineplots(lead_total_ugL.1)
Li_week_line_gravesend <- gravesend_weeklineplots(lithium_total_ugL.1)
Lu_week_line_gravesend <- gravesend_weeklineplots(lutetium_total_ugL.1)
Mn_week_line_gravesend <- gravesend_weeklineplots(manganese_total_ugL.1)
Mo_week_line_gravesend <- gravesend_weeklineplots(molybdenum_total_ugL.1)
Nd_week_line_gravesend <- gravesend_weeklineplots(neodymium_total_ugL.1)
Ni_week_line_gravesend <- gravesend_weeklineplots(nickel_total_ugL.1)
Nb_week_line_gravesend <- gravesend_weeklineplots(niobium_total_ugL.1)
Pd_week_line_gravesend <- gravesend_weeklineplots(palladium_total_ugL.1)
Pt_week_line_gravesend <- gravesend_weeklineplots(platinum_total_ugL.1)
Pr_week_line_gravesend <- gravesend_weeklineplots(praseodymium_total_ugL.1)
Rb_week_line_gravesend <- gravesend_weeklineplots(rubidium_total_ugL.1)
Ru_week_line_gravesend <- gravesend_weeklineplots(ruthenium_total_ugL.1)
Sm_week_line_gravesend <- gravesend_weeklineplots(samarium_total_ugL.1)
Sc_week_line_gravesend <- gravesend_weeklineplots(scandium_total_ugL.1)
Se_week_line_gravesend <- gravesend_weeklineplots(selenium_total_ugL.1)
Ag_week_line_gravesend <- gravesend_weeklineplots(silver_total_ugL.1)
Sr_week_line_gravesend <- gravesend_weeklineplots(strontium_total_ugL.1)
Te_week_line_gravesend <- gravesend_weeklineplots(tellurium_total_ugL.1)
Tb_week_line_gravesend <- gravesend_weeklineplots(terbium_total_ugL.1)
Tl_week_line_gravesend <- gravesend_weeklineplots(thallium_total_ugL.1)
Sn_week_line_gravesend <- gravesend_weeklineplots(tin_total_ugL.1)
Ti_week_line_gravesend <- gravesend_weeklineplots(titanium_total_ugL.1)
W_week_line_gravesend <- gravesend_weeklineplots(tungsten_total_ugL.1)
U_week_line_gravesend <- gravesend_weeklineplots(uranium_total_ugL.1)
V_week_line_gravesend <- gravesend_weeklineplots(vanadium_total_ugL.1)
Yb_week_line_gravesend <- gravesend_weeklineplots(ytterbium_total_ugL.1)
Y_week_line_gravesend <- gravesend_weeklineplots(yttrium_total_ugL.1)
Zn_week_line_gravesend <- gravesend_weeklineplots(zinc_total_ugL.1)
Zr_week_line_gravesend <- gravesend_weeklineplots(zirconium_total_ugL.1)

allmetals_week_line_gravesend <-
  ggarrange(Ag_week_line_gravesend,
            Al_week_line_gravesend,
            As_week_line_gravesend,
            B_week_line_gravesend,
            Ba_week_line_gravesend,
            Be_week_line_gravesend,
            Bi_week_line_gravesend,
            Cd_week_line_gravesend,
            Ce_week_line_gravesend,
            Co_week_line_gravesend,
            Cr_week_line_gravesend,
            Cs_week_line_gravesend,
            Cu_week_line_gravesend,
            Eu_week_line_gravesend,
            Fe_week_line_gravesend,
            Ga_week_line_gravesend,
            Gd_week_line_gravesend,
            Ge_week_line_gravesend,
            Hf_week_line_gravesend,
            Ho_week_line_gravesend,
            In_week_line_gravesend,
            La_week_line_gravesend,
            Li_week_line_gravesend,
            Lu_week_line_gravesend,
            Mn_week_line_gravesend,
            Mo_week_line_gravesend,
            Nb_week_line_gravesend,
            Nd_week_line_gravesend,
            Ni_week_line_gravesend,
            Pb_week_line_gravesend,
            Pd_week_line_gravesend,
            Pr_week_line_gravesend,
            Pt_week_line_gravesend,
            Rb_week_line_gravesend,
            Ru_week_line_gravesend,
            Sb_week_line_gravesend,
            Sc_week_line_gravesend,
            Se_week_line_gravesend,
            Sm_week_line_gravesend,
            Sn_week_line_gravesend,
            Sr_week_line_gravesend,
            Tb_week_line_gravesend,
            Te_week_line_gravesend,
            Ti_week_line_gravesend,
            Tl_week_line_gravesend,
            U_week_line_gravesend,
            V_week_line_gravesend,
            W_week_line_gravesend,
            Y_week_line_gravesend,
            Yb_week_line_gravesend,
            Zn_week_line_gravesend,
            Zr_week_line_gravesend,
            ncol = 8, nrow = 7)

print(allmetals_week_line_gravesend) #There are a bunch without any data so making a version without those

metals_week_line_gravesend <-
  ggarrange(Ag_week_line_gravesend,
            Al_week_line_gravesend,
            As_week_line_gravesend,
            B_week_line_gravesend,
            Ba_week_line_gravesend,
            Be_week_line_gravesend,
            Bi_week_line_gravesend,
            Cd_week_line_gravesend,
            Ce_week_line_gravesend,
            Co_week_line_gravesend,
            Cr_week_line_gravesend,
            Cs_week_line_gravesend,
            Cu_week_line_gravesend,
            Fe_week_line_gravesend,
            Ga_week_line_gravesend,
            La_week_line_gravesend,
            Li_week_line_gravesend,
            Mn_week_line_gravesend,
            Mo_week_line_gravesend,
            Nb_week_line_gravesend,
            Ni_week_line_gravesend,
            Pb_week_line_gravesend,
            Pt_week_line_gravesend,
            Rb_week_line_gravesend,
            Sb_week_line_gravesend,
            Se_week_line_gravesend,
            Sn_week_line_gravesend,
            Sr_week_line_gravesend,
            Ti_week_line_gravesend,
            Tl_week_line_gravesend,
            U_week_line_gravesend,
            V_week_line_gravesend,
            W_week_line_gravesend,
            Y_week_line_gravesend,
            Zn_week_line_gravesend,
            ncol = 6, nrow = 6)

print(metals_week_line_gravesend)

#By month
gravesend_monthlineplots <- function(y) {
  ggplot() +
    geom_line(data = gravesend_meanmonths,
              aes(x = month,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,12,4)) +
    theme_light()
}

Al_month_line_gravesend <- gravesend_monthlineplots(aluminum_total_ugL.1)
Sb_month_line_gravesend <- gravesend_monthlineplots(antimony_total_ugL.1)
As_month_line_gravesend <- gravesend_monthlineplots(arsenic_total_ugL.1)
Ba_month_line_gravesend <- gravesend_monthlineplots(barium_total_ugL.1)
Be_month_line_gravesend <- gravesend_monthlineplots(beryllium_total_ugL.1)
Bi_month_line_gravesend <- gravesend_monthlineplots(bismuth_total_ugL.1)
B_month_line_gravesend <- gravesend_monthlineplots(boron_total_ugL.1)
Cd_month_line_gravesend <- gravesend_monthlineplots(cadmium_total_ugL.1)
Ce_month_line_gravesend <- gravesend_monthlineplots(cerium_total_ugL.1)
Cs_month_line_gravesend <- gravesend_monthlineplots(cesium_total_ugL.1)
Cr_month_line_gravesend <- gravesend_monthlineplots(chromium_total_ugL.1)
Co_month_line_gravesend <- gravesend_monthlineplots(cobalt_total_ugL.1)
Cu_month_line_gravesend <- gravesend_monthlineplots(copper_total_ugL.1)
Eu_month_line_gravesend <-gravesend_monthlineplots(europium_total_ugL.1)
Gd_month_line_gravesend <- gravesend_monthlineplots(gadolinium_totalrecoverable_ugL.1)
Ga_month_line_gravesend <- gravesend_monthlineplots(gallium_total_ugL.1)
Ge_month_line_gravesend <- gravesend_monthlineplots(germanium_total_ugL.1)
Hf_month_line_gravesend <- gravesend_monthlineplots(hafnium_total_ugL.1)
Ho_month_line_gravesend <- gravesend_monthlineplots(holmium_total_ugL.1)
In_month_line_gravesend <- gravesend_monthlineplots(indium_total_ugL.1)
Fe_month_line_gravesend <- gravesend_monthlineplots(iron_total_ugL.1)
La_month_line_gravesend <- gravesend_monthlineplots(lanthanum_total_ugL.1)
Pb_month_line_gravesend <- gravesend_monthlineplots(lead_total_ugL.1)
Li_month_line_gravesend <- gravesend_monthlineplots(lithium_total_ugL.1)
Lu_month_line_gravesend <- gravesend_monthlineplots(lutetium_total_ugL.1)
Mn_month_line_gravesend <- gravesend_monthlineplots(manganese_total_ugL.1)
Mo_month_line_gravesend <- gravesend_monthlineplots(molybdenum_total_ugL.1)
Nd_month_line_gravesend <- gravesend_monthlineplots(neodymium_total_ugL.1)
Ni_month_line_gravesend <- gravesend_monthlineplots(nickel_total_ugL.1)
Nb_month_line_gravesend <- gravesend_monthlineplots(niobium_total_ugL.1)
Pd_month_line_gravesend <- gravesend_monthlineplots(palladium_total_ugL.1)
Pt_month_line_gravesend <- gravesend_monthlineplots(platinum_total_ugL.1)
Pr_month_line_gravesend <- gravesend_monthlineplots(praseodymium_total_ugL.1)
Rb_month_line_gravesend <- gravesend_monthlineplots(rubidium_total_ugL.1)
Ru_month_line_gravesend <- gravesend_monthlineplots(ruthenium_total_ugL.1)
Sm_month_line_gravesend <- gravesend_monthlineplots(samarium_total_ugL.1)
Sc_month_line_gravesend <- gravesend_monthlineplots(scandium_total_ugL.1)
Se_month_line_gravesend <- gravesend_monthlineplots(selenium_total_ugL.1)
Ag_month_line_gravesend <- gravesend_monthlineplots(silver_total_ugL.1)
Sr_month_line_gravesend <- gravesend_monthlineplots(strontium_total_ugL.1)
Te_month_line_gravesend <- gravesend_monthlineplots(tellurium_total_ugL.1)
Tb_month_line_gravesend <- gravesend_monthlineplots(terbium_total_ugL.1)
Tl_month_line_gravesend <- gravesend_monthlineplots(thallium_total_ugL.1)
Sn_month_line_gravesend <- gravesend_monthlineplots(tin_total_ugL.1)
Ti_month_line_gravesend <- gravesend_monthlineplots(titanium_total_ugL.1)
W_month_line_gravesend <- gravesend_monthlineplots(tungsten_total_ugL.1)
U_month_line_gravesend <- gravesend_monthlineplots(uranium_total_ugL.1)
V_month_line_gravesend <- gravesend_monthlineplots(vanadium_total_ugL.1)
Yb_month_line_gravesend <- gravesend_monthlineplots(ytterbium_total_ugL.1)
Y_month_line_gravesend <- gravesend_monthlineplots(yttrium_total_ugL.1)
Zn_month_line_gravesend <- gravesend_monthlineplots(zinc_total_ugL.1)
Zr_month_line_gravesend <- gravesend_monthlineplots(zirconium_total_ugL.1)

metals_month_line_gravesend <-
  ggarrange(Ag_month_line_gravesend,
            Al_month_line_gravesend,
            As_month_line_gravesend,
            B_month_line_gravesend,
            Ba_month_line_gravesend,
            Be_month_line_gravesend,
            Bi_month_line_gravesend,
            Cd_month_line_gravesend,
            Ce_month_line_gravesend,
            Co_month_line_gravesend,
            Cr_month_line_gravesend,
            Cs_month_line_gravesend,
            Cu_month_line_gravesend,
            Fe_month_line_gravesend,
            Ga_month_line_gravesend,
            La_month_line_gravesend,
            Li_month_line_gravesend,
            Mn_month_line_gravesend,
            Mo_month_line_gravesend,
            Nb_month_line_gravesend,
            Ni_month_line_gravesend,
            Pb_month_line_gravesend,
            Pt_month_line_gravesend,
            Rb_month_line_gravesend,
            Sb_month_line_gravesend,
            Se_month_line_gravesend,
            Sn_month_line_gravesend,
            Sr_month_line_gravesend,
            Ti_month_line_gravesend,
            Tl_month_line_gravesend,
            U_month_line_gravesend,
            V_month_line_gravesend,
            W_month_line_gravesend,
            Y_month_line_gravesend,
            Zn_month_line_gravesend,
            ncol = 6, nrow = 6)

print(metals_month_line_gravesend)
