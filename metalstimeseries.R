library(tidyverse)
library(dplyr)
library(ggpubr)

##Loading all hope datasheets and joining into one csv

hopemetals <- (read.csv("hopemetals1.csv") %>%
                 rename("sample_time" = 1)) %>%
  full_join((read.csv("hopemetals2.csv")%>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("hopemetals3.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("hopemetals4.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("hopemetals5.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("hopemetals6.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
             by = c("sample_time", "sample_number", "sample_type")) %>%
  filter(if_all(everything(), ~!grepl(-999999.000, .))) %>% #Removing an error variable
  mutate(week = lubridate::isoweek(sample_time),            #Adding week and month variables
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))

##Creating mean dataframes for weeks and months
hopemetals_meanweeks <-
  hopemetals %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

hopemetals_meanmonths <-
  hopemetals %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))


##Plotting all elements by week (average lines only for facetting)
hopemetals_weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = hopemetals_meanweeks,
              aes(x = week,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,52,13)) +
    theme_light()
}

Al_week_line_hope <- hopemetals_weeklineplots(aluminum_total_ugL.1)
Sb_week_line_hope <- hopemetals_weeklineplots(antimony_total_ugL.1)
As_week_line_hope <- hopemetals_weeklineplots(arsenic_total_ugL.1)
Ba_week_line_hope <- hopemetals_weeklineplots(barium_total_ugL.1)
Be_week_line_hope <- hopemetals_weeklineplots(beryllium_total_ugL.1)
Bi_week_line_hope <- hopemetals_weeklineplots(bismuth_total_ugL.1)
B_week_line_hope <- hopemetals_weeklineplots(boron_total_ugL.1)
Cd_week_line_hope <- hopemetals_weeklineplots(cadmium_total_ugL.1)
Ce_week_line_hope <- hopemetals_weeklineplots(cerium_total_ugL.1)
Cs_week_line_hope <- hopemetals_weeklineplots(cesium_total_ugL.1)
Cr_week_line_hope <- hopemetals_weeklineplots(chromium_total_ugL.1)
Co_week_line_hope <- hopemetals_weeklineplots(cobalt_total_ugL.1)
Cu_week_line_hope <- hopemetals_weeklineplots(copper_total_ugL.1)
Eu_week_line_hope <-hopemetals_weeklineplots(europium_total_ugL.1)
Gd_week_line_hope <- hopemetals_weeklineplots(gadolinium_totalrecoverable_ugL.1)
Ga_week_line_hope <- hopemetals_weeklineplots(gallium_total_ugL.1)
Ge_week_line_hope <- hopemetals_weeklineplots(germanium_total_ugL.1)
Hf_week_line_hope <- hopemetals_weeklineplots(hafnium_total_ugL.1)
Ho_week_line_hope <- hopemetals_weeklineplots(holmium_total_ugL.1)
In_week_line_hope <- hopemetals_weeklineplots(indium_total_ugL.1)
Fe_week_line_hope <- hopemetals_weeklineplots(iron_total_ugL.1)
La_week_line_hope <- hopemetals_weeklineplots(lanthanum_total_ugL.1)
Pb_week_line_hope <- hopemetals_weeklineplots(lead_total_ugL.1)
Li_week_line_hope <- hopemetals_weeklineplots(lithium_total_ugL.1)
Lu_week_line_hope <- hopemetals_weeklineplots(lutetium_total_ugL.1)
Mn_week_line_hope <- hopemetals_weeklineplots(manganese_total_ugL.1)
Mo_week_line_hope <- hopemetals_weeklineplots(molybdenum_total_ugL.1)
Nd_week_line_hope <- hopemetals_weeklineplots(neodymium_total_ugL.1)
Ni_week_line_hope <- hopemetals_weeklineplots(nickel_total_ugL.1)
Nb_week_line_hope <- hopemetals_weeklineplots(niobium_total_ugL.1)
Pd_week_line_hope <- hopemetals_weeklineplots(palladium_total_ugL.1)
Pt_week_line_hope <- hopemetals_weeklineplots(platinum_total_ugL.1)
Pr_week_line_hope <- hopemetals_weeklineplots(praseodymium_total_ugL.1)
Rb_week_line_hope <- hopemetals_weeklineplots(rubidium_total_ugL.1)
Ru_week_line_hope <- hopemetals_weeklineplots(ruthenium_total_ugL.1)
Sm_week_line_hope <- hopemetals_weeklineplots(samarium_total_ugL.1)
Sc_week_line_hope <- hopemetals_weeklineplots(scandium_total_ugL.1)
Se_week_line_hope <- hopemetals_weeklineplots(selenium_total_ugL.1)
Ag_week_line_hope <- hopemetals_weeklineplots(silver_total_ugL.1)
Sr_week_line_hope <- hopemetals_weeklineplots(strontium_total_ugL.1)
Te_week_line_hope <- hopemetals_weeklineplots(tellurium_total_ugL.1)
Tb_week_line_hope <- hopemetals_weeklineplots(terbium_total_ugL.1)
Tl_week_line_hope <- hopemetals_weeklineplots(thallium_total_ugL.1)
Sn_week_line_hope <- hopemetals_weeklineplots(tin_total_ugL.1)
Ti_week_line_hope <- hopemetals_weeklineplots(titanium_total_ugL.1)
W_week_line_hope <- hopemetals_weeklineplots(tungsten_total_ugL.1)
U_week_line_hope <- hopemetals_weeklineplots(uranium_total_ugL.1)
V_week_line_hope <- hopemetals_weeklineplots(vanadium_total_ugL.1)
Yb_week_line_hope <- hopemetals_weeklineplots(ytterbium_total_ugL.1)
Y_week_line_hope <- hopemetals_weeklineplots(yttrium_total_ugL.1)
Zn_week_line_hope <- hopemetals_weeklineplots(zinc_total_ugL.1)
Zr_week_line_hope <- hopemetals_weeklineplots(zirconium_total_ugL.1)

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
hopemetals_monthlineplots <- function(y) {
  ggplot() +
    geom_line(data = hopemetals_meanmonths,
              aes(x = month,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,12,4)) +
    theme_light()
}

Al_month_line_hope <- hopemetals_monthlineplots(aluminum_total_ugL.1)
Sb_month_line_hope <- hopemetals_monthlineplots(antimony_total_ugL.1)
As_month_line_hope <- hopemetals_monthlineplots(arsenic_total_ugL.1)
Ba_month_line_hope <- hopemetals_monthlineplots(barium_total_ugL.1)
Be_month_line_hope <- hopemetals_monthlineplots(beryllium_total_ugL.1)
Bi_month_line_hope <- hopemetals_monthlineplots(bismuth_total_ugL.1)
B_month_line_hope <- hopemetals_monthlineplots(boron_total_ugL.1)
Cd_month_line_hope <- hopemetals_monthlineplots(cadmium_total_ugL.1)
Ce_month_line_hope <- hopemetals_monthlineplots(cerium_total_ugL.1)
Cs_month_line_hope <- hopemetals_monthlineplots(cesium_total_ugL.1)
Cr_month_line_hope <- hopemetals_monthlineplots(chromium_total_ugL.1)
Co_month_line_hope <- hopemetals_monthlineplots(cobalt_total_ugL.1)
Cu_month_line_hope <- hopemetals_monthlineplots(copper_total_ugL.1)
Eu_month_line_hope <-hopemetals_monthlineplots(europium_total_ugL.1)
Gd_month_line_hope <- hopemetals_monthlineplots(gadolinium_totalrecoverable_ugL.1)
Ga_month_line_hope <- hopemetals_monthlineplots(gallium_total_ugL.1)
Ge_month_line_hope <- hopemetals_monthlineplots(germanium_total_ugL.1)
Hf_month_line_hope <- hopemetals_monthlineplots(hafnium_total_ugL.1)
Ho_month_line_hope <- hopemetals_monthlineplots(holmium_total_ugL.1)
In_month_line_hope <- hopemetals_monthlineplots(indium_total_ugL.1)
Fe_month_line_hope <- hopemetals_monthlineplots(iron_total_ugL.1)
La_month_line_hope <- hopemetals_monthlineplots(lanthanum_total_ugL.1)
Pb_month_line_hope <- hopemetals_monthlineplots(lead_total_ugL.1)
Li_month_line_hope <- hopemetals_monthlineplots(lithium_total_ugL.1)
Lu_month_line_hope <- hopemetals_monthlineplots(lutetium_total_ugL.1)
Mn_month_line_hope <- hopemetals_monthlineplots(manganese_total_ugL.1)
Mo_month_line_hope <- hopemetals_monthlineplots(molybdenum_total_ugL.1)
Nd_month_line_hope <- hopemetals_monthlineplots(neodymium_total_ugL.1)
Ni_month_line_hope <- hopemetals_monthlineplots(nickel_total_ugL.1)
Nb_month_line_hope <- hopemetals_monthlineplots(niobium_total_ugL.1)
Pd_month_line_hope <- hopemetals_monthlineplots(palladium_total_ugL.1)
Pt_month_line_hope <- hopemetals_monthlineplots(platinum_total_ugL.1)
Pr_month_line_hope <- hopemetals_monthlineplots(praseodymium_total_ugL.1)
Rb_month_line_hope <- hopemetals_monthlineplots(rubidium_total_ugL.1)
Ru_month_line_hope <- hopemetals_monthlineplots(ruthenium_total_ugL.1)
Sm_month_line_hope <- hopemetals_monthlineplots(samarium_total_ugL.1)
Sc_month_line_hope <- hopemetals_monthlineplots(scandium_total_ugL.1)
Se_month_line_hope <- hopemetals_monthlineplots(selenium_total_ugL.1)
Ag_month_line_hope <- hopemetals_monthlineplots(silver_total_ugL.1)
Sr_month_line_hope <- hopemetals_monthlineplots(strontium_total_ugL.1)
Te_month_line_hope <- hopemetals_monthlineplots(tellurium_total_ugL.1)
Tb_month_line_hope <- hopemetals_monthlineplots(terbium_total_ugL.1)
Tl_month_line_hope <- hopemetals_monthlineplots(thallium_total_ugL.1)
Sn_month_line_hope <- hopemetals_monthlineplots(tin_total_ugL.1)
Ti_month_line_hope <- hopemetals_monthlineplots(titanium_total_ugL.1)
W_month_line_hope <- hopemetals_monthlineplots(tungsten_total_ugL.1)
U_month_line_hope <- hopemetals_monthlineplots(uranium_total_ugL.1)
V_month_line_hope <- hopemetals_monthlineplots(vanadium_total_ugL.1)
Yb_month_line_hope <- hopemetals_monthlineplots(ytterbium_total_ugL.1)
Y_month_line_hope <- hopemetals_monthlineplots(yttrium_total_ugL.1)
Zn_month_line_hope <- hopemetals_monthlineplots(zinc_total_ugL.1)
Zr_month_line_hope <- hopemetals_monthlineplots(zirconium_total_ugL.1)

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

##GRAVESEND REACH

#Loading datasheets and joining to a single sheet

gravesendmetals <- (read.csv("gravesendmetals1.csv") %>%
                      rename("sample_time" = 1,
                             "sample_number" = 2,
                             "sample_type" = 3)) %>%
  full_join((read.csv("gravesendmetals2.csv")%>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("gravesendmetals3.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("gravesendmetals4.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("gravesendmetals5.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  full_join((read.csv("gravesendmetals6.csv") %>%
               rename("sample_time" = 1,
                      "sample_number" = 2,
                      "sample_type" = 3)), 
            by = c("sample_time", "sample_number", "sample_type")) %>%
  filter(if_all(everything(), ~!grepl(-999999.000, .))) %>% #Removing an error variable
  mutate(week = lubridate::isoweek(sample_time),            #Adding week and month variable to metals df
         month = lubridate::month(sample_time)) %>%
  relocate(c("week", "month"), .after = sample_time) %>%
  mutate(week = factor(week),
         month = factor(month))


##Creating mean dataframes for weeks and months
gravesendmetals_meanweeks <-
  gravesendmetals %>%
  group_by(lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

gravesendmetals_meanmonths <-
  gravesendmetals %>%
  group_by(lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

#Plotting week line plots

gravesendmetals_weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = gravesendmetals_meanweeks,
              aes(x = week,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,52,13)) +
    theme_light()
}

Al_week_line_gravesend <- gravesendmetals_weeklineplots(aluminum_total_ugL.1)
Sb_week_line_gravesend <- gravesendmetals_weeklineplots(antimony_total_ugL.1)
As_week_line_gravesend <- gravesendmetals_weeklineplots(arsenic_total_ugL.1)
Ba_week_line_gravesend <- gravesendmetals_weeklineplots(barium_total_ugL.1)
Be_week_line_gravesend <- gravesendmetals_weeklineplots(beryllium_total_ugL.1)
Bi_week_line_gravesend <- gravesendmetals_weeklineplots(bismuth_total_ugL.1)
B_week_line_gravesend <- gravesendmetals_weeklineplots(boron_total_ugL.1)
Cd_week_line_gravesend <- gravesendmetals_weeklineplots(cadmium_total_ugL.1)
Ce_week_line_gravesend <- gravesendmetals_weeklineplots(cerium_total_ugL.1)
Cs_week_line_gravesend <- gravesendmetals_weeklineplots(cesium_total_ugL.1)
Cr_week_line_gravesend <- gravesendmetals_weeklineplots(chromium_total_ugL.1)
Co_week_line_gravesend <- gravesendmetals_weeklineplots(cobalt_total_ugL.1)
Cu_week_line_gravesend <- gravesendmetals_weeklineplots(copper_total_ugL.1)
Eu_week_line_gravesend <-gravesendmetals_weeklineplots(europium_total_ugL.1)
Gd_week_line_gravesend <- gravesendmetals_weeklineplots(gadolinium_totalrecoverable_ugL.1)
Ga_week_line_gravesend <- gravesendmetals_weeklineplots(gallium_total_ugL.1)
Ge_week_line_gravesend <- gravesendmetals_weeklineplots(germanium_total_ugL.1)
Hf_week_line_gravesend <- gravesendmetals_weeklineplots(hafnium_total_ugL.1)
Ho_week_line_gravesend <- gravesendmetals_weeklineplots(holmium_total_ugL.1)
In_week_line_gravesend <- gravesendmetals_weeklineplots(indium_total_ugL.1)
Fe_week_line_gravesend <- gravesendmetals_weeklineplots(iron_total_ugL.1)
La_week_line_gravesend <- gravesendmetals_weeklineplots(lanthanum_total_ugL.1)
Pb_week_line_gravesend <- gravesendmetals_weeklineplots(lead_total_ugL.1)
Li_week_line_gravesend <- gravesendmetals_weeklineplots(lithium_total_ugL.1)
Lu_week_line_gravesend <- gravesendmetals_weeklineplots(lutetium_total_ugL.1)
Mn_week_line_gravesend <- gravesendmetals_weeklineplots(manganese_total_ugL.1)
Mo_week_line_gravesend <- gravesendmetals_weeklineplots(molybdenum_total_ugL.1)
Nd_week_line_gravesend <- gravesendmetals_weeklineplots(neodymium_total_ugL.1)
Ni_week_line_gravesend <- gravesendmetals_weeklineplots(nickel_total_ugL.1)
Nb_week_line_gravesend <- gravesendmetals_weeklineplots(niobium_total_ugL.1)
Pd_week_line_gravesend <- gravesendmetals_weeklineplots(palladium_total_ugL.1)
Pt_week_line_gravesend <- gravesendmetals_weeklineplots(platinum_total_ugL.1)
Pr_week_line_gravesend <- gravesendmetals_weeklineplots(praseodymium_total_ugL.1)
Rb_week_line_gravesend <- gravesendmetals_weeklineplots(rubidium_total_ugL.1)
Ru_week_line_gravesend <- gravesendmetals_weeklineplots(ruthenium_total_ugL.1)
Sm_week_line_gravesend <- gravesendmetals_weeklineplots(samarium_total_ugL.1)
Sc_week_line_gravesend <- gravesendmetals_weeklineplots(scandium_total_ugL.1)
Se_week_line_gravesend <- gravesendmetals_weeklineplots(selenium_total_ugL.1)
Ag_week_line_gravesend <- gravesendmetals_weeklineplots(silver_total_ugL.1)
Sr_week_line_gravesend <- gravesendmetals_weeklineplots(strontium_total_ugL.1)
Te_week_line_gravesend <- gravesendmetals_weeklineplots(tellurium_total_ugL.1)
Tb_week_line_gravesend <- gravesendmetals_weeklineplots(terbium_total_ugL.1)
Tl_week_line_gravesend <- gravesendmetals_weeklineplots(thallium_total_ugL.1)
Sn_week_line_gravesend <- gravesendmetals_weeklineplots(tin_total_ugL.1)
Ti_week_line_gravesend <- gravesendmetals_weeklineplots(titanium_total_ugL.1)
W_week_line_gravesend <- gravesendmetals_weeklineplots(tungsten_total_ugL.1)
U_week_line_gravesend <- gravesendmetals_weeklineplots(uranium_total_ugL.1)
V_week_line_gravesend <- gravesendmetals_weeklineplots(vanadium_total_ugL.1)
Yb_week_line_gravesend <- gravesendmetals_weeklineplots(ytterbium_total_ugL.1)
Y_week_line_gravesend <- gravesendmetals_weeklineplots(yttrium_total_ugL.1)
Zn_week_line_gravesend <- gravesendmetals_weeklineplots(zinc_total_ugL.1)
Zr_week_line_gravesend <- gravesendmetals_weeklineplots(zirconium_total_ugL.1)

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
gravesendmetals_monthlineplots <- function(y) {
  ggplot() +
    geom_line(data = gravesendmetals_meanmonths,
              aes(x = month,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,12,4)) +
    theme_light()
}

Al_month_line_gravesend <- gravesendmetals_monthlineplots(aluminum_total_ugL.1)
Sb_month_line_gravesend <- gravesendmetals_monthlineplots(antimony_total_ugL.1)
As_month_line_gravesend <- gravesendmetals_monthlineplots(arsenic_total_ugL.1)
Ba_month_line_gravesend <- gravesendmetals_monthlineplots(barium_total_ugL.1)
Be_month_line_gravesend <- gravesendmetals_monthlineplots(beryllium_total_ugL.1)
Bi_month_line_gravesend <- gravesendmetals_monthlineplots(bismuth_total_ugL.1)
B_month_line_gravesend <- gravesendmetals_monthlineplots(boron_total_ugL.1)
Cd_month_line_gravesend <- gravesendmetals_monthlineplots(cadmium_total_ugL.1)
Ce_month_line_gravesend <- gravesendmetals_monthlineplots(cerium_total_ugL.1)
Cs_month_line_gravesend <- gravesendmetals_monthlineplots(cesium_total_ugL.1)
Cr_month_line_gravesend <- gravesendmetals_monthlineplots(chromium_total_ugL.1)
Co_month_line_gravesend <- gravesendmetals_monthlineplots(cobalt_total_ugL.1)
Cu_month_line_gravesend <- gravesendmetals_monthlineplots(copper_total_ugL.1)
Eu_month_line_gravesend <-gravesendmetals_monthlineplots(europium_total_ugL.1)
Gd_month_line_gravesend <- gravesendmetals_monthlineplots(gadolinium_totalrecoverable_ugL.1)
Ga_month_line_gravesend <- gravesendmetals_monthlineplots(gallium_total_ugL.1)
Ge_month_line_gravesend <- gravesendmetals_monthlineplots(germanium_total_ugL.1)
Hf_month_line_gravesend <- gravesendmetals_monthlineplots(hafnium_total_ugL.1)
Ho_month_line_gravesend <- gravesendmetals_monthlineplots(holmium_total_ugL.1)
In_month_line_gravesend <- gravesendmetals_monthlineplots(indium_total_ugL.1)
Fe_month_line_gravesend <- gravesendmetals_monthlineplots(iron_total_ugL.1)
La_month_line_gravesend <- gravesendmetals_monthlineplots(lanthanum_total_ugL.1)
Pb_month_line_gravesend <- gravesendmetals_monthlineplots(lead_total_ugL.1)
Li_month_line_gravesend <- gravesendmetals_monthlineplots(lithium_total_ugL.1)
Lu_month_line_gravesend <- gravesendmetals_monthlineplots(lutetium_total_ugL.1)
Mn_month_line_gravesend <- gravesendmetals_monthlineplots(manganese_total_ugL.1)
Mo_month_line_gravesend <- gravesendmetals_monthlineplots(molybdenum_total_ugL.1)
Nd_month_line_gravesend <- gravesendmetals_monthlineplots(neodymium_total_ugL.1)
Ni_month_line_gravesend <- gravesendmetals_monthlineplots(nickel_total_ugL.1)
Nb_month_line_gravesend <- gravesendmetals_monthlineplots(niobium_total_ugL.1)
Pd_month_line_gravesend <- gravesendmetals_monthlineplots(palladium_total_ugL.1)
Pt_month_line_gravesend <- gravesendmetals_monthlineplots(platinum_total_ugL.1)
Pr_month_line_gravesend <- gravesendmetals_monthlineplots(praseodymium_total_ugL.1)
Rb_month_line_gravesend <- gravesendmetals_monthlineplots(rubidium_total_ugL.1)
Ru_month_line_gravesend <- gravesendmetals_monthlineplots(ruthenium_total_ugL.1)
Sm_month_line_gravesend <- gravesendmetals_monthlineplots(samarium_total_ugL.1)
Sc_month_line_gravesend <- gravesendmetals_monthlineplots(scandium_total_ugL.1)
Se_month_line_gravesend <- gravesendmetals_monthlineplots(selenium_total_ugL.1)
Ag_month_line_gravesend <- gravesendmetals_monthlineplots(silver_total_ugL.1)
Sr_month_line_gravesend <- gravesendmetals_monthlineplots(strontium_total_ugL.1)
Te_month_line_gravesend <- gravesendmetals_monthlineplots(tellurium_total_ugL.1)
Tb_month_line_gravesend <- gravesendmetals_monthlineplots(terbium_total_ugL.1)
Tl_month_line_gravesend <- gravesendmetals_monthlineplots(thallium_total_ugL.1)
Sn_month_line_gravesend <- gravesendmetals_monthlineplots(tin_total_ugL.1)
Ti_month_line_gravesend <- gravesendmetals_monthlineplots(titanium_total_ugL.1)
W_month_line_gravesend <- gravesendmetals_monthlineplots(tungsten_total_ugL.1)
U_month_line_gravesend <- gravesendmetals_monthlineplots(uranium_total_ugL.1)
V_month_line_gravesend <- gravesendmetals_monthlineplots(vanadium_total_ugL.1)
Yb_month_line_gravesend <- gravesendmetals_monthlineplots(ytterbium_total_ugL.1)
Y_month_line_gravesend <- gravesendmetals_monthlineplots(yttrium_total_ugL.1)
Zn_month_line_gravesend <- gravesendmetals_monthlineplots(zinc_total_ugL.1)
Zr_month_line_gravesend <- gravesendmetals_monthlineplots(zirconium_total_ugL.1)

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

###Combining Hope and gravesend plots

hopemetalssite <-
hopemetals %>%
  mutate(site = "hope") %>%
  relocate(site, .after = month)

gravesendmetalssite <-
  gravesendmetals %>%
  mutate(site = "gravesend") %>%
  relocate(site, .after = month) %>%
  subset(select = -c(iridium_total_ugL.1))


metals <- full_join(hopemetalssite, gravesendmetalssite, 
            by = (colnames(hopemetalssite)))

metals_meanweeks <-
  metals %>%
  group_by(site, lubridate::isoweek(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("week" = "lubridate::isoweek(sample_time)") %>%
  mutate(week = factor(week))

metals_meanmonths <-
  metals %>%
  group_by(site, lubridate::month(sample_time)) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1),
               mean,
               na.rm=TRUE) %>%
  rename("month" = "lubridate::month(sample_time)") %>%
  mutate(month = factor(month))

#combined metals week line

metals_weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = metals_meanweeks,
              aes(x = week,
                  y={{y}},
                  group = site,
                  colour = site)) +
    scale_x_discrete(breaks=seq(1,52,13)) +
    theme_light()
}

Al_week_line <- metals_weeklineplots(aluminum_total_ugL.1)
Sb_week_line <- metals_weeklineplots(antimony_total_ugL.1)
As_week_line <- metals_weeklineplots(arsenic_total_ugL.1)
Ba_week_line <- metals_weeklineplots(barium_total_ugL.1)
Be_week_line <- metals_weeklineplots(beryllium_total_ugL.1)
Bi_week_line <- metals_weeklineplots(bismuth_total_ugL.1)
B_week_line <- metals_weeklineplots(boron_total_ugL.1)
Cd_week_line <- metals_weeklineplots(cadmium_total_ugL.1)
Ce_week_line <- metals_weeklineplots(cerium_total_ugL.1)
Cs_week_line <- metals_weeklineplots(cesium_total_ugL.1)
Cr_week_line <- metals_weeklineplots(chromium_total_ugL.1)
Co_week_line <- metals_weeklineplots(cobalt_total_ugL.1)
Cu_week_line <- metals_weeklineplots(copper_total_ugL.1)
Eu_week_line <-metals_weeklineplots(europium_total_ugL.1)
Gd_week_line <- metals_weeklineplots(gadolinium_totalrecoverable_ugL.1)
Ga_week_line <- metals_weeklineplots(gallium_total_ugL.1)
Ge_week_line <- metals_weeklineplots(germanium_total_ugL.1)
Hf_week_line <- metals_weeklineplots(hafnium_total_ugL.1)
Ho_week_line <- metals_weeklineplots(holmium_total_ugL.1)
In_week_line <- metals_weeklineplots(indium_total_ugL.1)
Fe_week_line <- metals_weeklineplots(iron_total_ugL.1)
La_week_line <- metals_weeklineplots(lanthanum_total_ugL.1)
Pb_week_line <- metals_weeklineplots(lead_total_ugL.1)
Li_week_line <- metals_weeklineplots(lithium_total_ugL.1)
Lu_week_line <- metals_weeklineplots(lutetium_total_ugL.1)
Mn_week_line <- metals_weeklineplots(manganese_total_ugL.1)
Mo_week_line <- metals_weeklineplots(molybdenum_total_ugL.1)
Nd_week_line <- metals_weeklineplots(neodymium_total_ugL.1)
Ni_week_line <- metals_weeklineplots(nickel_total_ugL.1)
Nb_week_line <- metals_weeklineplots(niobium_total_ugL.1)
Pd_week_line <- metals_weeklineplots(palladium_total_ugL.1)
Pt_week_line <- metals_weeklineplots(platinum_total_ugL.1)
Pr_week_line <- metals_weeklineplots(praseodymium_total_ugL.1)
Rb_week_line <- metals_weeklineplots(rubidium_total_ugL.1)
Ru_week_line <- metals_weeklineplots(ruthenium_total_ugL.1)
Sm_week_line <- metals_weeklineplots(samarium_total_ugL.1)
Sc_week_line <- metals_weeklineplots(scandium_total_ugL.1)
Se_week_line <- metals_weeklineplots(selenium_total_ugL.1)
Ag_week_line <- metals_weeklineplots(silver_total_ugL.1)
Sr_week_line <- metals_weeklineplots(strontium_total_ugL.1)
Te_week_line <- metals_weeklineplots(tellurium_total_ugL.1)
Tb_week_line <- metals_weeklineplots(terbium_total_ugL.1)
Tl_week_line <- metals_weeklineplots(thallium_total_ugL.1)
Sn_week_line <- metals_weeklineplots(tin_total_ugL.1)
Ti_week_line <- metals_weeklineplots(titanium_total_ugL.1)
W_week_line <- metals_weeklineplots(tungsten_total_ugL.1)
U_week_line <- metals_weeklineplots(uranium_total_ugL.1)
V_week_line <- metals_weeklineplots(vanadium_total_ugL.1)
Yb_week_line <- metals_weeklineplots(ytterbium_total_ugL.1)
Y_week_line <- metals_weeklineplots(yttrium_total_ugL.1)
Zn_week_line <- metals_weeklineplots(zinc_total_ugL.1)
Zr_week_line <- metals_weeklineplots(zirconium_total_ugL.1)

allmetals_week_line <-
  ggarrange(Ag_week_line,
            Al_week_line,
            As_week_line,
            B_week_line,
            Ba_week_line,
            Be_week_line,
            Bi_week_line,
            Cd_week_line,
            Ce_week_line,
            Co_week_line,
            Cr_week_line,
            Cs_week_line,
            Cu_week_line,
            Eu_week_line,
            Fe_week_line,
            Ga_week_line,
            Gd_week_line,
            Ge_week_line,
            Hf_week_line,
            Ho_week_line,
            In_week_line,
            La_week_line,
            Li_week_line,
            Lu_week_line,
            Mn_week_line,
            Mo_week_line,
            Nb_week_line,
            Nd_week_line,
            Ni_week_line,
            Pb_week_line,
            Pd_week_line,
            Pr_week_line,
            Pt_week_line,
            Rb_week_line,
            Ru_week_line,
            Sb_week_line,
            Sc_week_line,
            Se_week_line,
            Sm_week_line,
            Sn_week_line,
            Sr_week_line,
            Tb_week_line,
            Te_week_line,
            Ti_week_line,
            Tl_week_line,
            U_week_line,
            V_week_line,
            W_week_line,
            Y_week_line,
            Yb_week_line,
            Zn_week_line,
            Zr_week_line,
            ncol = 8, nrow = 7,
            common.legend = TRUE)

print(allmetals_week_line) #There are a bunch without any data so making a version without those

metals_week_line <-
  ggarrange(Ag_week_line,
            Al_week_line,
            As_week_line,
            B_week_line,
            Ba_week_line,
            Be_week_line,
            Bi_week_line,
            Cd_week_line,
            Ce_week_line,
            Co_week_line,
            Cr_week_line,
            Cs_week_line,
            Cu_week_line,
            Fe_week_line,
            Ga_week_line,
            La_week_line,
            Li_week_line,
            Mn_week_line,
            Mo_week_line,
            Nb_week_line,
            Ni_week_line,
            Pb_week_line,
            Pt_week_line,
            Rb_week_line,
            Sb_week_line,
            Se_week_line,
            Sn_week_line,
            Sr_week_line,
            Ti_week_line,
            Tl_week_line,
            U_week_line,
            V_week_line,
            W_week_line,
            Y_week_line,
            Zn_week_line,
            ncol = 6, nrow = 6,
            common.legend = TRUE,
            legend = "right")

print(metals_week_line)


#combined metals month line

metals_monthlineplots <- function(y) {
  ggplot() +
    geom_line(data = metals_meanmonths,
              aes(x = month,
                  y={{y}},
                  group = site,
                  colour = site)) +
    scale_x_discrete(breaks=seq(1,12,4)) +
    theme_light()
}

Al_month_line <- metals_monthlineplots(aluminum_total_ugL.1)
Sb_month_line <- metals_monthlineplots(antimony_total_ugL.1)
As_month_line <- metals_monthlineplots(arsenic_total_ugL.1)
Ba_month_line <- metals_monthlineplots(barium_total_ugL.1)
Be_month_line <- metals_monthlineplots(beryllium_total_ugL.1)
Bi_month_line <- metals_monthlineplots(bismuth_total_ugL.1)
B_month_line <- metals_monthlineplots(boron_total_ugL.1)
Cd_month_line <- metals_monthlineplots(cadmium_total_ugL.1)
Ce_month_line <- metals_monthlineplots(cerium_total_ugL.1)
Cs_month_line <- metals_monthlineplots(cesium_total_ugL.1)
Cr_month_line <- metals_monthlineplots(chromium_total_ugL.1)
Co_month_line <- metals_monthlineplots(cobalt_total_ugL.1)
Cu_month_line <- metals_monthlineplots(copper_total_ugL.1)
Eu_month_line <-metals_monthlineplots(europium_total_ugL.1)
Gd_month_line <- metals_monthlineplots(gadolinium_totalrecoverable_ugL.1)
Ga_month_line <- metals_monthlineplots(gallium_total_ugL.1)
Ge_month_line <- metals_monthlineplots(germanium_total_ugL.1)
Hf_month_line <- metals_monthlineplots(hafnium_total_ugL.1)
Ho_month_line <- metals_monthlineplots(holmium_total_ugL.1)
In_month_line <- metals_monthlineplots(indium_total_ugL.1)
Fe_month_line <- metals_monthlineplots(iron_total_ugL.1)
La_month_line <- metals_monthlineplots(lanthanum_total_ugL.1)
Pb_month_line <- metals_monthlineplots(lead_total_ugL.1)
Li_month_line <- metals_monthlineplots(lithium_total_ugL.1)
Lu_month_line <- metals_monthlineplots(lutetium_total_ugL.1)
Mn_month_line <- metals_monthlineplots(manganese_total_ugL.1)
Mo_month_line <- metals_monthlineplots(molybdenum_total_ugL.1)
Nd_month_line <- metals_monthlineplots(neodymium_total_ugL.1)
Ni_month_line <- metals_monthlineplots(nickel_total_ugL.1)
Nb_month_line <- metals_monthlineplots(niobium_total_ugL.1)
Pd_month_line <- metals_monthlineplots(palladium_total_ugL.1)
Pt_month_line <- metals_monthlineplots(platinum_total_ugL.1)
Pr_month_line <- metals_monthlineplots(praseodymium_total_ugL.1)
Rb_month_line <- metals_monthlineplots(rubidium_total_ugL.1)
Ru_month_line <- metals_monthlineplots(ruthenium_total_ugL.1)
Sm_month_line <- metals_monthlineplots(samarium_total_ugL.1)
Sc_month_line <- metals_monthlineplots(scandium_total_ugL.1)
Se_month_line <- metals_monthlineplots(selenium_total_ugL.1)
Ag_month_line <- metals_monthlineplots(silver_total_ugL.1)
Sr_month_line <- metals_monthlineplots(strontium_total_ugL.1)
Te_month_line <- metals_monthlineplots(tellurium_total_ugL.1)
Tb_month_line <- metals_monthlineplots(terbium_total_ugL.1)
Tl_month_line <- metals_monthlineplots(thallium_total_ugL.1)
Sn_month_line <- metals_monthlineplots(tin_total_ugL.1)
Ti_month_line <- metals_monthlineplots(titanium_total_ugL.1)
W_month_line <- metals_monthlineplots(tungsten_total_ugL.1)
U_month_line <- metals_monthlineplots(uranium_total_ugL.1)
V_month_line <- metals_monthlineplots(vanadium_total_ugL.1)
Yb_month_line <- metals_monthlineplots(ytterbium_total_ugL.1)
Y_month_line <- metals_monthlineplots(yttrium_total_ugL.1)
Zn_month_line <- metals_monthlineplots(zinc_total_ugL.1)
Zr_month_line <- metals_monthlineplots(zirconium_total_ugL.1)

allmetals_month_line <-
  ggarrange(Ag_month_line,
            Al_month_line,
            As_month_line,
            B_month_line,
            Ba_month_line,
            Be_month_line,
            Bi_month_line,
            Cd_month_line,
            Ce_month_line,
            Co_month_line,
            Cr_month_line,
            Cs_month_line,
            Cu_month_line,
            Eu_month_line,
            Fe_month_line,
            Ga_month_line,
            Gd_month_line,
            Ge_month_line,
            Hf_month_line,
            Ho_month_line,
            In_month_line,
            La_month_line,
            Li_month_line,
            Lu_month_line,
            Mn_month_line,
            Mo_month_line,
            Nb_month_line,
            Nd_month_line,
            Ni_month_line,
            Pb_month_line,
            Pd_month_line,
            Pr_month_line,
            Pt_month_line,
            Rb_month_line,
            Ru_month_line,
            Sb_month_line,
            Sc_month_line,
            Se_month_line,
            Sm_month_line,
            Sn_month_line,
            Sr_month_line,
            Tb_month_line,
            Te_month_line,
            Ti_month_line,
            Tl_month_line,
            U_month_line,
            V_month_line,
            W_month_line,
            Y_month_line,
            Yb_month_line,
            Zn_month_line,
            Zr_month_line,
            ncol = 8, nrow = 7,
            common.legend = TRUE)

print(allmetals_month_line) #There are a bunch without any data so making a version without those

metals_month_line <-
  ggarrange(Ag_month_line,
            Al_month_line,
            As_month_line,
            B_month_line,
            Ba_month_line,
            Be_month_line,
            Bi_month_line,
            Cd_month_line,
            Ce_month_line,
            Co_month_line,
            Cr_month_line,
            Cs_month_line,
            Cu_month_line,
            Fe_month_line,
            Ga_month_line,
            La_month_line,
            Li_month_line,
            Mn_month_line,
            Mo_month_line,
            Nb_month_line,
            Ni_month_line,
            Pb_month_line,
            Pt_month_line,
            Rb_month_line,
            Sb_month_line,
            Se_month_line,
            Sn_month_line,
            Sr_month_line,
            Ti_month_line,
            Tl_month_line,
            U_month_line,
            V_month_line,
            W_month_line,
            Y_month_line,
            Zn_month_line,
            ncol = 6, nrow = 6,
            common.legend = TRUE,
            legend = "right")

print(metals_month_line)



###Plotting individual elements: points and lines
weektimeseries <- function(pointdata, linedata, y) {
  ggplot() +
    geom_point(data = pointdata,
               aes(x = week,
                   y= {{y}}),
               alpha = 1/5)+
    geom_line(data = linedata,
              aes(x = week,
                  y= {{y}},
                  group = 1)) +
    theme_light()
}

Al_week_hope <- weektimeseries(pointdata = hopemetals,
                               linedata = hopemetals_meanweeks,
                               y = aluminum_total_ugL.1)
Al_week_hope

Al_week_gravesend <- weektimeseries(pointdata = gravesendmetals,
                                    linedata = gravesendmetals_meanweeks,
                                    y = aluminum_total_ugL.1)
Al_week_gravesend

ggarrange(Al_week_hope, Al_week_gravesend,
          labels = c("Hope", "Gravesend reach"),
          hjust = c(-1.5, -.4),
          vjust = 2,
          nrow = 2, ncol = 1)

monthtimeseries <- function(pointdata, linedata, y) {
  ggplot() +
    geom_point(data = pointdata,
               aes(x = month,
                   y= {{y}}),
               alpha = 1/5)+
    geom_line(data = linedata,
              aes(x = month,
                  y= {{y}},
                  group = 1)) +
    theme_light()
}

Al_month_hope <- monthtimeseries(pointdata = hopemetals,
                                 linedata = hopemetals_meanmonths,
                                 y = aluminum_total_ugL.1)
Al_month_hope

