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


##Plotting all elements by week (average lines only for facetting)
weeklineplots <- function(y) {
  ggplot() +
    geom_line(data = meanweeks,
              aes(x = week,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,52,13)) +
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
Pd_week_line <- weeklineplots(palladium_total_ugL.1)
Pt_week_line <- weeklineplots(platinum_total_ugL.1)
Pr_week_line <- weeklineplots(praseodymium_total_ugL.1)
Rb_week_line <- weeklineplots(rubidium_total_ugL.1)
Ru_week_line <- weeklineplots(ruthenium_total_ugL.1)
Sm_week_line <- weeklineplots(samarium_total_ugL.1)
Sc_week_line <- weeklineplots(scandium_total_ugL.1)
Se_week_line <- weeklineplots(selenium_total_ugL.1)
Ag_week_line <- weeklineplots(silver_total_ugL.1)
Sr_week_line <- weeklineplots(strontium_total_ugL.1)
Te_week_line <- weeklineplots(tellurium_total_ugL.1)
Tb_week_line <- weeklineplots(terbium_total_ugL.1)
Tl_week_line <- weeklineplots(thallium_total_ugL.1)
Sn_week_line <- weeklineplots(tin_total_ugL.1)
Ti_week_line <- weeklineplots(titanium_total_ugL.1)
W_week_line <- weeklineplots(tungsten_total_ugL.1)
U_week_line <- weeklineplots(uranium_total_ugL.1)
V_week_line <- weeklineplots(vanadium_total_ugL.1)
Yb_week_line <- weeklineplots(ytterbium_total_ugL.1)
Y_week_line <- weeklineplots(yttrium_total_ugL.1)
Zn_week_line <- weeklineplots(zinc_total_ugL.1)
Zr_week_line <- weeklineplots(zirconium_total_ugL.1)

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
          ncol = 8, nrow = 7)
          
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
            ncol = 6, nrow = 6)

print(metals_week_line)

##Plotting all elements by month
monthlineplots <- function(y) {
  ggplot() +
    geom_line(data = meanmonths,
              aes(x = month,
                  y={{y}},
                  group = 1)) +
    scale_x_discrete(breaks=seq(1,12,4)) +
    theme_light()
}

Al_month_line <- monthlineplots(aluminum_total_ugL.1)
Sb_month_line <- monthlineplots(antimony_total_ugL.1)
As_month_line <- monthlineplots(arsenic_total_ugL.1)
Ba_month_line <- monthlineplots(barium_total_ugL.1)
Be_month_line <- monthlineplots(beryllium_total_ugL.1)
Bi_month_line <- monthlineplots(bismuth_total_ugL.1)
B_month_line <- monthlineplots(boron_total_ugL.1)
Cd_month_line <- monthlineplots(cadmium_total_ugL.1)
Ce_month_line <- monthlineplots(cerium_total_ugL.1)
Cs_month_line <- monthlineplots(cesium_total_ugL.1)
Cr_month_line <- monthlineplots(chromium_total_ugL.1)
Co_month_line <- monthlineplots(cobalt_total_ugL.1)
Cu_month_line <- monthlineplots(copper_total_ugL.1)
Eu_month_line <-monthlineplots(europium_total_ugL.1)
Gd_month_line <- monthlineplots(gadolinium_totalrecoverable_ugL.1)
Ga_month_line <- monthlineplots(gallium_total_ugL.1)
Ge_month_line <- monthlineplots(germanium_total_ugL.1)
Hf_month_line <- monthlineplots(hafnium_total_ugL.1)
Ho_month_line <- monthlineplots(holmium_total_ugL.1)
In_month_line <- monthlineplots(indium_total_ugL.1)
Fe_month_line <- monthlineplots(iron_total_ugL.1)
La_month_line <- monthlineplots(lanthanum_total_ugL.1)
Pb_month_line <- monthlineplots(lead_total_ugL.1)
Li_month_line <- monthlineplots(lithium_total_ugL.1)
Lu_month_line <- monthlineplots(lutetium_total_ugL.1)
Mn_month_line <- monthlineplots(manganese_total_ugL.1)
Mo_month_line <- monthlineplots(molybdenum_total_ugL.1)
Nd_month_line <- monthlineplots(neodymium_total_ugL.1)
Ni_month_line <- monthlineplots(nickel_total_ugL.1)
Nb_month_line <- monthlineplots(niobium_total_ugL.1)
Pd_month_line <- monthlineplots(palladium_total_ugL.1)
Pt_month_line <- monthlineplots(platinum_total_ugL.1)
Pr_month_line <- monthlineplots(praseodymium_total_ugL.1)
Rb_month_line <- monthlineplots(rubidium_total_ugL.1)
Ru_month_line <- monthlineplots(ruthenium_total_ugL.1)
Sm_month_line <- monthlineplots(samarium_total_ugL.1)
Sc_month_line <- monthlineplots(scandium_total_ugL.1)
Se_month_line <- monthlineplots(selenium_total_ugL.1)
Ag_month_line <- monthlineplots(silver_total_ugL.1)
Sr_month_line <- monthlineplots(strontium_total_ugL.1)
Te_month_line <- monthlineplots(tellurium_total_ugL.1)
Tb_month_line <- monthlineplots(terbium_total_ugL.1)
Tl_month_line <- monthlineplots(thallium_total_ugL.1)
Sn_month_line <- monthlineplots(tin_total_ugL.1)
Ti_month_line <- monthlineplots(titanium_total_ugL.1)
W_month_line <- monthlineplots(tungsten_total_ugL.1)
U_month_line <- monthlineplots(uranium_total_ugL.1)
V_month_line <- monthlineplots(vanadium_total_ugL.1)
Yb_month_line <- monthlineplots(ytterbium_total_ugL.1)
Y_month_line <- monthlineplots(yttrium_total_ugL.1)
Zn_month_line <- monthlineplots(zinc_total_ugL.1)
Zr_month_line <- monthlineplots(zirconium_total_ugL.1)

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
            ncol = 8, nrow = 7)

print(allmetals_month_line)

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
            ncol = 6, nrow = 6)

print(metals_month_line)

##Plotting by individual element

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
