library(tidyverse)
library(dplyr)
library(ggpubr)
library(lubridate)


bcfires <- read.csv("C:/Users/emmie/Documents/fraserbasinfires_2010-2020.csv")

bcfires$year = str_sub(bcfires$layer,-4)

bcfiressummary <- bcfires %>%
  group_by(year) %>%
  mutate(firecount = n()) %>%
  group_by(year, firecount) %>%
  summarize_at(vars(area),
               list(mean_firearea = mean,
                    sd_firearea = sd),
               na.rm = TRUE) %>%
  mutate(year = as.numeric(year))

hopemetals2010_2020 <- hopemetalstbl %>%
  filter(year %in% 2010:2020)

metals_fires_byyear <- hopemetals2010_2020 %>%
  group_by(year) %>%
  summarize_at(vars(aluminum_total_ugL.1:zirconium_total_ugL.1), 
               list(mean = mean, 
                    sd = sd), 
               na.rm = TRUE) %>%
  mutate(year = as.numeric(year)) %>%
  full_join(bcfiressummary, by = "year")
  
  

#Correlation matrix
library(Hmisc)
library(corrplot)

cormetalfire <- metals_fires_byyear[c(2:14,17,22:25,27:28,30:31,33,35,39:41,44:45,47:49,51:52,106:107)]

res <- rcorr(as.matrix(
  cormetalfire
))

p <- res$P
corp <- as.matrix(p[c(1:34),c(35:36)])
#None of them are significant. Need to look at more fine time scales for fires

cor <- cor(
  x= metals_fires_byyear[c(2:14,17,22:25,27:28,30:31,33,35,39:41,44:45,47:49,51:52)],
  y = metals_fires_byyear[106:107],  
  use="complete")

corrplot(cor,
         type = "full",
         p.mat = corp,
         insig = 'p-value',
         tl.col = "black",
         cl.pos = "r",
         cl.ratio = 2)

