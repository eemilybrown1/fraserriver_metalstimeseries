library(tidyverse)
library(dplyr)
library(ggpubr)

data <- read.csv("C:/Users/emmie/Dropbox/PC/Downloads/BC08MF0001_PYLTM_0_20000101_to_20220101.csv")

metals <-
  data %>%
  mutate(aluminum_ugL = 
              list(
                data[str_detect(colnames(data), pattern = "^Aluminum")]
                  ) 
              ) #This creates a list variable... how to sum across rows within this list variable?

##still need to incorporate conversion to ug/L


str_detect(colnames(data), pattern = "^Aluminum") #Correctly selects the columns but don't know how to turn this into a mutate call

