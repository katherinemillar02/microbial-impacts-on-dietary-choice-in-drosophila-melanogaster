## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(survminer)
library(survival)
library(ggsurvfit)


## Reading the data in
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx", range = cell_cols("A:E"))





## adding a sex section 
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")

## Survival object event, censor

Surv(lifespan_adultstraits$days_alive, lifespan_adultstraits$censor)

lifespan <- lifespan_adultstraits %>% 
  mutate(censor = replace_na(censor, 0)) %>% 
  mutate(`date_of _death` = as.Date(`date_of _death`)) %>% 
  mutate(