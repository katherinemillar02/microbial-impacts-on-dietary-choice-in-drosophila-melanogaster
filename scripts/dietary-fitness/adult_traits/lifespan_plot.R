## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(survminer)
library(survival)


## Reading the data in
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx")
lifespan_adultstraits <- as.data.frame(lifespan_adultstraits)



## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)



## adding a sex section 
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")



#### MESSING AROUND ####
surv_obj <- Surv(time = lifespan_adultstraits$days_alive, event = lifespan_adultstraits$census)
km_fit <- survfit(surv_obj ~ treatment, data = lifespan_adultstraits)
km_fit <- survfit(surv_obj ~ 1)


ggsurvplot(
  km_fit,
  data = lifespan_adultstraits,
  conf.int = TRUE,


  legend.title = "Treatment",
  xlab = "Days Alive",
  ylab = "Survival Probability",
  ggtheme = theme_minimal()
)



