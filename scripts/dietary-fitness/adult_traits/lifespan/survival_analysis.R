#### Packages ####
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
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

# Running Surv
Surv(lifespan_adultstraits$days_alive, lifespan_adultstraits$censor)
 # Using Surv() to run time-to-event data, censor represents the days alive not being definitive. 

## Creating a new survival dataset 
## This dataset will run the days alive to still running vials to whatever today's date is
survival <- lifespan_adultstraits %>% 
  mutate(censor = replace_na(censor, 0)) %>%  # replacing NA (not yet dead) with 0 
  mutate(`date_of _death` = as.Date(`date_of _death`)) %>% # reading date as date
  mutate(`date_of _death` = replace_na(`date_of _death`, Sys.Date())) %>% # replacing missing death dates with the system date (whatever today's date is)
  mutate(days_alive = as.numeric(as.duration(`date_of _death` - dmy("19-10-2024")), "days")) # calculating the days alive for NA (where they are still alive)



# Running a plot with the lifespan dataset
survival_plot <- survfit2(Surv(days_alive, censor) ~ treatment, data = survival) %>% 
  ggsurvfit()+
  add_confidence_interval()+
  theme_classic()+
  theme(
    legend.position = c(0.9, 0.9),  
    legend.justification = c(1, 1),  
    legend.title = element_blank()) +
  labs(x = "Days alive since experiment began", 
       y = "Survival Probability")

# Running plot
survival_plot




## Data Analysis 

# 
cox_model_1 <- coxph(Surv(days_alive, censor) ~ Conditioning * Sex, data = survival)

drop1(cox_model_1, test = "Chisq")

cox_model_2 <- coxph(Surv(days_alive, censor) ~ Conditioning + Sex, data = lifespan)


fit <- cox.zph(cox_model_2)

ggcoxzph(fit)

summary(coxph(Surv(days_alive, censor) ~ Conditioning + strata(Sex), data = lifespan))

lifespan_female<- lifespan %>% filter(Sex == "Focal female")

lifespan_male<- lifespan %>% filter(Sex == "Focal male")

summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_female))
summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_male))




