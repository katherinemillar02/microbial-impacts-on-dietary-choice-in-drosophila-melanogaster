## Packages
source("packages.R")
##


## Reading the data in
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx", range = cell_cols("A:E"))


## Adding a Conditioning variable:  
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
## Adding a Sex variable:  
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")

## Survival object event, censor, using the Surv() function
Surv(lifespan_adultstraits$days_alive, lifespan_adultstraits$censor)
 # 1 is death and 0 is discontinued
 # This will output days alive 


## Creating a new dataset 
lifespan <- lifespan_adultstraits %>% 
  mutate(censor = replace_na(censor, 0)) %>% 
  mutate(`date_of _death` = as.Date(`date_of _death`)) %>% 
  mutate(`date_of _death` = replace_na(`date_of _death`, Sys.Date())) %>% ## If there is not a death yet, this code will compare it to today's date
  mutate(days_alive = as.numeric(as.duration(`date_of _death` - dmy("19-10-2024")), "days"))


# Using survfit2
survival_curve <- survfit2(Surv(days_alive, censor) ~ treatment, data = lifespan) %>% 
  ggsurvfit() +
  add_confidence_interval() 
  # This specifically uses the Surv object
  # Within survfit2, use time to event (which is death) - with censoring information
  # after the ~ is treatment, because this is the predictor

# Run curve 
survival_curve


## Running survival mdoels
 # Testing for an interaction effect
cox_model_i <- coxph(Surv(days_alive, censor) ~ Conditioning * Sex, data = lifespan)
   # No significant interaction... 

 # Testing the model with the interaction effect removed
cox_model <- coxph(Surv(days_alive, censor) ~ Conditioning + Sex, data = lifespan)
# Sex differences (and conditiong?) differences found. 


# Proportional hazards assumption - assumes hazard ratios are constant overtime (with death they should not be)
fit <- cox.zph(cox_model)
#  a p-value lower than 0.05 violates the assumption
# sex violates the assumption - not constant over time 
# conditioning does not - constant over time 

# This will add a visualisation of Schoenfeld resiudals for each covariate over time 
ggcoxzph(fit)
 # Possibilities 
# smooth line - flat and horizontal - PH assumption is satisfied 
# line up and down - covariate may not satisfy the PH assumption 
 # curves look the same to me - maybe conditioning is more bendy 


summary(coxph(Surv(days_alive, censor) ~ Conditioning + strata(Sex), data = lifespan))
 # a proportional hazards model, with Surv including says alive and censor with the predictor variable being conditioning and sex
 # strata(Sex) means stratify by sex, so it has particular hazards for males and females 
 # but conditioning is assumed to be the same across sexes, maybe because there was no interaction effect... 
 # Need to look into what stratifying by sex actually does... 


# Creating separate sex datasets 
lifespan_female <- lifespan %>% filter(Sex == "Focal female")
lifespan_male <- lifespan %>% filter(Sex == "Focal male")

# Separate sex cox survival models
summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_female))
summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_male))

# In females, there is a significant effect of conditioning - they survive longer on conditioned diets 
# Males no sig effect - but the opposite result?? 



