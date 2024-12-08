## Packages
source("packages.R")



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
  mutate(`date_of _death` = replace_na(`date_of _death`, Sys.Date())) %>% 
  mutate(days_alive = as.numeric(as.duration(`date_of _death` - dmy("19-10-2024")), "days"))

survfit2(Surv(days_alive, censor) ~ treatment, data = lifespan) %>% 
  ggsurvfit()+
  add_confidence_interval()

coxph(Surv(days_alive, censor) ~ Conditioning * Sex, data = lifespan)

cox_model<-coxph(Surv(days_alive, censor) ~ Conditioning + Sex, data = lifespan)

fit<-cox.zph(cox_model)

ggcoxzph(fit)

summary(coxph(Surv(days_alive, censor) ~ Conditioning + strata(Sex), data = lifespan))

lifespan_female<- lifespan %>% filter(Sex == "Focal female")

lifespan_male<- lifespan %>% filter(Sex == "Focal male")

summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_female))
summary(coxph(Surv(days_alive, censor) ~ Conditioning, data = lifespan_male))




