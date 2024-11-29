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

## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

## messing around
boxplot_adulttraits_survival <- ggplot(lifespan_adultstraits, aes(x = days_alive, y = treatment, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 4, 8, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Days alive", 
       y = "Treatment",
       fill = "Treatment")+
  coord_flip()

# plot
boxplot_adulttraits_survival
