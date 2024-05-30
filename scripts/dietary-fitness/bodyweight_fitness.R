## Packages
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


viridis_colours <- viridis(10)

## Reading body weight data in
bodyweight <- read_excel("data/fitness_development/bodyweight_flies.xlsx")

bodyweight_2 <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")
##

## Multiplying the data by * 1000 so it can be visualised 
bodyweight$weight_mg <- bodyweight$weight_mg * 1000


## Visualising the data
bodyweight_plot <- ggplot(bodyweight, aes(x = sex, y = weight_mg, fill = treatment)) +
  geom_boxplot() +
  geom_point(aes(),
             size = 1,
             shape = 16,
             position = position_jitter(width = 0.4)) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colours[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "none") +
  labs(x = "Sex", 
       y = "Body Weight (μg) of fly") +
  labs(fill = "Treatment")+
  ylim(0,700)


## Multiplying the data by * 1000 so it can be visualised 
bodyweight_2$weight_mg <- bodyweight_2$weight_mg * 1000


## Visualising the data
bodyweight_plot_2 <- ggplot(bodyweight_2, aes(x = sex, y = weight_mg, fill = treatment)) +
  geom_boxplot() +
  geom_point(aes(),
             size = 1,
             shape = 16,
             position = position_jitter(width = 0.4)) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colours[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical")+
  labs(x = "Sex", 
       y = "") +
  labs(fill = "Treatment")+
  ylim(0,700)



bodyweight_plot  + bodyweight_plot_2 




#### Statistical Analyses 

glm_mm_weight <- glmmTMB(weight_mg ~ treatment + sex, family = poisson, data = bodyweight)

## qq plot from the model
residuals <- residuals(glm_mm_weight)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos
 

check_overdispersion(glm_mm_weight)
check_zeroinflation(glm_mm_weight) ## alright 

drop1(glm_mm_weight, test = "Chisq")

summary(glm_mm_weight)


glm_mm_weight_2 <- glmmTMB(weight_mg ~ treatment + sex, family = poisson, data = bodyweight_2)

## qq plot from the model
residuals <- residuals(glm_mm_weight_2)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos


check_overdispersion(glm_mm_weight)
check_zeroinflation(glm_mm_weight) ## alright 

drop1(glm_mm_weight_2, test = "Chisq")

summary(glm_mm_weight_2)

