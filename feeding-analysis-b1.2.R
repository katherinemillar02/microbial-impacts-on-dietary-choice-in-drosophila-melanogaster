
#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Adding a variable 
feeding_analysis_uncond_1.2 <- unconditioned_b1.2_median_long %>% mutate(treatment = "unconditioned")
feeding_analysis_cond_1.2 <- conditioned_b1.2_median_long %>% mutate(treatment = "conditioned")

# Binding the two datasets 
feeding_analysis_1.2 <- rbind(feeding_analysis_cond_1.2,feeding_analysis_uncond_1.2)


## feeding analysis plot
feeding_analysis_plot <- feeding_analysis_1.2  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "Across Conditioned and Unconditioned Feeding count")+
  theme(legend.position="none")+
  ylim(-0.01,5)+
  geom_jitter(data =  feeding_analysis_1.2 , 
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


#### Data Analysis ----
# First testing a linear model 
feeding_analysis_sig <- lm(fly_numbers ~ treatment, data = feeding_analysis_1.2)

drop1(feeding_analysis_sig, test = "F")



## trying out a linear model

feeding_analysis_1.2_lm <- lm(fly_numbers ~ diet, data = feeding_analysis_1.2)

# Assumption Checking of the model 
performance::check_model(feeding_analysis_1.2_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straightperformance::check_model(egg_analysis_combined_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(feeding_analysis_1.2_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(feeding_analysis_1.2_lm, check = c("homogeneity"))
performance::check_model(feeding_analysis_1.2_lm, check = c("outliers"))



# Trying a generalised linear model
feeding_analysis_1.2_glm <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = feeding_analysis_1.2)
summary(feeding_analysis_1.2_glm) # overdispersed so using quaispoisson

feeding_analysis_1.2_glm  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = feeding_analysis_1.2)



performance::check_model(feeding_analysis_1.2_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(feeding_analysis_1.2_glm, check = c("homogeneity")) # not flat but better
performance::check_model(feeding_analysis_1.2_glm, check = c("outliers"))

# homogeneirty is not great for glm but qq looks a lot better 

# summary function, shows t test
summary(feeding_analysis_1.2_lm)

# using anova 
anova(feeding_analysis_1.2_lm)

# emmeans for tukey analysis 
emmeans::emmeans(feeding_analysis_1.2_lm, pairwise ~ diet)



