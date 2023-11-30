#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditionedunconditioned_b1.2_median_egg <- read_excel("data/ConditionedandUnconditioned_Treatment_Egg_B1.2.xlsx")



## Making the data long 
conditionedunconditioned_b1.2_median_egg_long  <- conditionedunconditioned_b1.2_median_egg %>% 
  pivot_longer(cols = ("1:4 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --

conditionedunconditioned_b1.2_median_egg_plot <- conditionedunconditioned_b1.2_median_egg_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Unconditioned and Conditioned Egg Treatment")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data =  conditionedunconditioned_b1.2_median_egg_long , 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



#### Data Analysis ----
# First testing a linear model 
conditionedunconditioned_egg_b1.2_lm <- lm(egg_numbers ~  diet, data = conditionedunconditioned_b1.2_median_egg_long)

# Assumption Checking of the model 
performance::check_model(conditionedunconditioned_egg_b1.2_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straight
performance::check_model(conditionedunconditioned_egg_b1.2_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(conditionedunconditioned_egg_b1.2_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(conditionedunconditioned_egg_b1.2_lm, check = c("outliers"))



# Trying a generalised linear model
conditionedunconditioned_egg_b1.2_glm <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = conditionedunconditioned_b1.2_median_egg_long)
summary(conditionedunconditioned_egg_b1.2_glm) ## overdispersed so quasipoisson

performance::check_model(conditionedunconditioned_egg_b1.2_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditionedunconditioned_egg_b1.2_glm, check = c("homogeneity")) # not flat but better
performance::check_model(conditionedunconditioned_egg_b1.2_glm, check = c("outliers"))

# homogeneity and qq look better for glm

# summary function, shows t test
summary(conditionedunconditioned_egg_b1.2_glm )

# using anova 
anova(conditionedunconditioned_egg_b1.2_glm )

# emmeans for tukey analysis 
emmeans::emmeans(conditionedunconditioned_egg_b1.2_glm , pairwise ~ diet)



