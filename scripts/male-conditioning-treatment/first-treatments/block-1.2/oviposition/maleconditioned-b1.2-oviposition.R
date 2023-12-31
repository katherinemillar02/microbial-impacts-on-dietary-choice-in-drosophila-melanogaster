#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)



#### Upload data
conditioned_b1.2_median_egg <- read_excel("data/Conditioned_Treatment_Egg_B1.2.xlsx")



## Making the data long 
conditioned_b1.2_median_egg_long  <-conditioned_b1.2_median_egg %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
conditioned_b1.2_median_egg_plot <- conditioned_b1.2_median_egg_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Conditioned Treatment Eggs")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data =  conditioned_b1.2_median_egg_long, 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

conditioned_b1.2_median_egg_plot  + unconditioned_b1.2_median_egg_plot


#### Data Analysis ----
# First testing a linear model 
conditioned_b1.2_egg_lm <- lm(egg_numbers ~  diet, data = conditioned_b1.2_median_egg_long)

# Assumption Checking of the model 
performance::check_model(conditioned_b1.2_egg_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straight
performance::check_model(conditioned_b1.2_egg_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(conditioned_b1.2_egg_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(conditioned_b1.2_egg_lm, check = c("outliers"))



# Trying a generalised linear model
conditioned_b1.2_egg_glm  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = conditioned_b1.2_median_egg_long)
summary(conditioned_b1.2_egg_glm) ## underdispersed

performance::check_model(conditioned_b1.2_egg_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_b1.2_egg_glm, check = c("homogeneity")) # not flat but better
performance::check_model(conditioned_b1.2_egg_glm , check = c("outliers"))

# lm probs better but neither models are great

# summary function, shows t test
summary(conditioned_b1.2_egg_glm)

# using anova 
anova(conditioned_b1.2_egg_glm)

# emmeans for tukey analysis 
emmeans::emmeans(conditioned_b1.2_egg_glm, pairwise ~ diet)






