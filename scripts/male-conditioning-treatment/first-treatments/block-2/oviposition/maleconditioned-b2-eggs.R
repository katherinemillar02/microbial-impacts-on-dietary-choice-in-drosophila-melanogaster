#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditioned_b2_eggs <- read_excel("data/maleconditioned-oviposition-b2.xlsx")

## Making the data long 
conditioned_b2_eggs_long <- conditioned_b2_eggs %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
conditioned_b2_eggs_plot <- conditioned_b2_eggs_long   %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = " Male Conditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,200)+
  geom_jitter(data =  conditioned_b2_eggs_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



#### Data Analysis ----
# First testing a linear model 
conditioned_b2_egg_lm <- lm(egg_numbers ~  diet, data = conditioned_b2_eggs_long)

# Assumption Checking of the model 
performance::check_model(conditioned_b2_egg_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straight
performance::check_model(conditioned_b2_egg_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(conditioned_b2_egg_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(conditioned_b2_egg_lm, check = c("outliers"))



# Trying a generalised linear model
conditioned_b2_egg_glm <- glm(egg_numbers ~  diet, family = poisson(link = "log"), conditioned_b2_eggs_long)
summary(conditioned_b2_egg_glm) ## underdispersed

conditioned_b2_egg_glm_2 <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), conditioned_b2_eggs_long)


performance::check_model(conditioned_b2_egg_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_b2_egg_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(conditioned_b2_egg_glm_2, check = c("outliers"))

# lm probs better but neither models are great

# summary function, shows t test
summary(conditioned_b2_egg_glm_2)

# using anova 
anova(conditioned_b2_egg_glm_2 )

# emmeans for tukey analysis 
emmeans::emmeans(conditioned_b2_egg_glm_2 , pairwise ~ diet)


