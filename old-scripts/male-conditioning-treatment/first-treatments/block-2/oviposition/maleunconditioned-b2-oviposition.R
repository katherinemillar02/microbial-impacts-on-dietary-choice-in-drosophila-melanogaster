#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b2_eggs <- read_excel("data/maleunconditioned-oviposition-b2.xlsx")

## Making the data long 
unconditioned_b2_eggs_long <- unconditioned_b2_eggs %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
unconditioned_b2_eggs_plot <- unconditioned_b2_eggs_long %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = " Male Unconditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,200)+
  geom_jitter(data =  unconditioned_b2_eggs_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


conditioned_b2_eggs_plot + unconditioned_b2_eggs_plot



## Unconditioned data analysis 


#### Data Analysis ----
# First testing a linear model 
unconditioned_b2_egg_lm <- lm(egg_numbers ~  diet, data = unconditioned_b2_eggs_long)

# Assumption Checking of the model 
performance::check_model(unconditioned_b2_egg_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straight
performance::check_model(unconditioned_b2_egg_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(unconditioned_b2_egg_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(unconditioned_b2_egg_lm, check = c("outliers"))

## mostly looks okay

# Trying a generalised linear model
unconditioned_b2_egg_glm <- glm(egg_numbers ~  diet, family = poisson(link = "log"), unconditioned_b2_eggs_long)
summary(unconditioned_b2_egg_glm) ## underdispersed ?? 

## Doing glm with quasipoisson for now
unconditioned_b2_egg_glm_2 <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), unconditioned_b2_eggs_long)


performance::check_model(unconditioned_b2_egg_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(unconditioned_b2_egg_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(unconditioned_b2_egg_glm_2, check = c("outliers"))

# glm probs looks better 

# summary function, shows t test
summary(unconditioned_b2_egg_glm_2 )

# using anova 
anova(unconditioned_b2_egg_glm_2 )

# emmeans for tukey analysis 
emmeans::emmeans(unconditioned_b2_egg_glm_2 , pairwise ~ diet)

