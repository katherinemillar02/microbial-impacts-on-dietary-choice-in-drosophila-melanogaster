#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_oviposition_b1 <- read_excel("data/oviposition_1-4-b1.xlsx")

## Making the data long 
one_to_four_oviposition_b1_long <- one_to_four_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
one_to_four_oviposition_b1_plot <- one_to_four_oviposition_b1_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Number of eggs per diet patch", 
       title = "1:4 Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,150)+
  geom_jitter(data =  one_to_four_oviposition_b1_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
onetofour_oviposition_lm <- lm(egg_numbers ~  diet, data = one_to_four_oviposition_b1_long)

# Assumption Checking of the model 
performance::check_model(onetofour_oviposition_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(onetofour_oviposition_lm , check = c("homogeneity")) # line is not flat.
performance::check_model(onetofour_oviposition_lm , check = c("linearity")) # line is very flat.
performance::check_model(onetofour_oviposition_lm , check = c("outliers"))

## could be better 

# Trying a generalised linear model
onetofour_oviposition_glm_1  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = one_to_four_oviposition_b1_long)
summary(onetofour_oviposition_glm_1) # underdispersed


onetofour_oviposition_glm_2  <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = one_to_four_oviposition_b1_long)


performance::check_model(onetofour_oviposition_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_oviposition_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_oviposition_glm_2, check = c("outliers"))

# glm with quasipoisson looks slighly better

# summary function, shows t test
summary(onetofour_oviposition_glm_2)

# using anova 
anova(onetofour_oviposition_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_oviposition_glm_2, pairwise ~ diet)
# unconditioned diets significantly preferred 


