#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b1.1_egg <- read_excel("data/maleunconditioned-oviposition-b1.1.xlsx")

## Making the data long 
unconditioned_b1.1_egg_long <- unconditioned_b1.1_egg %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
## Median boxplot --
unconditioned_b1.1_egg_plot <- unconditioned_b1.1_egg_long %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Conditioned Treatment Eggs 1.1")+
  theme(legend.position="none")+
  ylim(0,150)+
  geom_jitter(data =  unconditioned_b1.1_egg_long, 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




#### Data Analysis ----
# First testing a linear model 
unconditioned_b1.1_egg_lm <- lm(egg_numbers ~  diet, data = unconditioned_b1.1_egg_long)

# Assumption Checking of the model 
performance::check_model(unconditioned_b1.1_egg_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straight
performance::check_model(unconditioned_b1.1_egg_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(unconditioned_b1.1_egg_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(unconditioned_b1.1_egg_lm, check = c("outliers"))



# Trying a generalised linear model
unconditioned_b1.1_egg_glm <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = unconditioned_b1.1_egg_long)
summary(unconditioned_b1.1_egg_glm) ## underdispersed



## trying a new glm
unconditioned_b1.1_egg_glm_2 <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_b1.1_egg_long)



performance::check_model(unconditioned_b1.1_egg_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(unconditioned_b1.1_egg_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(unconditioned_b1.1_egg_glm_2, check = c("outliers"))

# lm looks better

# summary function, shows t test
summary(unconditioned_b1.1_egg_lm)

# using anova 
anova(unconditioned_b1.1_egg_lm)

# emmeans for tukey analysis 
emmeans::emmeans(unconditioned_b1.1_egg_lm, pairwise ~ diet)






