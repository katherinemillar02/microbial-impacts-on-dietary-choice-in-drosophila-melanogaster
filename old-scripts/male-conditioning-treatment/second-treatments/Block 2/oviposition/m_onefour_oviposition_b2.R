#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

onefour_oviposition_b2 <- read_excel("data/oviposition_1-4_t2b2.xlsx")

## Making the data long 
onefour_oviposition_b2_long <- onefour_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## data visualisation
onefour_oviposition_b2_plot  <- onefour_oviposition_b2_long   %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,150)+
  geom_jitter(data =  onefour_oviposition_b2_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




## 
## Statistical analysis ----
# First testing a linear model 
onetofour_oviposition_b2_lm <- lm(egg_numbers ~  diet, data = onefour_oviposition_b2_long)

# Assumption Checking of the model 
performance::check_model(onetofour_oviposition_b2_lm, check = c("qq")) # qq looks awful
performance::check_model(onetofour_oviposition_b2_lm, check = c("homogeneity")) # bad
performance::check_model(onetofour_oviposition_b2_lm, check = c("linearity")) # bad
performance::check_model(onetofour_oviposition_b2_lm, check = c("outliers"))

## looks okay but trying glm just incase 

# Trying a generalised linear model
onetofour_oviposition_b2_glm_1  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = onefour_oviposition_b2_long)

summary(onetofour_oviposition_b2_glm_1 ) # very overdispersed 

onetofour_oviposition_b2_glm_2  <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = onefour_oviposition_b2_long)


performance::check_model(onetofour_oviposition_b2_glm_2 , check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_oviposition_b2_glm_2 , check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_oviposition_b2_glm_2 , check = c("outliers"))

# glm looks a bit worse than lm at least for homogeneity 
# so choosing lm

# summary function, shows t test
summary(onetofour_oviposition_b2_lm)

# using anova 
anova(onetofour_oviposition_b2_lm)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_oviposition_b2_lm, pairwise ~ diet)
## not quite significant

