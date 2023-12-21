#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

fourone_oviposition_b2 <- read_excel("data/oviposition_4-1_t2b2.xlsx")

## Making the data long 
fourone_oviposition_b2_long <- fourone_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## data visualisation
fourone_oviposition_b2_plot  <- fourone_oviposition_b2_long   %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 and 1:4 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,100)+
  geom_jitter(data =  fourone_oviposition_b2_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




## 
## Statistical analysis ----
# First testing a linear model 
fourtoone_b2_oviposition_lm <- lm(egg_numbers ~  diet, data = fourone_oviposition_b2_long)

# Assumption Checking of the model 
performance::check_model(fourtoone__b2_oviposition_lm, check = c("qq")) # qq looks awful
performance::check_model(fourtoone__b2_oviposition_lm, check = c("homogeneity")) # bad
performance::check_model(fourtoone__b2_oviposition_lm, check = c("linearity")) # bad
performance::check_model(fourtoone__b2_oviposition_lm, check = c("outliers"))

## looks okay but trying glm just incase 

# Trying a generalised linear model
fourtoone_b2_oviposition_glm_1  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = fourone_oviposition_b2_long)

summary(fourtoone_b2_oviposition_glm_1) # very overdispersed 

fourtoone_b2_oviposition_b2_glm_2  <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = fourone_oviposition_b2_long)


performance::check_model(fourtoone_b2_oviposition_b2_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(fourtoone_b2_oviposition_b2_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(fourtoone_b2_oviposition_b2_glm_2, check = c("outliers"))

# glm looks better for homogeneity so choosing this 


# summary function, shows t test
summary(fourtoone_b2_oviposition_b2_glm_2)

# using anova 
anova(fourtoone_b2_oviposition_b2_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(fourtoone_b2_oviposition_b2_glm_2, pairwise ~ diet)
## no sig difference but could have chosen wrong model

