

#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
fourone_onefour_OvoD1_oviposition <- read_excel("data/female_conditioning/ovod1/block_1/4-1_1-4_oviposition_ovod1.xlsx")

## Making the data long 
fourone_onefour_OvoD1_oviposition_long <- fourone_onefour_OvoD1_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Visualising the data 
fourone_onefour_OvoD1_oviposition_plot <- fourone_onefour_OvoD1_oviposition_long %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot(outlier.shape  = NA)+
  theme_classic()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 + 1:4 OvoD1 Female Conditioned and Unconditioned Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,250)+
  geom_jitter(aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              #width = 0.2,
              shape = 21)






#### Data Analysis 

## 
## Statistical analysis ----
# First testing a linear model 
fourone_onefour_OvoD1_oviposition_lm <- lm(fly_numbers ~  diet, data = fourone_onefour_OvoD1_oviposition_long)

# Assumption Checking of the model 
performance::check_model(fourone_onefour_OvoD1_oviposition_lm, check = c("qq")) # qq looks quite okay.
performance::check_model(fourone_onefour_OvoD1_oviposition_lm, check = c("homogeneity")) # bad, quite windy.
performance::check_model(fourone_onefour_OvoD1_oviposition_lm, check = c("linearity")) # quite straight, could be okay.
performance::check_model(fourone_onefour_OvoD1_oviposition_lm, check = c("outliers")) # don't see anything particularly awful. 


##  Looks sort of okay

# Trying a generalised linear model
fourone_onefour_OvoD1_oviposition_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = fourone_onefour_OvoD1_oviposition_long)
# overdispersed - 
fourone_onefour_OvoD1_oviposition_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = fourone_onefour_OvoD1_oviposition_long)


performance::check_model(fourone_onefour_OvoD1_oviposition_glm_2, check = c("qq")) # dots are on the line, bit shaky. 
performance::check_model(fourone_onefour_OvoD1_oviposition_glm_2, check = c("homogeneity")) # line is a bit windy, better than lm. 
performance::check_model(fourone_onefour_OvoD1_oviposition_glm_2, check = c("outliers")) # seems okay/ don't see anything particularly awful. 

# lm probabaly better  

# summary function, shows t test
summary(fourone_onefour_OvoD1_oviposition_glm_2)

# using anova 
anova(fourone_onefour_OvoD1_oviposition_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(fourone_onefour_OvoD1_oviposition_glm_2, pairwise ~ diet)
## This shows no significant difference between 1:4 Conditioned and 1:4 Unconditioned. 
## Shows a significant difference between 4:1 Conditioned and Unconditioned. 
