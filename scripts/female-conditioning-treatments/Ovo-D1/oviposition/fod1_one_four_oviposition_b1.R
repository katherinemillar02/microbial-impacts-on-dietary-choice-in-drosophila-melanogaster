

#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
onefour_OvoD1_oviposition <- read_excel("data/female_conditioning/ovod1/block_1/1_4_oviposition_ovod1.xlsx")

## Making the data long 
onefour_OvoD1_oviposition_long <- onefour_OvoD1_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Visualising the data 
onefour_OvoD1_oviposition_plot <- onefour_OvoD1_oviposition_long %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot(outlier.shape  = NA)+
  theme_classic()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 OvoD1 Female Conditioned and Unconditioned Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,250)+
  geom_jitter(aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              #width = 0.2,
              shape = 21)


fod1_oviposition_00 <-
  ggplot( onefour_OvoD1_oviposition_long, aes(x = diet, y = egg_numbers, pattern = diet, fill = diet)) +
  geom_boxplot(aes(fill = diet))+
  scale_fill_manual(name = "Diet", values = c("#00cb98", "#00cb98" )) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
                       color = "black",
                       pattern_fill = "white",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("stripe", "none", "stripe", "none")) +
  ylim(0,250)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_jitter(data =  onefour_OvoD1_oviposition_long ,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21) +
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "")+
  theme(legend.position="none")+
  theme(axis.text = element_text(size=6, angle = 0, hjust =0.5))



#### Data Analysis 

## 
## Statistical analysis ----
# First testing a linear model 
onefour_OvoD1_oviposition_lm <- lm(fly_numbers ~  diet, data = onefour_OvoD1_oviposition_long)

# Assumption Checking of the model 
performance::check_model(onefour_OvoD1_oviposition_lm, check = c("qq")) # qq looks quite okay.
performance::check_model(onefour_OvoD1_oviposition_lm, check = c("homogeneity")) # bad, quite windy.
performance::check_model(onefour_OvoD1_oviposition_lm, check = c("linearity")) # quite straight, could be okay.
performance::check_model(onefour_OvoD1_oviposition_lm, check = c("outliers")) # don't see anything particularly awful. 


##  Looks sort of okay

# Trying a generalised linear model
onefour_OvoD1_oviposition_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = onefour_OvoD1_oviposition_long)
# overdispersed - 
onefour_OvoD1_oviposition_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = onefour_OvoD1_oviposition_long)


performance::check_model(onefour_OvoD1_oviposition_glm_2, check = c("qq")) # dots are on the line, bit shaky. 
performance::check_model(onefour_OvoD1_oviposition_glm_2, check = c("homogeneity")) # line is a bit windy, better than lm. 
performance::check_model(onefour_OvoD1_oviposition_glm_2, check = c("outliers")) # seems okay/ don't see anything particularly awful. 

# lglm with quasipoisson looks a lot better

# summary function, shows t test
summary(fourone_onefour_OvoD1_oviposition_glm_2)

# using anova 
anova(onefour_OvoD1_oviposition_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(onefour_OvoD1_oviposition_glm_2, pairwise ~ diet)
## This shows no significant difference between 1:4 Conditioned and 1:4 Unconditioned. 
## Shows a significant difference between 4:1 Conditioned and Unconditioned. 
