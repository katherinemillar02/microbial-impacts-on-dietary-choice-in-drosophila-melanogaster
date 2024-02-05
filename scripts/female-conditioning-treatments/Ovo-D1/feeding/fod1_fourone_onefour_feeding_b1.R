####### OvoD1 Conditioning - 4:1 + 1:4 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
fourone_onefour_OvoD1 <- read_excel("data/OvoD1_4-1_1-4_median.xlsx")

## Making the data long 
fourone_onefour_OvoD1_long <- fourone_onefour_OvoD1   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Visualising the data 
fourone_onefour_OvoD1_plot <- fourone_onefour_OvoD1_long %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 + 1:4 OvoD1 Female Conditioned and Unconditioned Feeding")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  fourone_onefour_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



#### Data Analysis 

## 
## Statistical analysis ----
# First testing a linear model 
onefour_fourone_OvoD1_lm <- lm(fly_numbers ~  diet, data = fourone_onefour_OvoD1_long)

# Assumption Checking of the model 
performance::check_model(onefour_fourone_OvoD1_lm , check = c("qq")) # qq looks quite okay.
performance::check_model(onefour_fourone_OvoD1_lm , check = c("homogeneity")) # bad, quite windy.
performance::check_model(onefour_fourone_OvoD1_lm , check = c("linearity")) # quite straight, could be okay.
performance::check_model(onefour_fourone_OvoD1_lm , check = c("outliers")) # don't see anything particularly awful. 




# Trying a generalised linear model
onefour_fourone_OvoD1_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = fourone_onefour_OvoD1_long)
# overdispersed - 
onefour_fourone_OvoD1_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = fourone_onefour_OvoD1_long)


performance::check_model(onefour_fourone_OvoD1_glm_2, check = c("qq")) # dots are on the line, bit shaky. 
performance::check_model(onefour_fourone_OvoD1_glm_2, check = c("homogeneity")) # line is a bit windy, better than lm. 
performance::check_model(onefour_fourone_OvoD1_glm_2, check = c("outliers")) # seems okay/ don't see anything particularly awful. 

# glm with quasipoisson may be the better model out of the ones assumption/ performance checked. 

# summary function, shows t test
summary(onefour_fourone_OvoD1_glm_2)

# using anova 
anova(onefour_fourone_OvoD1_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(onefour_fourone_OvoD1_glm_2, pairwise ~ diet)
## This shows no significant difference between 1:4 Conditioned and 1:4 Unconditioned. 
## Shows a significant difference between 4:1 Conditioned and Unconditioned. 