####### OvoD1 Conditioning - 1:4 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_OvoD1 <- read_excel("data/OvoD1_1-4_median.xlsx")

## Making the data long 
one_to_four_OvoD1_long <- one_to_four_OvoD1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Visualising the data 
one_to_four_OvoD1_plot <- one_to_four_OvoD1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,2)+
  geom_jitter(data =  one_to_four_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
## might have to look at mean average for this plot. 
## might have messed up plates, need to think about this. 


#### Data Analysis 

## 
## Statistical analysis ----
# First testing a linear model 
one_to_four_OvoD1_lm <- lm(fly_numbers ~  diet, data = one_to_four_OvoD1_long)

# Assumption Checking of the model 
performance::check_model(one_to_four_OvoD1_lm, check = c("qq")) # qq looks awful, lines are all spread out 
performance::check_model(one_to_four_OvoD1_lm, check = c("homogeneity")) # bad, quite windy
performance::check_model(one_to_four_OvoD1_lm, check = c("linearity")) # bad, quite windy again
performance::check_model(one_to_four_OvoD1_lm, check = c("outliers")) # don't see anything particularly awful




# Trying a generalised linear model
one_to_four_OvoD1_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = one_to_four_OvoD1_long)
# underdispersed - trying quasipoisson? not the best thing to do though
one_to_four_OvoD1_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = one_to_four_OvoD1_long)


performance::check_model(one_to_four_OvoD1_glm_2, check = c("qq")) # dots are slightly closer to the line
performance::check_model(one_to_four_OvoD1_glm_2, check = c("homogeneity")) # line is flat, but dots are a bit dispersed
performance::check_model(one_to_four_OvoD1_glm_2, check = c("outliers")) # seems okay 

# glm with quasipoisson may be the better model out of the ones assumption/ performance checked. 

# summary function, shows t test
summary(one_to_four_OvoD1_glm_2)

# using anova 
anova(onetofour_b2_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(one_to_four_OvoD1_glm_2, pairwise ~ diet)
## This shows a significant difference between 1:4 Conditioned and 1:4 Unconditioned. 





