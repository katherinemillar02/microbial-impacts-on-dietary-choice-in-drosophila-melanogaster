####### OvoD1 Conditioning - 4:1 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
four_to_one_OvoD1 <- read_excel("data/OvoD1-4_1-median.xlsx")

## Making the data long 
four_to_one_OvoD1_long <- four_to_one_OvoD1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Visualising the data 
four_to_one_OvoD1_plot <- four_to_one_OvoD1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,4)+
  geom_jitter(data =  four_to_one_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## might have messed up plates, need to think about this. 


#### Data Analysis 

## 
## Statistical analysis ----
# First testing a linear model 
four_to_one_OvoD1_lm <- lm(fly_numbers ~  diet, data = four_to_one_OvoD1_long)

# Assumption Checking of the model 
performance::check_model(four_to_one_OvoD1_lm, check = c("qq")) # qq looks okay, dots are a bit spaced out, could be a lot better. 
performance::check_model(four_to_one_OvoD1_lm, check = c("homogeneity")) # okay, quite straight.
performance::check_model(four_to_one_OvoD1_lm, check = c("linearity")) # very windy, quite bad. 
performance::check_model(four_to_one_OvoD1_lm, check = c("outliers")) # don't see anything particularly awful. 




# Trying a generalised linear model
four_to_one_OvoD1_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), four_to_one_OvoD1_long)
# underdispersed - trying quasipoisson? not the best thing to do though
four_to_one_OvoD1_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), four_to_one_OvoD1_long)


performance::check_model(four_to_one_OvoD1_glm_2, check = c("qq")) # looks pretty much the same as lm, maybe (slightly) better. 
performance::check_model(four_to_one_OvoD1_glm_2, check = c("homogeneity")) # line is not flat, slopes down. 
performance::check_model(four_to_one_OvoD1_glm_2, check = c("outliers")) # seems okay? 

# lm may be better? quite similar though.  

# summary function, shows t test
summary(four_to_one_OvoD1_glm_2)

# using anova 
anova(four_to_one_OvoD1_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(four_to_one_OvoD1_glm_2, pairwise ~ diet)
## This shows there is not a significant difference between 4:1 Conditioned and 4:1 Unconditioned. 



