#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_b1 <- read_excel("data/m1-4_b1.xlsx")

## Making the data long 
one_to_four_b1_long <- one_to_four_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
one_to_four_b1_plot <- one_to_four_b1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male 1:4 Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,1)+
  geom_jitter(data =  one_to_four_b1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
onetofour_b1_lm <- lm(fly_numbers ~  diet, data = one_to_four_b1_long)

# Assumption Checking of the model 
performance::check_model(onetofour_b1_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(onetofour_b1_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(onetofour_b1_lm, check = c("linearity")) # line is very flat.
performance::check_model(onetofour_b1_lm, check = c("outliers"))




# Trying a generalised linear model
onetofour_b1_glm_1  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = one_to_four_b1_long)
# would not let me do poisson - but choosing glm
onetofour_b1_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = one_to_four_b1_long)


performance::check_model(onetofour_b1_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_b1_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_b1_glm_2, check = c("outliers"))

# lm may be better

# summary function, shows t test
summary(onetofour_b1_lm)

# using anova 
anova(onetofour_b1_lm)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_b1_lm, pairwise ~ diet)


