#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
four_to_one_oviposition_b1 <- read_excel("data/oviposition-4_1-b1.xlsx")

## Making the data long 
four_to_one_oviposition_b1_long <- four_to_one_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Making the data long 
four_to_one_oviposition_b1_plot <- 
  four_to_one_oviposition_b1_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Number of eggs per diet patch", 
       title = "4:1 Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,100)+
  geom_jitter(data =  ## Making the data long 
                four_to_one_oviposition_b1_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
## Making the data long 
four_to_one_oviposition_b1_lm <- lm(egg_numbers ~  diet, data = ## Making the data long 
                                 four_to_one_oviposition_b1_long)

# Assumption Checking of the model 
performance::check_model(four_to_one_oviposition_b1_lm , check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(four_to_one_oviposition_b1_lm , check = c("homogeneity")) # line is not flat.
performance::check_model(four_to_one_oviposition_b1_lm , check = c("linearity")) # line is very flat.
performance::check_model(onetofour_oviposition_lm , check = c("outliers"))

## could be better 

# Trying a generalised linear model
four_to_one_oviposition_b1_glm_1  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = four_to_one_oviposition_b1_long)
summary(four_to_one_oviposition_b1_glm_1 ) # overdispersed


four_to_one_oviposition_b1_glm_2 <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = four_to_one_oviposition_b1_long)


performance::check_model(four_to_one_oviposition_b1_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(four_to_one_oviposition_b1_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(four_to_one_oviposition_b1_glm_2, check = c("outliers"))

# lm looks better

# summary function, shows t test
summary(four_to_one_oviposition_b1_lm)

# using anova 
anova(four_to_one_oviposition_b1_lm)

# emmeans for tukey analysis 
emmeans::emmeans(four_to_one_oviposition_b1_lm, pairwise ~ diet)
# not quite significant?


