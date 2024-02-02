#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)


#### Upload data for plot - median calculated 
onetofour_fourtoone_oviposition_b1 <- read_excel("data/oviposition-4-1_1-4-b1.xlsx")

## Making the data long 
onetofour_fourtoone_oviposition_b1_long <- onetofour_fourtoone_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --

onetofour_fourtoone_oviposition_b1_plot <- 
  onetofour_fourtoone_oviposition_b1_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 and 4:1 Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,150)+
  geom_jitter(data =  ## Making the data long 
                onetofour_fourtoone_oviposition_b1_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
onetofour_fourtoone_oviposition_lm <- lm(egg_numbers ~  diet, data = onetofour_fourtoone_oviposition_b1_long)

# Assumption Checking of the model 
performance::check_model(onetofour_fourtoone_oviposition_lm , check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(onetofour_fourtoone_oviposition_lm , check = c("homogeneity")) # line is not flat.
performance::check_model(onetofour_fourtoone_oviposition_lm , check = c("linearity")) # line is very flat.
performance::check_model(onetofour_fourtoone_oviposition_lm , check = c("outliers"))

## lm looks pretty good 



# Trying a generalised linear model
onetofour_fourtoone_oviposition_glm_1  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = onetofour_fourtoone_oviposition_b1_long)
summary(onetofour_fourtoone_oviposition_glm_1) # overdispersered so quasipoission 

# would not let me do poisson - but choosing glm
onetofour_fourtoone_oviposition_glm_2  <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = onetofour_fourtoone_oviposition_b1_long)


performance::check_model(onetofour_fourtoone_oviposition_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_fourtoone_oviposition_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_fourtoone_oviposition_glm_2, check = c("outliers"))

# lm may be better

# summary function, shows t test
summary(onetofour_fourtoone_oviposition_lm)

# using anova 
anova(onetofour_fourtoone_oviposition_lm)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_fourtoone_oviposition_lm, pairwise ~ diet)


