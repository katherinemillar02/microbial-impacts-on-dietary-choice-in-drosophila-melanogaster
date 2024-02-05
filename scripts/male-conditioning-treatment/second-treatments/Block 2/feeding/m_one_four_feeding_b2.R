#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_b2 <- read_excel("data/1-4_t2b2_median.xlsx")

## Making the data long 
one_to_four_b2_long <- one_to_four_b2   %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


one_to_four_b2_plot <- one_to_four_b2_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,1.5)+
  geom_jitter(data =  one_to_four_b2_long ,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




## 
## Statistical analysis ----
# First testing a linear model 
onetofour_b2_lm <- lm(fly_numbers ~  diet, data = one_to_four_b2_long)

# Assumption Checking of the model 
performance::check_model(onetofour_b2_lm, check = c("qq")) # qq looks awful
performance::check_model(onetofour_b2_lm, check = c("homogeneity")) # bad
performance::check_model(onetofour_b2_lm, check = c("linearity")) # bad
performance::check_model(onetofour_b2_lm, check = c("outliers"))




# Trying a generalised linear model
onetofour_b2_glm_1  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = one_to_four_b2_long)
# underdispersed - trying quasipoisson
onetofour_b2_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = one_to_four_b2_long)


performance::check_model(onetofour_b2_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_b2_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_b2_glm_2, check = c("outliers"))

# glm may be better - but qq still looks bad 

# summary function, shows t test
summary(onetofour_b2_glm_2)

# using anova 
anova(onetofour_b2_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_b2_glm_2, pairwise ~ diet)
## no significant difference



##### STEP 2 - data analysis with raw data 

## Uploading the raw data
one_to_four_b2_raw <- read_excel("data/male_conditioning/treatment_2/block_2/rawdata_m1-4_t2b2.xlsx")


## Making the raw data long 
one_to_four_b2_raw_long <- one_to_four_b2_raw  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# library(lme4) needed 
## using the non long data so it gets the variables? 
bin_mod <- glmer(cbind(`4:1 Conditioned`, `4:1 Unconditioned`) ~ 2 + (plate/observation), data = one_to_four_b2_raw , family = binomial)
## Get errors



