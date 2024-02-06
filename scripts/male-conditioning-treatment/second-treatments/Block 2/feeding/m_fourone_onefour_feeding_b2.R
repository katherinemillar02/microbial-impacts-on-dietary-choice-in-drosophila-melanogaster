#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
fourtoone_onetofour_b2 <- read_excel("data/4_1-1_4-t2b2_median.xlsx")

## Making the data long 
fourtoone_onetofour_b2_long <- fourtoone_onetofour_b2   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## data visualisation
fourtoone_onetofour_b2_plot  <- fourtoone_onetofour_b2_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 and 1:4 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,3)+
  geom_jitter(data =  fourtoone_onetofour_b2_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



#### Trying the new plot 




## 
## Statistical analysis ----
# First testing a linear model 
fourtoone_onetofour_b2_lm <- lm(fly_numbers ~  diet, data = fourtoone_onetofour_b2_long)

# Assumption Checking of the model 
performance::check_model(fourtoone_onetofour_b2_lm, check = c("qq")) # qq looks awful
performance::check_model(fourtoone_onetofour_b2_lm, check = c("homogeneity")) # bad
performance::check_model(fourtoone_onetofour_b2_lm, check = c("linearity")) # bad
performance::check_model(fourtoone_onetofour_b2_lm, check = c("outliers"))

## looks okay but trying glm just incase 

# Trying a generalised linear model
fourtoone_onetofour_b2_glm_1  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = fourtoone_onetofour_b2_long)
# underdispersed - trying quasipoisson
fourtoone_onetofour_b2_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = fourtoone_onetofour_b2_long)


performance::check_model(fourtoone_onetofour_b2_glm_2 , check = c("qq")) # dots seem to match to line better than lm
performance::check_model(fourtoone_onetofour_b2_glm_2 , check = c("homogeneity")) # not flat but better
performance::check_model(fourtoone_onetofour_b2_glm_2 , check = c("outliers"))

# glm looks a bit worse than lm at least for homogeneity 
# so choosing lm

# summary function, shows t test
summary(fourtoone_onetofour_b2_lm)

# using anova 
anova(fourtoone_onetofour_b2_lm)

# emmeans for tukey analysis 
emmeans::emmeans(fourtoone_onetofour_b2_lm, pairwise ~ diet)
## sig difference - conditioned more preferred 



##### STEP 2 - data analysis with raw data 

## Uploading the raw data
fourone_onefour_b2_raw <- read_excel("data/male_conditioning/treatment_2/block_2/rawdata_m4-1_1-4_t2b2.xlsx")


## Making the raw data long 
fourone_onefour_b2_raw_long <- fourone_onefour_b2_raw %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# library(lme4) needed 
## using the non long data so it gets the variables? 
bin_mod <- glmer(cbind(`4:1 Conditioned`, `4:1 Unconditioned`) ~ 2 + (plate/observation), data = one_to_four_b2_raw , family = binomial)
## Get errors




