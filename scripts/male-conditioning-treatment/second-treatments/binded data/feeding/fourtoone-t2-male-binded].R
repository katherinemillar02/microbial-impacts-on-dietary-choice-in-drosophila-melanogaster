# Four to One
#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
 # male binded 
# adding a variable 
fourtoone_t2_bind_1 <- four_to_one_b1_long %>% mutate(experiment = "one")
fourtoone_t2_bind_2 <- four_to_one_b2_long %>% mutate(experiment = "two")

## binding the data
male_t2_bind_conditioned_4to1 <- rbind(fourtoone_t2_bind_1, fourtoone_t2_bind_2)

male_t2_bind_conditioned_4to1_plot <- 
  male_t2_bind_conditioned_4to1  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 Conditioned and Unconditioned Feeding")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  
                male_t2_bind_conditioned_4to1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



## new plot design
male_feeding_0 <-
  ggplot(male_t2_bind_conditioned_4to1, aes(x = diet, y = fly_numbers, pattern = diet, fill = diet)) +
  geom_boxplot(aes(fill = diet))+
  scale_fill_manual(name = "Diet", values = c("#FDECCD","#FDECCD", "lightblue", "lightblue" )) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
                       color = "black",
                       pattern_fill = "white",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("stripe", "none", "stripe", "none")) +
  ylim(0,6)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_jitter(data =  male_t2_bind_conditioned_4to1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21) +
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "")+
  theme(legend.position="none")

## 
#### Data Analysis ----
# First testing a linear model 
experiment_t2_sig_fourone <- lm(fly_numbers ~ experiment, data = male_t2_bind_conditioned_4to1)

## looking for sig across treatments for feeding number
drop1(experiment_t2_sig_fourone, test = "F")
## no sig between the two so can use?

## trying out a linear model
t2_feeding_lm_fourone <- lm(fly_numbers ~ diet, data = male_t2_bind_conditioned_4to1)

# Assumption Checking of the model 
performance::check_model(t2_feeding_lm_fourone, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straightperformance::check_model(egg_analysis_combined_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(t2_feeding_lm_fourone, check = c("linearity")) # line is very much not flat.
performance::check_model(t2_feeding_lm_fourone, check = c("homogeneity"))
performance::check_model(t2_feeding_lm_fourone, check = c("outliers"))

# doesn't look awful

# Trying a generalised linear model
t2_feeding_glm_fourone <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = male_t2_bind_conditioned_4to1)
summary(t2_feeding_glm_fourone) # overdispersed

t2_feeding_glm_fourone_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = male_t2_bind_conditioned_4to1)


#### Assumption Checking ####
performance::check_model(t2_feeding_glm_fourone_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(t2_feeding_glm_fourone_2 , check = c("homogeneity")) # not flat but better
performance::check_model(t2_feeding_glm_fourone_2 , check = c("outliers"))

# glm may be better 

# summary function, shows t test
summary(t2_feeding_glm_fourone_2)

# using anova 
anova(t2_feeding_glm_fourone_2)

# emmeans for tukey analysis 
emmeans::emmeans(t2_feeding_glm_fourone_2, pairwise ~ diet)

# strong significant difference 