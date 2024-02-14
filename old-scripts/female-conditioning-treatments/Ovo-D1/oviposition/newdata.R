####### OvoD1 Conditioning - 1:4 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_OvoD1 <- read_excel("data/rawresults_OvoD1_1-4_copy.xlsx")

bin_mod <- glm(cbind('1:4 Conditioned', '1:4 Unconditioned') ~ 1, data = one_to_four_OvoD1)


## Making the data long 
one_to_four_OvoD1_long <- one_to_four_OvoD1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Visualising the data 
one_to_four_OvoD1_plot <- one_to_four_OvoD1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot(outlier.shape  = NA)+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 Conditioned and Unconditioned Feeding")+
  theme(legend.position="none")+ 
  ylim(-0.01,2)+
  geom_jitter(aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              #width = 0.2,
              shape = 21)
## might have to look at mean average for this plot. 
one_to_four_OvoD1_plot


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





##### SHOWING THE MEAN AVERAGE FOR DATA VISUALISATION PURPOSES 
#### INSTALL PACKAGES 


#### Upload data
one_to_four_OvoD1_mean <- read_excel("data/OvoD1_1-4_mean.xlsx")

## Making the data long 
one_to_four_OvoD1_mean_long <- one_to_four_OvoD1_mean  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Making a summary of the data 
one_to_four_OvoD1_mean_summary <- one_to_four_OvoD1_mean_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


## A bar plot of the mean data
one_to_four_OvoD1_mean_plot <- one_to_four_OvoD1_mean_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity", aes(fill=diet))+
  scale_fill_brewer(palette = "Set2")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "black",
                width = 0.2)+
  geom_jitter(data = one_to_four_OvoD1_mean_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "black",
              colour = "skyblue",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 1.5)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "1:4 Conditioned Feeding")+
  theme_classic()+
  theme(legend.position = "none")



## 
## Statistical analysis ----
# First testing a linear model 
one_to_four_OvoD1_mean_lm <- lm(fly_numbers ~  diet, data = one_to_four_OvoD1_mean_long)

# Assumption Checking of the model 
performance::check_model(one_to_four_OvoD1_mean_lm, check = c("qq")) # qq looks pretty good
performance::check_model(one_to_four_OvoD1_mean_lm, check = c("homogeneity")) # bad, quite windy
performance::check_model(one_to_four_OvoD1_mean_lm, check = c("linearity")) # bad, quite windy again
performance::check_model(one_to_four_OvoD1_mean_lm, check = c("outliers")) # don't see anything particularly awful




# Trying a generalised linear model
one_to_four_OvoD1_mean_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = one_to_four_OvoD1_mean_long)
# underdispersed - trying quasipoisson? not the best thing to do though
one_to_four_OvoD1_mean_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = one_to_four_OvoD1_mean_long)


performance::check_model(one_to_four_OvoD1_mean_glm_2, check = c("qq")) # dots are close to the line but a bit shakey
performance::check_model(one_to_four_OvoD1_mean_glm_2, check = c("homogeneity")) # line looks pretty straight/ flat
performance::check_model(one_to_four_OvoD1_mean_glm_2, check = c("outliers")) # seems okay 

# glm with quasipoisson may be the better model out of the ones assumption/ performance checked. 

# summary function, shows t test
summary(one_to_four_OvoD1_mean_glm_2)

# using anova 
anova(one_to_four_OvoD1_mean_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(one_to_four_OvoD1_mean_glm_2, pairwise ~ diet)
## This shows a significant difference between 1:4 Conditioned and 1:4 Unconditioned. 


 bin_mod <- glm(cbind('1:4 Conditioned', '1:4 Unconditioned') ~ 1, data = one_to_four_OvoD1)

bin_mod <- glmer(cbind(`1:4 Conditioned`, `1:4 Unconditioned`) ~ Ratio + (plate/observation), data = one_to_four_OvoD1, family = binomial)

