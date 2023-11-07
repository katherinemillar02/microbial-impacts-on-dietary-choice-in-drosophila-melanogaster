#### INSTALL PACKAGES 
library(tidyverse)  
library(readxl)
library(patchwork)

#### CALCULATING THE MEAN AVERAGE 

## Import Data
conditioned_and_unconditioned_diets_2 <- read_excel("data/male_conditionedandunconditioned_rep2_mean.xlsx")

## Making the data long 
conditioned_and_unconditioned_diets_2_long <- conditioned_and_unconditioned_diets_2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



## Summarising the data; mean, sd, se
conditioned_and_unconditioned_diets_2_summary <- conditioned_and_unconditioned_diets_2_long   %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


## Visualising the mean average with a bar plot
# creating a boxplot with the data that has been summarised
conditioned_and_unconditioned_diets_2_plot <- conditioned_and_unconditioned_diets_2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = conditioned_and_unconditioned_diets_2_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet Condition",
       y = "Mean +/- S.E. Number of flies per diet patch",
       title = "Conditioned and Unconditioned Diet Patches Rep 2")+
  theme_classic() 

ggsave("plots/conditioned_and_unconditioned_diets_2_plot.png", dpi=300)




#### Data Analysis 


# First testing a linear model 
conditioned_and_unconditioned_rep2_lm <- lm(fly_numbers ~  diet, data = conditioned_and_unconditioned_diets_2_long)

# Assumption Checking of the model 
performance::check_model(conditioned_and_unconditioned_rep2_lm, check = c("qq")) # I think qqplot looks okay
performance::check_model(conditioned_and_unconditioned_rep2_lm, check = c("homogeneity")) # not great
performance::check_model(conditioned_and_unconditioned_rep2_lm, check = c("linearity")) # not great
performance::check_model(conditioned_and_unconditioned_rep2_lm, check = c("outliers"))


# Trying a generalised linear model
conditioned_and_unconditioned_rep2_glm_01 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = conditioned_and_unconditioned_diets_2_long)
summary(conditioned_and_unconditioned_rep2_glm_01) # underdispersed 

# using quasi for now for underdispersion
conditioned_and_unconditioned_rep2_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = conditioned_and_unconditioned_diets_2_long)


performance::check_model(conditioned_and_unconditioned_rep2_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_and_unconditioned_rep2_glm, check = c("homogeneity")) # not great either but dots do appear?
performance::check_model(conditioned_and_unconditioned_rep2_glm, check = c("outliers"))

# go with glm

# summary function, shows t test
summary(conditioned_and_unconditioned_rep2_glm)

# using anova 
anova(conditioned_and_unconditioned_rep2_glm)

# emmeans for tukey
emmeans::emmeans(conditioned_and_unconditioned_rep2_glm, pairwise ~ diet)


## Splitting the data

## Splitting the data into variables
conditioned_and_unconditioned_diets_2_long$condition <- ifelse(conditioned_and_unconditioned_diets_2_long$diet %in% c("4:1 Conditioned", "1:4 Conditioned"), "Conditioned", "Unconditioned")
conditioned_and_unconditioned_diets_2_long$nutrient_composition <- ifelse(conditioned_and_unconditioned_diets_2_long$diet %in% c("4:1 Conditioned", "4:1 Unconditioned"), "4:1", "1:4")

## summary of split variables data 

# nutrient composition
unconditioned_and_conditioned_rep2_nutrient_summary <- conditioned_and_unconditioned_diets_2_long  %>%  
  group_by(nutrient_composition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


# conditioned or unconditioned 
unconditioned_and_conditioned_rep2_condition_summary <- conditioned_and_unconditioned_diets_2_long  %>%  
  group_by(condition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


## Visualising the data
# creating a boxplot with the data that has been summarised
unconditioned_and_conditioned_rep2_nutrient_plot <- unconditioned_and_conditioned_rep2_nutrient_summary  %>% 
  ggplot(aes(x = nutrient_composition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = conditioned_and_unconditioned_diets_2_long,
              aes(x = nutrient_composition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.5)+
  labs(x = "Diet Condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 1")+
  theme_classic() 

# creating a boxplot with the data that has been summarised - condition
unconditioned_and_conditioned_rep2_condition_plot <- unconditioned_and_conditioned_rep2_condition_summary %>% 
  ggplot(aes(x = condition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = conditioned_and_unconditioned_diets_2_long,
              aes(x = condition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.5)+
  labs(x = "Diet Condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 1")+
  theme_classic() 


#### visualising the separate variables 
unconditioned_and_conditioned_rep2_nutrient_plot + unconditioned_and_conditioned_rep2_condition_plot


#### Data Analysis of separated variables


# First testing a linear model 
conditioned_and_unconditioned_rep2_separate_lm <- lm(fly_numbers ~  nutrient_composition * condition, data = conditioned_and_unconditioned_diets_2_long)




# Assumption Checking of the model 
performance::check_model(conditioned_and_unconditioned_rep2_separate_lm , check = c("qq")) # I think qqplot looks okay
performance::check_model(conditioned_and_unconditioned_rep2_separate_lm , check = c("homogeneity")) # not too straight
performance::check_model(conditioned_and_unconditioned_rep2_separate_lm , check = c("linearity")) # very straight
performance::check_model(conditioned_and_unconditioned_rep2_separate_lm , check = c("outliers"))



# Trying a generalised linear model
conditioned_and_unconditioned_rep2_separate_glm01 <- glm(fly_numbers ~  nutrient_composition * condition, family = poisson(link = "log"), data = conditioned_and_unconditioned_diets_2_long)
summary(conditioned_and_unconditioned_rep2_separate_glm01)  #underdispersed again? 

# using quasi but need rec on what to do when there is underdispersion with count data? 
conditioned_and_unconditioned_rep2_separate_glm <- glm(fly_numbers ~  nutrient_composition * condition, family = quasipoisson(link = "log"), conditioned_and_unconditioned_diets_2_long)



performance::check_model(conditioned_and_unconditioned_rep1_separate_glm, check = c("qq")) # looks bit worse than the lm
performance::check_model(conditioned_and_unconditioned_rep1_separate_glm, check = c("homogeneity")) # straighter but still not great
performance::check_model(conditioned_and_unconditioned_rep1_separate_glm, check = c("outliers"))

# i think lm is better
drop1(conditioned_and_unconditioned_rep2_separate_lm, test = "F") 


