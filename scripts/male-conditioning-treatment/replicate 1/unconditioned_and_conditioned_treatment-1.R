#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(dplyr)


## Import Data
unconditioned_and_conditioned_rep1 <- read_excel("data/both_conditioned.xlsx")






## Making the data long 
unconditioned_and_conditioned_rep1_long <- unconditioned_and_conditioned_rep1%>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



## Summarising the data; mean, sd, se
unconditioned_and_conditioned_rep1_summary <- unconditioned_and_conditioned_rep1_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
unconditioned_and_conditioned_rep1_plot <- unconditioned_and_conditioned_rep1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = unconditioned_and_conditioned_rep1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 0.75)+
  labs(x = "Diet Condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 1")+
  theme_classic() 

##  Saving the plot to a plots file
ggsave("plots/unconditioned_and_conditioned_rep1_plot.png", dpi=300)



#### Data Analysis 


# First testing a linear model 
conditioned_and_unconditioned_rep1_lm <- lm(fly_numbers ~  diet, data = unconditioned_and_conditioned_rep1_long)

# Assumption Checking of the model 
performance::check_model(conditioned_and_unconditioned_rep1_lm, check = c("qq")) # I think qqplot looks okay
performance::check_model(conditioned_and_unconditioned_rep1_lm, check = c("homogeneity")) # not great
performance::check_model(conditioned_and_unconditioned_rep1_lm, check = c("linearity")) # not great
performance::check_model(conditioned_and_unconditioned_rep1_lm, check = c("outliers"))


# Trying a generalised linear model
conditioned_and_unconditioned_rep1_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_and_conditioned_rep1_long)


performance::check_model(conditioned_and_unconditioned_rep1_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_and_unconditioned_rep1_glm, check = c("homogeneity")) # not great either but dots do appear?
performance::check_model(conditioned_and_unconditioned_rep1_glm, check = c("outliers"))

# pretty much the same - go with glm

# summary function, shows t test
summary(conditioned_and_unconditioned_rep1_glm)

# using anova 
anova(conditioned_and_unconditioned_rep1_glm)

# emmeans for tukey
emmeans::emmeans(conditioned_and_unconditioned_rep1_glm, pairwise ~ diet)


# No significance... lack of results? Maybe best to combine? 



## Splitting the data into variables
unconditioned_and_conditioned_rep1_long$condition <- ifelse(unconditioned_and_conditioned_rep1_long$diet %in% c("4:1 Conditioned", "1:4 Conditioned"), "Conditioned", "Unconditioned")
unconditioned_and_conditioned_rep1_long$nutrient_composition <- ifelse(unconditioned_and_conditioned_rep1_long$diet %in% c("4:1 Conditioned", "4:1 Unconditioned"), "4:1", "1:4")

## summary of split variables data 

# nutrient composition
unconditioned_and_conditioned_rep1_nutrient_summary <- unconditioned_and_conditioned_rep1_long  %>%  
  group_by(nutrient_composition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


# conditioned or unconditioned 
unconditioned_and_conditioned_rep1_condition_summary <- unconditioned_and_conditioned_rep1_long  %>%  
  group_by(condition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


## Visualising the data
# creating a boxplot with the data that has been summarised
unconditioned_and_conditioned_rep1_nutrient_plot <- unconditioned_and_conditioned_rep1_nutrient_summary  %>% 
  ggplot(aes(x = nutrient_composition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = unconditioned_and_conditioned_rep1_long,
              aes(x = nutrient_composition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 0.75)+
  labs(x = "Diet Condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 1")+
  theme_classic() 

# creating a boxplot with the data that has been summarised - condition
unconditioned_and_conditioned_rep1_condition_plot <- unconditioned_and_conditioned_rep1_condition_summary %>% 
  ggplot(aes(x = condition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = unconditioned_and_conditioned_rep1_long,
              aes(x = condition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 0.75)+
  labs(x = "Diet Condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 1")+
  theme_classic() 


#### Data Analysis of separated variables


# First testing a linear model 
conditioned_and_unconditioned_rep1_separate_lm <- lm(fly_numbers ~  nutrient_composition * condition, data = unconditioned_and_conditioned_rep1_long)

drop1(conditioned_and_unconditioned_rep1_separate_lm, test = "F")


# Assumption Checking of the model 
performance::check_model(conditioned_and_unconditioned_rep1_separate_lm , check = c("qq")) # I think qqplot looks okay
performance::check_model(conditioned_and_unconditioned_rep1_separate_lm , check = c("homogeneity")) # not great
performance::check_model(conditioned_and_unconditioned_rep1_separate_lm , check = c("linearity")) # not great
performance::check_model(conditioned_and_unconditioned_rep1_separate_lm , check = c("outliers"))



# Trying a generalised linear model
conditioned_and_unconditioned_rep1_separate_glm <- glm(fly_numbers ~  nutrient_composition * condition, family = quasipoisson(link = "log"), data = unconditioned_and_conditioned_rep1_long)

drop1(conditioned_and_unconditioned_rep1_separate_glm, test = "F")
# no interaction effect? 

performance::check_model(conditioned_and_unconditioned_rep1_separate_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_and_unconditioned_rep1_separate_glm, check = c("homogeneity")) # not great either but dots do appear?
performance::check_model(conditioned_and_unconditioned_rep1_separate_glm, check = c("outliers"))

# pretty much the same - go with glm

# summary function, shows t test
summary(conditioned_and_unconditioned_rep1_separate_glm)

# using anova 
anova(conditioned_and_unconditioned_rep1_separate_glm)


