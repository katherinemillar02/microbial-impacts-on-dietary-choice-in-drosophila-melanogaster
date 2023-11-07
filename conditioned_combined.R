#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)



# Putting together data 
conditioned_diets_combined <- rbind(conditioned_diets_rep1,conditioned_diets_2)

## Making the data long 
conditioned_diets_combined_long <- conditioned_diets_combined %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "fly_numbers")


## Summarising the data; mean, sd, se
conditioned_diets_combined_summary  <- conditioned_diets_combined_long %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))



## Visualising the data
# Creating a boxplot with the data that has been summarised
conditioned_diets_combined_plot <- conditioned_diets_combined_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#E57157",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#E57157",
                width = 0.2)+
  geom_jitter(data = conditioned_diets_combined_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.5)+
  labs(x = "Diet Condition",
       y = "Mean +/- S.E. Number of flies per patch",
       title = "Male Conditioned Diet Patches Combined")+
  theme_classic() 





#### Data Analysis 

# First testing a linear model 
conditioned_combined_lm <- lm(fly_numbers ~  diet, data = conditioned_diets_combined_long)

# Assumption Checking of the model 
performance::check_model(conditioned_combined_lm, check = c("qq")) # points are a bit dispersed 
performance::check_model(conditioned_combined_lm, check = c("homogeneity")) # not great
performance::check_model(conditioned_combined_lm, check = c("linearity")) # not great
performance::check_model(conditioned_combined_lm, check = c("outliers"))


# Trying a generalised linear model
conditioned_combined_glm01 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = conditioned_diets_combined_long)
summary(conditioned_combined_glm01) # overdispersion with poisson

# glm with quasipoisson
conditioned_combined_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = conditioned_diets_combined_long)

# assumption checking
performance::check_model(conditioned_combined_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_combined_glm, check = c("homogeneity")) # not great either but dots do appear?
performance::check_model(conditioned_combined_glm, check = c("outliers")) # same as lm

# glm may be better 

# summary function, shows t test
summary(conditioned_combined_glm)

# using anova 
anova(conditioned_combined_glm)

# emmeans for tukey
emmeans::emmeans(conditioned_combined_glm, pairwise ~ diet)

# combined data actually shows significance 
