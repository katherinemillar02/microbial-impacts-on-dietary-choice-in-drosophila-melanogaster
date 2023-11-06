#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)

## Import Data
conditioned_diets_rep1 <- read_excel("data/male_conditioning.xlsx")

## Making the data long 
male_conditioned_diets_long <- conditioned_diets_rep1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
male_conditioned_diets_summary <- male_conditioned_diets_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# Creating a boxplot with the data that has been summarised
conditioned_diets_rep1_plot <- male_conditioned_diets_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#E57157",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#E57157",
                width = 0.2)+
  geom_jitter(data = male_conditioned_diets_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.5)+
  labs(x = "Diet Condition",
       y = "Mean +/- S.E. Number of flies per patch",
       title = "Male Conditioned Diet Patches Rep 1")+
  theme_classic() 

## Saving the plot to a plots file
ggsave("plots/conditioned_diets_rep1_plot.png", dpi=300)


#### Data Analysis 

# First testing a linear model 
conditioned_rep1_lm <- lm(fly_numbers ~  diet, data = male_conditioned_diets_long)

# Assumption Checking of the model 
performance::check_model(conditioned_rep1_lm, check = c("qq")) # I think qqplot looks okay
performance::check_model(conditioned_rep1_lm, check = c("homogeneity")) # not great
performance::check_model(conditioned_rep1_lm, check = c("linearity")) # not great
performance::check_model(conditioned_rep1_lm, check = c("outliers"))


# Trying a generalised linear model
conditioned_rep1_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = male_conditioned_diets_long)


performance::check_model(conditioned_rep1_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_rep1_glm, check = c("homogeneity")) # not great either but dots do appear?
performance::check_model(conditioned_rep1_lm, check = c("outliers"))

# glm may be better 

# summary function, shows t test
summary(conditioned_rep1_glm)

# using anova 
anova(conditioned_rep1_glm)

# emmeans for tukey
emmeans::emmeans(conditioned_rep1_glm, pairwise ~ diet)

# statistical tests show no significance
