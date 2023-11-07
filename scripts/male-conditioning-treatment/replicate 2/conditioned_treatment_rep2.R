#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)

#### CALCULATING THE MEAN AVERAGE 

#### Data upload and summaries ----
## Import Data
conditioned_diets_2 <- read_excel("data/male_conditioned_rep2_mean.xlsx")

## Making the data long 
conditioned_diets_2_long <- conditioned_diets_2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "fly_numbers")



## Summarising the data; mean, sd, se
conditioned_diets_2_summary <- conditioned_diets_2_long   %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


## Visualising the data ----
# creating a boxplot with the data that has been summarised
conditioned_diets_2_plot <- conditioned_diets_2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = conditioned_diets_2_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.5)+
  labs(x = "Diet condition",
       y = "Mean +/- S.E. Number of flies per diet patch",
       title = "Conditioned Diet Patches Rep 2")+
  theme_classic() 

## saving plots to a plot folder
ggsave("plots/conditioned_diets_2_plot.png", dpi=300)


## Statistical analysis ----
# First testing a linear model 
conditioned_rep2_lm <- lm(fly_numbers ~  diet, data = conditioned_diets_2_long)

# Assumption Checking of the model 
performance::check_model(conditioned_rep2_lm, check = c("qq")) # I think qqplot looks okay, even better than the lm for rep 1
performance::check_model(conditioned_rep2_lm, check = c("homogeneity")) # line is more flat than rep 1 lm
performance::check_model(conditioned_rep2_lm, check = c("linearity")) # line is more flat than rep 1 lm
performance::check_model(conditioned_rep2_lm, check = c("outliers"))




# Trying a generalised linear model
conditioned_rep2_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = conditioned_diets_2_long)


performance::check_model(conditioned_rep2_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned_rep2_glm, check = c("homogeneity")) # not flat
performance::check_model(conditioned_rep1_lm, check = c("outliers"))

# glm qq better for this repeat, but others not, go with lm for rep 2

# summary function, shows t test
summary(conditioned_rep2_lm)

# using anova 
anova(conditioned_rep2_lm)

# emmeans for tukey
emmeans::emmeans(conditioned_rep2_lm, pairwise ~ diet)



