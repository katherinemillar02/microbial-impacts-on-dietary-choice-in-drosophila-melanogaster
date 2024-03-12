#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)

#### Data upload and summaries ----
## Import Data
unconditioned_diets_rep1 <- read_excel("data/not_conditioned.xlsx")

## Making the data long 
unconditioned_diets_rep1_long <- unconditioned_diets_rep1 %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
unconditioned_diets_rep1_summary <- unconditioned_diets_rep1_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data ----
# creating a boxplot with the data that has been summarised
unconditioned_diets_rep1_plot <- unconditioned_diets_rep1_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = unconditioned_diets_rep1_long ,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 1.25)+
  labs(x = "Diet condition",
       y = "Mean +/- S.E. Number of flies per diet patch",
       title = "Unconditioned Diet Patches Rep 1")+
  theme_classic() 

## Combining the two plots 
conditioned_notconditioned_treatments <- conditioned_diets_rep1_plot + unconditioned_diets_rep1_plot
 
 # Saving the plots to a plots file
# not conditioned plot
ggsave("plots/unconditioned_diets_rep1_plot.png", dpi=300)
# a patchwork of male conditioned and not conditioned plot
ggsave("plots/conditioned_notconditioned_treatments", dpi=300)


#### Data Analysis ----

# First testing a linear model 
unconditioned_rep1_lm <- lm(fly_numbers ~  diet, data = unconditioned_diets_rep1_long)

# Assumption Checking of the model 
performance::check_model(unconditioned_rep1_lm, check = c("qq")) # a bit poor 
performance::check_model(unconditioned_rep1_lm, check = c("homogeneity")) # not great
performance::check_model(unconditioned_rep1_lm, check = c("linearity")) # not great
performance::check_model(unconditioned_rep1_lm, check = c("outliers"))


# Trying a generalised linear model
unconditioned_rep1_glm_01 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = unconditioned_diets_rep1_long)
summary(unconditioned_rep1_glm_01) # underdispersed 

# quasi for underdispersion but need to check this
unconditioned_rep1_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_diets_rep1_long)


performance::check_model(unconditioned_rep1_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(unconditioned_rep1_glm, check = c("homogeneity")) # not great either but dots do appear?
performance::check_model(unconditioned_rep1_glm, check = c("outliers"))

# similar to lm but glm may be better 

# summary function, shows t test
summary(unconditioned_rep1_glm)

# using anova 
anova(unconditioned_rep1_glm)

# emmeans for tukey
emmeans::emmeans(unconditioned_rep1_glm, pairwise ~ diet)

# maybe using lm instead
emmeans::emmeans(unconditioned_rep1_lm, pairwise ~ diet)
 