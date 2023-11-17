#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditionedandunconditioned_b1.2_median <- read_excel("data/male_block1.2_conditionedandunconditioned.xlsx")

## Making the data long 
conditionedandunconditioned_b1.2_median_long <- conditionedandunconditioned_b1.2_median %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
## Making the data long 
conditionedandunconditioned_b1.2_median_plot <- 
  conditionedandunconditioned_b1.2_median_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "BuPu")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male Conditioned and Unconditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,5)+
  geom_jitter(data =  ## Making the data long 
                conditionedandunconditioned_b1.2_median_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



## 
## Statistical analysis ----
# First testing a linear model 
conditionedandunconditioned_b1.2_median_lm <- lm(fly_numbers ~  diet, data = conditionedandunconditioned_b1.2_median_long)

# Assumption Checking of the model 
performance::check_model(conditionedandunconditioned_b1.2_median_lm, check = c("qq")) # Looks a bit weird.
performance::check_model(conditionedandunconditioned_b1.2_median_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(conditionedandunconditioned_b1.2_median_lm, check = c("linearity")) # line is very flat.
performance::check_model(conditionedandunconditioned_b1.2_median_lm, check = c("outliers"))




# Trying a generalised linear model
conditionedandunconditioned_b1.2_median_glm  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = conditionedandunconditioned_b1.2_median_long)
# would not let me do poisson - but choosing glm

performance::check_model(conditionedandunconditioned_b1.2_median_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditionedandunconditioned_b1.2_median_glm, check = c("homogeneity")) # not flat but better
performance::check_model(conditionedandunconditioned_b1.2_median_glm, check = c("outliers"))

# glm better

# summary function, shows t test
summary(conditionedandunconditioned_b1.2_median_glm )

# using anova 
anova(conditioned_rep2_lm)

# emmeans for tukey analysis 
emmeans::emmeans(conditionedandunconditioned_b1.2_median_lm,  pairwise ~ diet)

