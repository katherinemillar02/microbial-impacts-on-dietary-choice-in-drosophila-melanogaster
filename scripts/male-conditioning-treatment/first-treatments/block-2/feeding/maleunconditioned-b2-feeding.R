#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b2_median <- read_excel("data/uc-males-b2.xlsx")

## Making the data long 
unconditioned_b2_median_long  <- unconditioned_b2_median    %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
unconditioned_b2_median_plot <- unconditioned_b2_median_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male Unconditioned Treatment Block 2")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  unconditioned_b2_median_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

conditioned_b2_median_plot + unconditioned_b2_median_plot



## 
## Statistical analysis ----
# First testing a linear model 
unconditioned_b2_lm <- lm(fly_numbers ~  diet, data = unconditioned_b2_median_long)

# Assumption Checking of the model 
performance::check_model(unconditioned_b2_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(unconditioned_b2_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(unconditioned_b2_lm, check = c("linearity")) # line is very flat.
performance::check_model(unconditioned_b2_lm, check = c("outliers"))




# Trying a generalised linear model
unconditioned_b2_glm <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = unconditioned_b2_median_long)
summary(unconditioned_b2_glm) #underdispersed



## doing  quasipoisson for now
unconditioned_b2_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_b2_median_long)



performance::check_model(unconditioned_b2_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(unconditioned_b2_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(unconditioned_b2_glm_2, check = c("outliers"))

# glm looks better? 

# summary function, shows t test
summary(unconditioned_b2_glm_2)

# using anova 
anova(unconditioned_b2_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(unconditioned_b2_glm_2, pairwise ~ diet)
