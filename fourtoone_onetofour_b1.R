#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
onetofour_fourtoone_b1 <- read_excel("data/4-1_1-4_mc_.xlsx")

## Making the data long 
onetofour_fourtoone_b1_long <- onetofour_fourtoone_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
onetofour_fourtoone_b1_plot <- onetofour_fourtoone_b1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male 1:4 Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,5)+
  geom_jitter(data =  onetofour_fourtoone_b1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
conditioned__b1.2_median_lm <- lm(fly_numbers ~  diet, data = conditioned_b1.2_median_long)

# Assumption Checking of the model 
performance::check_model(conditioned__b1.2_median_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(conditioned__b1.2_median_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(conditioned__b1.2_median_lm, check = c("linearity")) # line is very flat.
performance::check_model(conditioned__b1.2_median_lm, check = c("outliers"))




# Trying a generalised linear model
conditioned__b1.2_median_glm  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = conditioned_b1.2_median_long)
# would not let me do poisson - but choosing glm

performance::check_model(conditioned__b1.2_median_glm , check = c("qq")) # dots seem to match to line better than lm
performance::check_model(conditioned__b1.2_median_glm , check = c("homogeneity")) # not flat but better
performance::check_model(conditioned__b1.2_median_glm , check = c("outliers"))

# glm qq better for this repeat

# summary function, shows t test
summary(conditioned_rep2_lm)

# using anova 
anova(conditioned_rep2_lm)

# emmeans for tukey analysis 
emmeans::emmeans(conditioned__b1.2_median_glm, pairwise ~ diet)


