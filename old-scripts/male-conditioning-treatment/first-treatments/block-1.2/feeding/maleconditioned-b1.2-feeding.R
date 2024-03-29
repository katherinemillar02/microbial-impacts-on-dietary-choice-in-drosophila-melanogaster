#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditioned_b1.2_median <- read_excel("data/conditioned_t1b1.2_median.xlsx")

## Making the data long 
conditioned_b1.2_median_long <- conditioned_b1.2_median %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
conditioned_b1.2_median_plot <- conditioned_b1.2_median_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male Conditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(0.01,6)+
  geom_jitter(data =  conditioned_b1.2_median_long,
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


