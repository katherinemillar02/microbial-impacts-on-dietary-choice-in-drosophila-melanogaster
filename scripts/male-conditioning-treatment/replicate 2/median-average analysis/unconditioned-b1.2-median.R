#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b1.2_median <- read_excel("data/male_block1.2_unconditioned.xlsx")

## Making the data long 
unconditioned_b1.2_median_long <- unconditioned_b1.2_median %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
unconditioned_b1.2_median_plot <- unconditioned_b1.2_median_long %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#1A85FF", "#D41159"))+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  unconditioned_b1.2_median_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



conditioned_b1.2_median_plot + unconditioned_b1.2_median_plot 

## 
## Statistical analysis ----
# First testing a linear model 
unconditioned__b1.2_median_lm <- lm(fly_numbers ~  diet, data = unconditioned_b1.2_median_long)

# Assumption Checking of the model 
performance::check_model(unconditioned__b1.2_median_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(unconditioned__b1.2_median_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(unconditioned__b1.2_median_lm, check = c("linearity")) # line is very flat.
performance::check_model(unconditioned__b1.2_median_lm, check = c("outliers"))




# Trying a generalised linear model
unconditioned__b1.2_median_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_b1.2_median_long)
# would not let me do poisson - but choosing glm

performance::check_model(unconditioned__b1.2_median_glm, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(unconditioned__b1.2_median_glm, check = c("homogeneity")) # not flat but better
performance::check_model(unconditioned__b1.2_median_glm, check = c("outliers"))

# glm qq better for this repeat

# summary function, shows t test
summary(unconditioned__b1.2_median_lm)

# using anova 
anova(conditioned_rep2_lm)

# emmeans for tukey analysis 
emmeans::emmeans(unconditioned__b1.2_median_lm, pairwise ~ diet)
