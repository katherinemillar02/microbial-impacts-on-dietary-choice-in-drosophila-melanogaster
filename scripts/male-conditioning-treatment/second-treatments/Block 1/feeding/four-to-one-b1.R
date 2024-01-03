#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
four_to_one_b1 <- read_excel("data/4-1_t2.xlsx")

## Making the data long 
four_to_one_b1_long <- four_to_one_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
four_to_one_b1_plot <- four_to_one_b1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,7)+
  geom_jitter(data =  four_to_one_b1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
fourtoone_b1_lm <- lm(fly_numbers ~  diet, data = four_to_one_b1_long)

# Assumption Checking of the model 
performance::check_model(fourtoone_b1_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(fourtoone_b1_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(fourtoone_b1_lm, check = c("linearity")) # line is very flat.
performance::check_model(fourtoone_b1_lm, check = c("outliers"))




# Trying a generalised linear model
fourtoone_b1_glm_1  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = conditioned_b1.2_median_long)

summary(fourtoone_b1_glm_1) # underdispersed 

fourtoone_b1_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = conditioned_b1.2_median_long)


performance::check_model(fourtoone_b1_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(fourtoone_b1_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(fourtoone_b1_glm_2, check = c("outliers"))

# glm qq better for this repeat

# summary function, shows t test
summary(fourtoone_b1_glm_2)

# using anova 
anova(fourtoone_b1_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(fourtoone_b1_glm_2, pairwise ~ diet)


