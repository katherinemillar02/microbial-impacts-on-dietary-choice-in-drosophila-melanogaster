#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_b1 <- read_excel("data/mean-1-4_t2_b2.xlsx")

## Making the data long 
one_to_four_b1_long <- one_to_four_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

one_to_four_b1_summary <- one_to_four_b1_long %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


one_to_four_b1_plot <- one_to_four_b1_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity", aes(fill=diet))+
  scale_fill_brewer(palette = "Set2")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = one_to_four_b1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 1.5)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "1:4 Diets")+
  theme_classic()+
  theme(legend.position = "none")





## 
## Statistical analysis ----
# First testing a linear model 
onetofour_b1_lm <- lm(fly_numbers ~  diet, data = one_to_four_b1_long)

# Assumption Checking of the model 
performance::check_model(onetofour_b1_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(onetofour_b1_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(onetofour_b1_lm, check = c("linearity")) # line is very flat.
performance::check_model(onetofour_b1_lm, check = c("outliers"))




# Trying a generalised linear model
onetofour_b1_glm_1  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = one_to_four_b1_long)
# would not let me do poisson - but choosing glm
onetofour_b1_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = one_to_four_b1_long)


performance::check_model(onetofour_b1_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_b1_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_b1_glm_2, check = c("outliers"))

# lm may be better

# summary function, shows t test
summary(onetofour_b1_lm)

# using anova 
anova(onetofour_b1_lm)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_b1_lm, pairwise ~ diet)


