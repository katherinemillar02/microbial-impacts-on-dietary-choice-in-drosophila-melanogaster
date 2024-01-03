#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
four_to_one_b2 <- read_excel("data/4_1_t2b2_median.xlsx")

## Making the data long 
four_to_one_b2_long <- four_to_one_b2   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


four_to_one_b2_plot  <- four_to_one_b2_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,7)+
  geom_jitter(data =  four_to_one_b2_long ,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




## 
## Statistical analysis ----
# First testing a linear model 
fourtoone_b2_lm <- lm(fly_numbers ~  diet, data = four_to_one_b2_long)

# Assumption Checking of the model 
performance::check_model(fourtoone_b2_lm, check = c("qq")) # qq looks awful
performance::check_model(fourtoone_b2_lm, check = c("homogeneity")) # bad
performance::check_model(fourtoone_b2_lm, check = c("linearity")) # bad
performance::check_model(fourtoone_b2_lm, check = c("outliers"))




# Trying a generalised linear model
fourtoone_b2_glm_1  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = four_to_one_b2_long)
# overdispersed - trying quasipoisson
fourtoone_b2_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = four_to_one_b2_long)


performance::check_model(fourtoone_b2_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(fourtoone_b2_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(fourtoone_b2_glm_2, check = c("outliers"))

# homogeneity looks a lot better in glm and qq looks slighlty bettee too

# summary function, shows t test
summary(fourtoone_b2_glm_2)

# using anova 
anova(fourtoone_b2_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(fourtoone_b2_glm_2, pairwise ~ diet)
## sig difference - conditioned more preferred 

