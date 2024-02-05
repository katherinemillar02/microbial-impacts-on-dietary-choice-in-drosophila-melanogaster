####### OvoD1 Conditioning - 4:1 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data for plot - median calculated 
four_to_one_OvoD1 <- read_excel("data/female_conditioning/ovod1/block_1/OvoD1-4_1-median.xlsx")

## Making the median data long 
four_to_one_OvoD1_long <- four_to_one_OvoD1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Visualising the data - median data 
four_to_one_OvoD1_plot <- four_to_one_OvoD1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 OvoD1 Female Conditioned and Unconditioned Feeding")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  four_to_one_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)





#### Data Analysis 
## analysis of the median calculated data - may ignore this one 
#### Second analysis of results 
#### Upload data

## 
## Statistical analysis ----
# First testing a linear model 
four_to_one_OvoD1_lm <- lm(fly_numbers ~  diet, data = four_to_one_OvoD1_long)

# Assumption Checking of the model 
performance::check_model(four_to_one_OvoD1_lm, check = c("qq")) # qq looks okay, dots are a bit spaced out, could be a lot better. 
performance::check_model(four_to_one_OvoD1_lm, check = c("homogeneity")) # quite windy
performance::check_model(four_to_one_OvoD1_lm, check = c("linearity")) # quite windy, not the best
performance::check_model(four_to_one_OvoD1_lm, check = c("outliers")) # don't see anything particularly awful. 




# Trying a generalised linear model
four_to_one_OvoD1_glm_1 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), four_to_one_OvoD1_long)
# overdispersed - trying quasipoisson? not the best thing to do though
four_to_one_OvoD1_glm_2 <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), four_to_one_OvoD1_long)

## Assumption Checking
performance::check_model(four_to_one_OvoD1_glm_2 , check = c("qq")) # lines go a bit sideways compared to lm.
performance::check_model(four_to_one_OvoD1_glm_2 , check = c("homogeneity")) # line is flat almost, bit high. 
performance::check_model(four_to_one_OvoD1_glm_2 , check = c("outliers")) # seems okay? 

# lm may be better? quite similar though.  

# summary function, shows t test
summary(four_to_one_OvoD1_glm_2)

# using anova 
anova(four_to_one_OvoD1_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(four_to_one_OvoD1_glm_2, pairwise ~ diet)
## This shows there is not a significant difference between 4:1 Conditioned and 4:1 Unconditioned.




##### NEW ANALYSIS 
# Analysing from the raw data # 

##  uploading the raw data 
four_to_one_OvoD1_raw <- read_excel("data/female_conditioning/ovod1/block_1/rawdata_OvoD1_4-1.xlsx")

## making the data long, 
four_to_one_OvoD1_raw_long <- four_to_one_OvoD1_raw  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## model binomial - cannot get to work. 
bin_mod_od1_fourone <- glm(cbind('4:1 Conditioned', '4:1 Unconditioned') ~ 2, data = four_to_one_OvoD1_raw, family = binomial)

bin_mod <- glmer(cbind( "4:1 Conditioned", "4:1 Unconditioned") + (plate/observation), data = four_to_one_OvoD1_raw, family = binomial)

test <- cbind( "4:1 Conditioned", "4:1 Unconditioned") 
bin_mod_od1_fourone <- glm(test ~ 1, data = four_to_one_OvoD1_raw, family = binomial)
## Can't get this to work
## Error in `[[<-.data.frame`(`*tmp*`, i, value = 1:2) : replacement has 2 rows, data has 1

## trying ~ 2
bin_mod_od1_fourone <- glm(test ~ 2, data = four_to_one_OvoD1_raw, family = binomial)
# Error in terms.formula(formula, data = data) : invalid model formula in ExtractVars
# Error in as.data.frame.default(data, optional = TRUE) : cannot coerce class ‘"function"’ to a data.frame

