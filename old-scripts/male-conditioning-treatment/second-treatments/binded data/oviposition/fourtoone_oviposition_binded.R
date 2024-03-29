fourone_oviposition_b2_long
four_to_one_oviposition_b1_long


fourone_oviposition_t2_bind_1 <- four_to_one_oviposition_b1_long %>% mutate(experiment = "one")
fourone_oviposition_t2_bind_2 <- fourone_oviposition_b2_long %>% mutate(experiment = "two")


fourone_oviposition_t2 <- rbind(fourone_oviposition_t2_bind_1, fourone_oviposition_t2_bind_2)

male_t2_bind_conditioned_4to1_plot  <- 
  fourone_oviposition_t2 %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 Conditioned and Unconditioned Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,150)+
  geom_jitter(data =  
                fourone_oviposition_t2,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

########
library(ggplot2)
library(ggpattern)
## new plot design
male_oviposition_1 <-
  ggplot(fourone_oviposition_t2, aes(x = diet, y = egg_numbers, pattern = diet, fill = diet)) +
  geom_boxplot(aes(fill = diet))+
  scale_fill_manual(name = "Diet", values = c("#FF7F50","#FF7F50" )) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
                       color = "black",
                       pattern_fill = "white",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("stripe", "none", "stripe", "none")) +
  ylim(0,250)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_jitter(data = fourone_oviposition_t2 ,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21) +
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "")+
  theme(legend.position="none")+
  theme(axis.text = element_text(size=6, angle = 0, hjust =0.5))




## 
#### Data Analysis ----
# First testing a linear model 
experiment_t2_sig_fourone <- lm(egg_numbers ~  experiment, data =  fourone_oviposition_t2)

## looking for sig across treatments for feeding number
drop1(experiment_t2_sig_fourone, test = "F")
## no sig between the two so can use?

## trying out a linear model
experiment_t2_fourone_lm <- lm(egg_numbers ~ diet, data = fourone_oviposition_t2)

# Assumption Checking of the model 
performance::check_model(experiment_t2_fourone_lm , check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straightperformance::check_model(egg_analysis_combined_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(experiment_t2_fourone_lm , check = c("linearity")) # line is very much not flat.
performance::check_model(experiment_t2_fourone_lm , check = c("homogeneity"))
performance::check_model(experiment_t2_fourone_lm, check = c("outliers"))

# doesn't look awful

# Trying a generalised linear model
experiment_t2_fourone_glm <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = fourone_oviposition_t2)
summary(experiment_t2_fourone_glm) # overdispersed

experiment_t2_fourone_glm_2  <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = fourone_oviposition_t2)


#### Assumption Checking ####
performance::check_model(experiment_t2_fourone_glm_2 , check = c("qq")) # dots seem to match to line better than lm
performance::check_model(experiment_t2_fourone_glm_2  , check = c("homogeneity")) # not flat but better
performance::check_model(experiment_t2_fourone_glm_2  , check = c("outliers"))

# glm may be better 

# summary function, shows t test
summary(experiment_t2_fourone_glm_2)

# using anova 
anova(experiment_t2_fourone_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(experiment_t2_fourone_glm_2, pairwise ~ diet)

# there is a sig diff between 4:1C and 4:1 UC 
