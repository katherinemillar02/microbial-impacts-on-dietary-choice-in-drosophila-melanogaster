# Combining Conditioned and Unconditioned

# mutating a variable to the data files
oviposition_analysis_cond_b1.1 <- conditioned_b1.1_egg_long %>% mutate(treatment="conditioned")
oviposition_analysis_cond_b1.1 <- unconditioned_b1.1_egg_long %>% mutate(treatment="unconditioned")

## binding these two data sets 
oviposition_analysis_b1.1 <- rbind(oviposition_analysis_uncond_b1.1, oviposition_analysis_cond_b1.1)

# visualising the data 
oviposition_analysis_b1.1_plot <- oviposition_analysis_b1.1 %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Conditioned Treatment Eggs 1.1")+
  theme(legend.position="none")+
  ylim(0,150)+
  geom_jitter(data =  oviposition_analysis_b1.1, 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


#### Data analysis 

# looking for an interaction effect between treatment and egg number 
oviposition_b1.1_sig <- lm(egg_numbers ~ treatment + diet, data = oviposition_analysis_b1.1)

drop1(oviposition_b1.1_sig, test= "F") ## egg numbers are similar across treatments in general

## doing some more models 
# First testing a linear model 
oviposition_b1.1_lm <- lm(egg_numbers ~  diet, data = oviposition_analysis_b1.1)

# Assumption Checking of the model 
performance::check_model(oviposition_b1.1_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straight
performance::check_model(oviposition_b1.1_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(oviposition_b1.1_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(oviposition_b1.1_lm, check = c("outliers"))



# Trying a generalised linear model
oviposition_b1.1_glm <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = oviposition_analysis_b1.1)
summary(oviposition_b1.1_glm) ## underdispersed



## trying a new glm
oviposition_b1.1_glm_2 <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = oviposition_analysis_b1.1)



performance::check_model(oviposition_b1.1_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(oviposition_b1.1_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(oviposition_b1.1_glm_2, check = c("outliers"))

# glm looks a lot better

# summary function, shows t test
summary(oviposition_b1.1_glm_2)

# using anova 
anova(oviposition_b1.1_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(oviposition_b1.1_glm_2, pairwise ~ diet)

