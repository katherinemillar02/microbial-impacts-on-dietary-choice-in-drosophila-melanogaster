# adding an experiment block variable 
onefour_t2_oviposition_1 <- one_to_four_oviposition_b1_long %>% mutate(experiment = "one")
onefour_t2_oviposition_2 <- onefour_oviposition_b2_long %>% mutate(experiment = "two")

# binding the data 
onefour_t2_oviposition <- rbind(onefour_t2_oviposition_1, onefour_t2_oviposition_2) 

## Median boxplot --
onefour_t2_oviposition_plot <- onefour_t2_oviposition  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Diet Condition",
       y = "Number of eggs per diet patch", 
       title = "1:4 Conditioned and Unconditioned Oviposition")+
  theme(legend.position="none")+ 
  ylim(-0.01,150)+
  geom_jitter(data =  onefour_t2_oviposition,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
## Statistical analysis ----
# First testing a linear model 
onetofour_oviposition_lm_sig <- lm(egg_numbers ~  experiment, data = onefour_t2_oviposition)

drop1(onetofour_oviposition_lm_sig, test = "F")


## making a linear model without experiment 
onetofour_oviposition_lm_both <- lm(egg_numbers ~  diet, data = onefour_t2_oviposition)

# Assumption Checking of the model 
performance::check_model(onetofour_oviposition_lm_both, check = c("qq")) # I think qqplot looks okay, few dots dispersed.
performance::check_model(onetofour_oviposition_lm_both , check = c("homogeneity")) # line is not flat.
performance::check_model(onetofour_oviposition_lm_both, check = c("linearity")) # line is very flat.
performance::check_model(onetofour_oviposition_lm_both, check = c("outliers"))

## model looks somewhat okay

# Trying a generalised linear model
onetofour_oviposition_both_glm_1  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = onefour_t2_oviposition)
summary(onetofour_oviposition_both_glm_1 ) # overdispersed 


onetofour_oviposition_both_glm_2  <- glm(egg_numbers ~  diet, family = quasipoisson(link = "log"), data = onefour_t2_oviposition)


performance::check_model(onetofour_oviposition_both_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(onetofour_oviposition_both_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(onetofour_oviposition_both_glm_2, check = c("outliers"))

# glm with quasipoisson looks slighly better

# summary function, shows t test
summary(onetofour_oviposition_both_glm_2)

# using anova 
anova(onetofour_oviposition_both_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(onetofour_oviposition_both_glm_2, pairwise ~ diet)
# unconditioned diets significantly preferred 
