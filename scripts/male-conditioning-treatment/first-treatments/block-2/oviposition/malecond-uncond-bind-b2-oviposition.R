#### A script for testing "Conditioned" and "Unconditioned" in a model together

## mutating a variable
egg_analysis_con_2 <- conditioned_b2_eggs_long %>% mutate(treatment = "conditioned")
egg_analysis_uncon_2 <- unconditioned_b2_eggs_long %>% mutate(treatment = "unconditioned")

## creating an R dataset to be used for analysis 
egg_analysis_2 <- rbind(egg_analysis_con_2, egg_analysis_uncon_2)

## Data visualising for combined egg data 
egg_analysis_plot_2 <- egg_analysis_2  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Across Conditioned and Unconditioned Plates - B2")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data =  egg_analysis_2, 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




#### Data Analysis ----
# First testing a linear model 
egg_analysis_combined_lm_2 <- lm(egg_numbers ~  diet : treatment, data = egg_analysis_2)

egg_analysis_sig <- lm(egg_numbers ~ treatment, data = egg_analysis_2)

emmeans::emmeans(egg_analysis_sig, pairwise ~ treatment)

drop1(egg_analysis_sig , test = "F")

# Assumption Checking of the model 
performance::check_model(egg_analysis_combined_lm_2, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straightperformance::check_model(egg_analysis_combined_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(egg_analysis_combined_lm_2, check = c("linearity")) # line is very much not flat.
performance::check_model(egg_analysis_combined_lm_2, check = c("outliers"))



# Trying a generalised linear model
egg_analysis_combined_glm_2  <- glm(egg_numbers ~  diet, family = poisson(link = "log"), data = egg_analysis_2)
summary(egg_analysis_combined_glm) # overdispersed so using quaispoisson

egg_analysis_combined_glm_2_2 <- glm(egg_numbers ~  diet * treatment, family = quasipoisson(link = "log"), data = egg_analysis)



performance::check_model(egg_analysis_combined_glm_2_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(egg_analysis_combined_glm_2_2, check = c("homogeneity")) # not flat but better
performance::check_model(egg_analysis_combined_glm_2_2, check = c("outliers"))

# homogeneirty is not great for glm but qq looks a lot better 

# summary function, shows t test
summary(egg_analysis_combined_glm_2_2)

# using anova 
anova(egg_analysis_combined_glm_2)

# emmeans for tukey analysis 
emmeans::emmeans(egg_analysis_combined_glm_2_2, pairwise ~ diet)


