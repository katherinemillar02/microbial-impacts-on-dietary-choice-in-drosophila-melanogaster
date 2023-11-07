
## Combining the data
unconditioned_conditioned_combined <- rbind(unconditioned_and_conditioned_rep1, conditioned_and_unconditioned_diets_2)


## Making the data long 
unconditioned_conditioned_long <- unconditioned_conditioned_combined %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Summarising the data; mean, sd, se
unconditioned_conditioned_summary  <- unconditioned_conditioned_long %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))



## Visualising the data
# Creating a boxplot with the data that has been summarised
unconditioned_conditioned_plot <- unconditioned_conditioned_summary   %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#E57157",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#E57157",
                width = 0.2)+
  geom_jitter(data = unconditioned_conditioned_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.5)+
  labs(x = "Diet Condition",
       y = "Mean +/- S.E. Number of flies per patch",
       title = "Conditioned and Unconditioned Diet Patches Combined")+
  theme_classic() 



#### Data Analysis 

# First testing a linear model 
conditioned_unconditioned_combined_lm <- lm(fly_numbers ~  diet, data = unconditioned_conditioned_long)

# Assumption Checking of the model 
performance::check_model(conditioned_unconditioned_combined_lm , check = c("qq")) # points are a quite dispersed 
performance::check_model(conditioned_unconditioned_combined_lm , check = c("homogeneity")) # not great- not flat
performance::check_model(conditioned_unconditioned_combined_lm , check = c("linearity")) # looks okay
performance::check_model(conditioned_unconditioned_combined_lm , check = c("outliers"))


# Trying a generalised linear model
conditioned_unconditioned_combined_glm01 <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = unconditioned_conditioned_long)
summary(conditioned_unconditioned_combined_glm01) # overdispersion with poisson

# glm with quasipoisson as there is overdispersin
conditioned_unconditioned_combined_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_conditioned_long)

# assumption checking
performance::check_model(conditioned_unconditioned_combined_glm , check = c("qq")) # looks okay - few extra points at the end
performance::check_model(conditioned_unconditioned_combined_glm , check = c("homogeneity")) # flatter than lm
performance::check_model(conditioned_unconditioned_combined_glm , check = c("outliers")) # same as lm

# glm is better by quite a bit

# summary function, shows t test
summary(conditioned_unconditioned_combined_glm)

# using anova 
anova(conditioned_unconditioned_combined_glm)

# emmeans for tukey
emmeans::emmeans(conditioned_unconditioned_combined_glm, pairwise ~ diet)
