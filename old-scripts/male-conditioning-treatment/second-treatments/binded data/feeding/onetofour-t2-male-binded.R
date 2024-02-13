
# adding a variable 
onetofour_t2_bind_1 <- one_to_four_b1_median_long  %>% mutate(experiment = "one")
onetofour_t2_bind_2 <- one_to_four_b2_long %>% mutate(experiment = "two")

## binding the data
male_t2_bind_conditioned <- rbind(onetofour_t2_bind_1, onetofour_t2_bind_2)

male_t2_bind_conditioned_plot <- 
  male_t2_bind_conditioned  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 Conditioned and Unconditioned Feeding")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  
                male_t2_bind_conditioned,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

## male conditioning new plot 
male_feeding_1 <-
  ggplot(male_t2_bind_conditioned, aes(x = diet, y = fly_numbers, pattern = diet, fill = diet)) +
  geom_boxplot(aes(fill = diet))+
  scale_fill_manual(name = "Diet", values = c("lightblue","lightblue" )) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
                       color = "black",
                       pattern_fill = "white",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("stripe", "none", "stripe", "none")) +
  ylim(0,6)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_jitter(data =  male_t2_bind_conditioned,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21) +
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "")+
  theme(legend.position="none")


## 
#### Data Analysis ----
# First testing a linear model 
experiment_t2_sig <- lm(fly_numbers ~ experiment, data = male_t2_bind_conditioned)

## looking for sig across treatments for feeding number
drop1(experiment_t2_sig, test = "F")
## no sig between the two so can use?
summary(experiment_t2_sig)

## trying out a linear model
t2_feeding_lm <- lm(fly_numbers ~ diet, data = male_t2_bind_conditioned)

# Assumption Checking of the model 
performance::check_model(t2_feeding_lm, check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straightperformance::check_model(egg_analysis_combined_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(t2_feeding_lm, check = c("linearity")) # line is very much not flat.
performance::check_model(t2_feeding_lm, check = c("homogeneity"))
performance::check_model(t2_feeding_lm, check = c("outliers"))

# lm does not look the best 

# Trying a generalised linear model
t2_feeding_glm  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = male_t2_bind_conditioned)
summary(t2_feeding_glm) # underdispersed 

t2_feeding_glm_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = male_t2_bind_conditioned)


#### Assumption Checking ####
performance::check_model(t2_feeding_glm_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(t2_feeding_glm_2, check = c("homogeneity")) # not flat but better
performance::check_model(t2_feeding_glm_2, check = c("outliers"))

# lm may be bette4
# summary function, shows t test
summary(t2_feeding_lm)

# using anova 
anova(t2_feeding_lm)

# emmeans for tukey analysis 
emmeans::emmeans(t2_feeding_lm, pairwise ~ diet)

# no sig difference 
