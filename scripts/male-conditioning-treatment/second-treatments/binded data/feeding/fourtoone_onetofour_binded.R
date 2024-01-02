#### NEED TO FIX THIS SCRIPT SO MEDIAN IS INCLUDED IN BLOCK 1 


# Four to One and One to Four 

# male binded 
# adding a variable 
fourtoone_onetofour_t2_bind_1 <- onetofour_fourtoone_b1_median_long   %>% mutate(experiment = "one")
fourtoone_onetofour_t2_bind_2 <- fourtoone_onetofour_b2_long  %>% mutate(experiment = "two")

## binding the data
male_t2_bind_conditioned_4to1_1to4 <- rbind(fourtoone_onetofour_t2_bind_1 , fourtoone_onetofour_t2_bind_2)

male_t2_bind_conditioned_4to1_1to4_plot  <- 
  male_t2_bind_conditioned_4to1_1to4   %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 and 1:4")+
  theme(legend.position="none")+ 
  ylim(-0.01,4)+
  geom_jitter(data =  
                male_t2_bind_conditioned_4to1_1to4,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


## 
#### Data Analysis ----
# First testing a linear model 
experiment_t2_sig_fourone_onetofour <- lm(fly_numbers ~ experiment, data = male_t2_bind_conditioned_4to1_1to4)

## looking for sig across treatments for feeding number
drop1(experiment_t2_sig_fourone_onetofour, test = "F") ## really significant? issue? 



## trying out a linear model
t2_feeding_lm_fourone_onefour <- lm(fly_numbers ~ diet, data = male_t2_bind_conditioned_4to1_1to4)

# Assumption Checking of the model 
performance::check_model(t2_feeding_lm_fourone_onefour , check = c("qq")) # I think qqplot looks okay, few dots dispersed. Line is straightperformance::check_model(egg_analysis_combined_lm, check = c("homogeneity")) # line is not flat.
performance::check_model(t2_feeding_lm_fourone_onefour , check = c("linearity")) # line is very much not flat.

performance::check_model(t2_feeding_lm_fourone_onefour , check = c("outliers"))

# doesn't look awful

# Trying a generalised linear model
t2_feeding_glm_fourone_onefour  <- glm(fly_numbers ~  diet, family = poisson(link = "log"), data = male_t2_bind_conditioned_4to1_1to4)
summary(t2_feeding_lm_fourone_onefour ) # overdispersed

t2_feeding_glm_fourone_onefour_2  <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = male_t2_bind_conditioned_4to1_1to4)


#### Assumption Checking ####
performance::check_model(t2_feeding_glm_fourone_onefour_2, check = c("qq")) # dots seem to match to line better than lm
performance::check_model(t2_feeding_glm_fourone_onefour_2 , check = c("homogeneity")) # not flat but better
performance::check_model(t2_feeding_glm_fourone_onefour_2 , check = c("outliers"))

# glm may be better 

# summary function, shows t test
summary(t2_feeding_glm_fourone_onefour_2)

# using anova 
anova(t2_feeding_glm_fourone_onefour_2)

# emmeans for tukey analysis 
emmeans::emmeans(t2_feeding_lm_fourone_onefour, pairwise ~ diet)

# strong significant difference 