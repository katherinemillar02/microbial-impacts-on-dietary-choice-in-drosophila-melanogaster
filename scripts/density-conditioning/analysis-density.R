#### 4-1 AND 1-4 ANALYSIS 


## 50 mm
fourone_onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1_1-4.xlsx")

fourone_onefour_50mm_long <- fourone_onefour_50mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

fourone_onefour_50mm_long <- fourone_onefour_50mm_long %>%
  mutate(density = "50mm")


glm_mm_density_50 <- glmmTMB(fly_numbers ~ diet + (1|plate) + (1|observation), family = poisson, data = fourone_onefour_50mm_long)

summary(glm_mm_density_50)

emmeans::emmeans(glm_mm_density_50, pairwise ~ diet)


## 90 mm
fourone_onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1_1-4.xlsx")

fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

fourone_onefour_90mm_long <- fourone_onefour_90mm_long %>%
  mutate(density = "90mm")

glm_mm_density_90 <- glmmTMB(fly_numbers ~ diet + (1|plate) + (1|observation), family = poisson, data = fourone_onefour_90mm_long)

summary(glm_mm_density_90)

emmeans::emmeans(glm_mm_density_90, pairwise ~ diet)


combined_assays <- rbind(fourone_onefour_50mm_long, fourone_onefour_90mm_long)

glm_mm_density_combined <- glmmTMB(fly_numbers ~ ratio + treatment * density + (1|plate) + (1|observation), family = poisson, data = combined_assays)

drop1(glm_mm_density_combined, test = "Chisq") ## no interaction effect


glm_mm_density_combined <- glmmTMB(fly_numbers ~ ratio + treatment + density + (1|plate) + (1|observation), family = poisson, data = combined_assays)



## separating diet 

combined_assays <- combined_assays %>% 
  separate(diet, into = c("ratio", "treatment"), sep = " ")
