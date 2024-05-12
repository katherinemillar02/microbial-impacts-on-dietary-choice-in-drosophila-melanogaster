
## Feeding Analysis 


fourone_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1.xlsx")
onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_1-4.xlsx")
fourone_onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1_1-4.xlsx")


fourone_90mm_long <- fourone_90mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

onefour_90mm_long <- onefour_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

fourone_90mm_long <- fourone_90mm_long  %>% mutate(density = "90mm")
onefour_90mm_long <- onefour_90mm_long   %>% mutate(density = "90mm")
onefour_90mm_long <- fourone_onefour_90mm_long   %>% mutate(density = "90mm")

fourone_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")
onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")
fourone_onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1_1-4.xlsx")



fourone_50mm_long <- fourone_50mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

onefour_50mm_long <- onefour_50mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

fourone_onefour_50mm_long <- fourone_onefour_50mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

fourone_onefour_50mm_long <- fourone_onefour_50mm_long %>% mutate(density = "50mm")
fourone_onefour_90mm_long <- fourone_onefour_50mm_long %>% mutate(density = "90mm")

combined_density <- rbind(fourone_onefour_50mm_long, fourone_onefour_90mm_long)


glm_poisson_density <- glm(fly_numbers ~ diet * density, family = poisson, data = combined_density)

check_zeroinflation(glm_poisson_density)

check_overdispersion(glm_poisson_density)

glm.nb_density <- glm.nb(fly_numbers ~ diet * density, data = combined_density)


check_overdispersion(glm.nb_density) ## no overdispersion

glm_mm_density <- glmmTMB(fly_numbers ~ diet * density +(1|factor(density)/plate) + (1|observation), family = poisson, data = combined_density)

summary(glm_mm_density )

drop1(glm_mm_density, check = "F")

