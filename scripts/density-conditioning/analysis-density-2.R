
## DATA: 

fourone_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1.xlsx")
onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_1-4.xlsx")

fourone_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")
onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")

fourone_90mm_long <- fourone_90mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

onefour_90mm_long <- onefour_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

fourone_50mm_long <- fourone_50mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

onefour_50mm_long <- onefour_50mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 








fourone_90mm_long <- fourone_90mm_long %>%  
  mutate(id = "4-1_90")

onefour_90mm_long <- onefour_90mm_long  %>%  
  mutate(id = "1-4_90")

fourone_50mm_long <- fourone_50mm_long %>%  
  mutate(id = "4-1_50")


onefour_50mm_long <- onefour_50mm_long  %>%  
  mutate(id = "1-4_50")








two_choice_density <- rbind( fourone_50mm_long, onefour_50mm_long, fourone_90mm_long, onefour_90mm_long )

two_choice_density_1 <- two_choice_density %>%
  mutate(density = case_when(
    str_detect(id, "50") ~ "50",
    str_detect(id, "90") ~ "90",
    ))


two_choice_density_df <- two_choice_density_1 %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%
  group_by(id, observation, plate, ratio, condition, density) %>%
  summarise(count = sum(fly_numbers)) %>%
  pivot_wider(names_from = "condition", values_from = "count")


# Ensure 'id' column is treated as character
two_choice_density_df$id <- as.character(two_choice_density_df$id)

# Define the string to search for
ninety <- "90"

# Identify rows where 'id' contains "90"
exclude_ninety <- grepl(ninety, two_choice_density_df$id)

# Filter out those rows
two_choice_density_df_fifty <- two_choice_density_df[!exclude_ninety, ]


## two choice densiry 
glmer.mm <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * density   + (1|plate) + (1|observation) , family = binomial, data = two_choice_density_df)

summary(glmer.mm)
drop1(glmer.mm, test = "Chisq")

fifty <- "50"
exclude_fifty <- grepl(fifty, two_choice_density_df$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
two_choice_density_df_ninenty <- two_choice_density_df[!exclude_fifty, ]


glmer.mm_ninety <- glmer(cbind(Conditioned, Unconditioned) ~ ratio   + (1|plate) + (1|observation) , family = binomial, data = two_choice_density_df_ninenty)

summary(glmer.mm_ninety )


## mutate a variable 


## model 
glm.bin_density <- glm(cbind(Conditioned, Unconditioned) ~ ratio * density , family = binomial, data = two_choice_density_df)

glm.bin_density_residuals <- residuals(glm.bin_density, type = "pearson")

# pearson residual check
plot(glm.bin_density_residuals ~ fitted(glm.bin_density), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")

glmer.mm_density <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * density  + (1|plate) + (1|observation) , family = binomial, data = two_choice_density_df)


drop1(glmer.mm_density, test = "Chisq")


summary(glmer.mm_density)


## Four choice density 
fourone_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1.xlsx")
onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_1-4.xlsx")

fourone_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")
onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")


glmer.mm_density <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + density  + (1|plate) + (1|observation) , family = binomial, data = two_choice_density_df)
drop1(glmer.mm_density, test = "Chisq")

summary(glmer.mm_density)

## used cbind so can't do interaction effecr