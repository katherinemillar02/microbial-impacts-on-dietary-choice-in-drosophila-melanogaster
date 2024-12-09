#### Male #### 


#### Reading, binding, cleaning data 📖 ####
# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx")
# Mutating a block variable
fourone_onefour_male_oviposition_b1 <- fourone_onefour_male_oviposition_b1 %>% mutate(block = "one")
fourone_onefour_male_oviposition_b2 <- fourone_onefour_male_oviposition_b2%>% mutate(block = "two")
# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the data different dataframes
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Splitting "diet" up
combined_ovi_m_split <- combined_ovi_m %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")





#### Virgin Female ####
#### Reading, binding, cleaning data 📖 ####

## Reading in the different data-sets
fourone_onefour_oviposition_virgin_b2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")
fourone_onefour_oviposition_virgin_b3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")
fourone_onefour_oviposition_virgin_b4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")
## Mutating a block variable to the data-sets
fourone_onefour_oviposition_virgin_b2 <- fourone_onefour_oviposition_virgin_b2 %>% mutate(block = "two")
fourone_onefour_oviposition_virgin_b3 <- fourone_onefour_oviposition_virgin_b3 %>% mutate(block = "three")
fourone_onefour_oviposition_virgin_b4 <- fourone_onefour_oviposition_virgin_b4 %>% mutate(block = "four")

## Binding the different data-sets
fourone_onefour_oviposition_virgin <- rbind(fourone_onefour_oviposition_virgin_b2, fourone_onefour_oviposition_virgin_b3, fourone_onefour_oviposition_virgin_b4)


## adding some data names
combined_ovi_v <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Using separate to split the diet into rartio and condition 
combined_ovi_v_split <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")






#### OvoD1 Female #### 


#### Reading, binding, cleaning data 📖 ####
## Reading the data in 
combined_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
combined_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

# Mutating a block variable 
combined_ovod1_b1 <- combined_ovod1_b1  %>% mutate(block = "one")
combined_ovod1_b2 <- combined_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
combined_ovod1 <- rbind(combined_ovod1_b1, combined_ovod1_b2)

# Making the data long
combined_of_egg  <- combined_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Splitting up diet variable 
combined_of_egg_split <- combined_of_egg %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")



