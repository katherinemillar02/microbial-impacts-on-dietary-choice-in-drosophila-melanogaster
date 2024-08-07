################################# --
####### TREATMENT: MALE CONDITIONING ♂️ #######
################################# --


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



#### DIET = CONDITION + RATIO 🍟💊 ####

## Splitting "diet" up
combined_ovi_m_split <- combined_ovi_m %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")



#### The chosen model: Negative Binomial GLM ####

####  Changing what the intercept is. 
combined_ovi_m_split$ratio <- as.factor(combined_ovi_m_split$ratio)
combined_ovi_m_split$ratio <- relevel(combined_ovi_m_split$ratio, ref = "4:1")

# Testing a 3-way interaction, using *, will also test 2-way interactions
comb_m_egg_glm.nb <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_ovi_m_split)


## This will show results or the 3-way interaction. 
drop1(comb_m_egg_glm.nb, test = "F")
## No 3-way interaction found. 



#### Testing for a 2-way interaction
comb_m_egg_glm.nb.2 <- glm.nb(egg_numbers ~
                                
                                + ratio * condition 
                              + ratio * block
                              + condition * block,
                              
                              data =  combined_ovi_m_split)

## This will show any significance of two-way interactions.
drop1(comb_m_egg_glm.nb.2, test = "F")
## no two way  interactions 


comb_m_egg_glm.nb.3 <- glm.nb(egg_numbers ~
                                ratio + condition + block, data =  combined_ovi_m_split)

#### DATA ANALYSIS 📊 ####
summary(comb_m_egg_glm.nb.3)




exp(confint(comb_m_egg_glm.nb.3))

tab_model(comb_m_egg_glm.nb.3)







############################## --
#### TREATMENT: VIRGIN CONDITIONING 👰 ####
############################# --

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



#### DIET = CONDITION + RATIO 🍟💊####


## Using separate to split the diet into rartio and condition 
combined_ovi_v_split <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


#### The chosen model: Negative Binomial GLM ####

#### Changing the intercept value 

## Changing the ratio intercept to 4:1. 
combined_ovi_v_split$ratio <- as.factor(combined_ovi_v_split$ratio)
combined_ovi_v_split$ratio <- relevel(combined_ovi_v_split$ratio, ref = "4:1")

## Changing the block intercept to two. 
combined_ovi_v_split$block <- as.factor(combined_ovi_v_split$block)
combined_ovi_v_split$block <- relevel(combined_ovi_v_split$block, ref = "two")


## Testing for a 3-way interaction effect 
glm.nb_v_comb_egg.2 <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_ovi_v_split)


## Will show any significance in the 3-way interaction effect
drop1(glm.nb_v_comb_egg.2, test = "F")


## Testing for 2-way interaction effects 
glm.nb_v_comb_egg.3 <- glm.nb(egg_numbers ~
                                
                                ratio * condition +
                                block * condition + 
                                ratio * block , 
                              
                              data =  combined_ovi_v_split)


## Will show any significance in the 2-way interaction effects
drop1(glm.nb_v_comb_egg.3, test = "F")
## Interaction effect between condition and block 


# The final model
glm.nb_v_comb_egg.4 <- glm.nb(egg_numbers ~
                                condition * block + ratio , data =  combined_ovi_v_split)

# Using drop1 to confirm the appropriate interaction effects 
drop1(glm.nb_v_comb_egg.4, test = "F")


#### DATA ANALYSIS 📊 ####
summary(glm.nb_v_comb_egg.4)

exp(confint(glm.nb_v_comb_egg.4))

tab_model(glm.nb_v_comb_egg.4)










#### TREATMENT = OVOD1 FEMALE 🥚❌ ####





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


#### DIET = RATIO + CONDITION #### 

#### Splitting up diet in the data 

combined_of_egg_split <- combined_of_egg %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


#### Changing what the intercept is... 

## Changing "ratio" to 4:1 
combined_of_egg_split$ratio <- as.factor(combined_of_egg_split$ratio)
combined_of_egg_split$ratio <- relevel(combined_of_egg_split$ratio, ref = "4:1")

## Changing block to "two" (as one will not be used for oviposition data)
combined_of_egg_split$block <- as.factor(combined_of_egg_split$block)
combined_of_egg_split$block <- relevel(combined_of_egg_split$block, ref = "one")


## Testing for a 3-way interaction effect, along with two way interaction effects still in the model. 
glm.nb_of_comb_egg.2 <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_of_egg_split)


#### Using drop1 to look for the sig interaction effects. 
drop1(glm.nb_of_comb_egg.2, test = "F")
## No 3-way interaction effect found. 


## looking for two-way interaction effects.
glm.nb_of_comb_egg.3 <- glm.nb(egg_numbers ~
                                
                                ratio * condition +
                                block * condition + 
                                ratio * block ,
                              
                              data =  combined_of_egg_split)

## Will directly show significant 2-way interactions
drop1(glm.nb_of_comb_egg.3, test = "F")
## Iteraction effect between condition and block 


## The final model
glm.nb_of_comb_egg.4 <- glm.nb(egg_numbers ~
                                condition * block 
                               + ratio * block
                               , data =  combined_of_egg_split)

## Using drop1 to confirm the appropriate interaction effects. 
drop1(glm.nb_of_comb_egg.4, test = "F")


#### DATA ANALYSIS 📊 ####
summary(glm.nb_of_comb_egg.4)



exp(confint(glm.nb_of_comb_egg.4))

emmeans::emmeans(glm.nb_of_comb_egg.4, pairwise ~ condition + ratio)

emmeans::emmeans(glm.nb_of_comb_egg.4, ~ condition + ratio, type = "respomse")

emmeans::emmeans(glm.nb_of_comb_egg.4, ~ condition + ratio, type = "response")


tab_model(glm.nb_of_comb_egg.4)

