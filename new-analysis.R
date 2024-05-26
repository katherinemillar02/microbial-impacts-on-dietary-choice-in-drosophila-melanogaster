

#### NEW RESULTS 


## OvoD1 Feeding 1:4 
glm_test_1 <- glmer(fly_numbers ~ diet + block + (1|plate) , family = binomial , data = df_ovod1_2)


emmeans::emmeans(glm_test_1, pairwise ~ diet)

## Virgin Oviposition 4:1 
glm_test <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = df_virgin_oviposition)

emmeans::emmeans(glm_test, pairwise ~ diet)

## OvoD1 Feeding 4:1
glm_test_0 <- glmmTMB(fly_numbers ~ diet + block + (1|plate) , family = poisson, data = df_ovod1)




emmeans::emmeans(glm_test_0, pairwise ~ diet)



four_one <- "4-1"
exclude_onefour <- grepl(one_four, df_ovod1$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
df_ovod1_2 <- df_ovod1[!exclude_onefour, ]

