## Packages ## 📦📦📦📦
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(pscl)
library(MASS)
############### 📦📦📦📦








## OVIPOSITION ##

####################### --
#### VIRGIN FEMALE ####
####################### --

## Creating a path to Virgin Conditioning Oviposition data
pathvirginoviposition <- "data/female_conditioning/virgin"

# reading in oviposition data for virgin 
read_raw_virgin_oviposition <-function(path = pathvirginoviposition, pattern_to_exclude = "4-1_1-4_oviposition"){
  list_of_files <- list.files(path = pathvirginoviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}



# using the function to find the path 
read_raw_virgin_oviposition(pathvirginoviposition)


### making a dataset without block 1




## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin_oviposition <- pathvirginoviposition %>% 
  map_df(~read_raw_virgin_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently

# Use grepl to identify rows with "1-4" pattern in any column
pattern <- "b1"
exclude_rows <- grepl(pattern, df_virgin_oviposition$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
df_virgin_oviposition <- df_virgin_oviposition[!exclude_rows, ]


## Mutating a variable for block 
df_virgin_oviposition <- df_virgin_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b2") ~ "two",
    str_detect(id, "b3") ~ "three",
    str_detect(id, "b4") ~ "four",
  ))


# uses what was generated with "df"
df2_virgin_oviposition <- df_virgin_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_virgin_oviposition # does it recognise condition from the long data









#### OVOD1 FEMALE ----
pathovod1oviposition <- "data/female_conditioning/ovod1"

## This creates  function

######################################################################################################################## --
read_raw_ovod1_oviposition <-function(path = pathovod1oviposition, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathovod1oviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
######################################################################################################################## --





## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1_oviposition(pathovod1oviposition) 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1_oviposition <- pathovod1oviposition %>% 
  map_df(~read_raw_ovod1_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently


## Mutating a variable for block
df_ovod1_oviposition <- df_ovod1_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    
  ))


# uses what was generated with "df"
df2_ovod1_oviposition <- df_ovod1_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_ovod1_oviposition # does it recognise condition from the long data? 








################ --
#### MALE ####
################ --

## Creating a path to get to the data
pathmaleoviposition <- "data/male_conditioning/treatment_2"


################################################################################################################ --
read_raw_male_oviposition <-function(path = pathmaleoviposition, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathmaleoviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
################################################################################################################--


## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male_oviposition(read_raw_male_oviposition)



## creating an actual data set that will read the paths
# first data frame - purr package 
df_male_oviposition <- pathmaleoviposition %>% 
  map_df(~read_raw_male_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently



## Mutating a variable for block 
df_male_oviposition <- df_male_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "t2b1") ~ "one",
    str_detect(id, "t2b2") ~ "two"
  ))




# uses what was generated with "df"
df2_male_oviposition <- df_male_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_male_oviposition # does it recognise condition from the long data? 




## This is the data frame for male oviposition, for just 4:1 and 1:4 - and includes a variable for block one and block two


############################################################ --
 
  





######################################################################################################################## --
                                               #### DATA ANALYSIS ####
######################################################################################################################## --


########################### --
#### MALE OVIPOSITION #####
########################### --


## Model 1 
 # glm binial model
binom_m_egg <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_male_oviposition)

## Assumption checking 

## easystats

## qq 
performance::check_model(binom_m_egg, check = c("qq")) ## the qq for this model looks pretty good

## different qq
residuals_m <- residuals(binom_m_egg, type = "pearson")
qnorm(residuals_m)
qqnorm(residuals_m)
qqline(residuals_m) ## there are still some dispersed lines 

## Homogeneity 
performance::check_model(binom_m_egg, check = c("homogeneity")) 
   ## there is still some dispersion between variables
   ## not everything completley lines up 

## Another way to look at qq and homogeneity 
plot(binom_m_egg) ## similar results, points aren't completley together 

## Using DHARMa
testDispersion(binom_m_egg) ## This is quite bad? 
  ## seems to be quite overdispersed 

check_overdispersion(binom_m_egg) ## model is quite overdispersed 

## Looking at qq and homogenity again
simulation_Output <- simulateResiduals(fittedModel = binom_m_egg, plot = F)
plot(simulation_Output) 
    ## This shows even worse results, why is this? 




## Model 2
## Trying a new glm with random effects 
glmer.mm_m_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_male_oviposition)

## Assumption Checking 

## qq checking 
performance::check_model(glmer.mm_m_egg, check = c("qq")) 
   ## This qqplot seems to go more off at the end 
   ## but dots are sort of on the line 


## different qq
residuals_m_mixed <- residuals(glmer.mm_m_egg, type = "pearson")
qnorm(residuals_m_mixed)
qqnorm(residuals_m_mixed)
qqline(residuals_m_mixed)
  ## There is some weird dispersion, goes off at the end 


## Homogeneity assumption checking 
performance::check_model(glmer.mm_m_egg, check = c("homogeneity")) 
  ## This is all a lot less lined up than the binomial model 
  ## assumptions do not look greatly met here 

## other ways to look as assumptions 
plot(glmer.mm_m_egg) ## This doesn't generate everything? 

## Using DHARMa 
## looking for overdispersion 
testDispersion(glmer.mm_m_egg) ## This looks pretty good

# easystats dispersion check 
check_overdispersion(glmer.mm_m_egg) ## still says overdispersion is detected 

## More ways to look at qq and homogeneity
simulation_Output <- simulateResiduals(fittedModel = glmer.mm_m_egg, plot = F)
plot(simulation_Output) 
  ## Not much really lines up well in either of these 

## Comparing the two models 
AIC(binom_m_egg, glmer.mm_m_egg)
  ## It says the mixed model has a much lower AIC,
  ## even though I think the assumptions look a bit worse

## Choosing the mixed model for now: 
glmer.mm_m_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate/block)  , family = binomial, data = df2_male_oviposition)

## Looking for the signifince in block
drop1(glmer.mm_m_egg, test = "Chisq") ## says block is quite significant, keeping it in the model

## Using model for now, looking at results: 
summary(glmer.mm_m_egg) ## says block is significant 











################################## --
### OvoD1 FEMALE OVIPOSITION ####
################################## --


## Model 1 

# Binomial model 
binom_od1_egg <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 

# easystats does not work at the moment 
performance::check_model(binom_od1_egg , check = c("qq")) ## looks okay

## comparing this with a different qqplot to understand qq plots 
## Generating residuals 
residuals <- residuals(binom_od1_egg, type = "pearson")
qnorm(residuals)
## Will generate a qq 
qqnorm(residuals) ## qq looks bad 
qqline(residuals, col = "green") ## putting a line there to understand it better

## Understanding the qqplot:  
 ## There are points in the data below where the data should fall?
 ## should be straight across if in ascending order? 

## Homogeneity
# easystats plot
performance::check_model(binom_od1_egg , check = c("homogeneity")) ## the dots are not really lines up 
# first one should be homogeneity
plot(binom_od1_egg) ## lines look sort of lined up but not the best
   
## Understanding homogeneity: 
 # Assumptions: the level of variance for a particular variable is constant across the sample 
 # Groups of data: the variance of the outcome variable should be the same in each group
 # So, I think the dots should sort of be in line with eachother? 
 # This doesn't really happen, so there isn't a great assumption of homogeneity? 
   


#### MORE ASSUMPTION CHECKS
# DHARMa 
testDispersion(binom_od1_egg) # looks pretty poor
## easystats overdispersion check
check_overdispersion(binom_od1_egg) ## overdispersion 

## Understanding overdispersion 
  ## There is quite a lot of overdispersion
  ## This means that the variance of the response variable (fly numbers) is greater than what is
  ## expected by the model

## Another qqplot 
simulationOutput <- simulateResiduals(fittedModel = binom_od1_egg, plot = T)
 ## is predicted the same as fitted? 
 ## is the second plot homogeneity assumptions? 

## understanding the qqplot shows the points are off the usual data and the second plot shows there is not 
## really homogeneity of variance 

## Overall, this model is really not the best...




## Model 2 
## Trying a new model 

## glm with random effects
glmer.ovod1_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 
# easystats 
performance::check_model(glmer.ovod1_f_egg, check = c("qq"))
  ## this qqplot looks pretty good, this means the data falls through quite a nice distribution
  ## data is expected to fall the way the data falls 

## different way of doing a qqplot
## Generating residuals 
residuals <- residuals(glmer.ovod1_f_egg, type = "pearson")
qnorm(residuals)
## Will generate a qq 
qqnorm(residuals) ## qq looks a lot better 
qqline(residuals, col = "green") ## again, points seem to fall along the line 


## Homogeneity check 
performance::check_model(glmer.ovod1_f_egg, check = c("homogeneity")) 
  ## There is still not great homogeneity assumptions 
  ## This means the level of variance is not really constant still 



## MORE ASSUMPTIONS CHECKS 
# DHARMa 

## Looking for overdispersion
testDispersion(glmer.ovod1_f_egg) # looks a lot better I think 

# easystats check for overdispersion 
check_overdispersion(glmer.ovod1_f_egg) ## there is still overdispersion detected 


## More DHARMa assumption checks 
simulationOutput <- simulateResiduals(fittedModel = glmer.ovod1_f_egg, plot = T)
  ## DHARMa assumption checks show the model to look a lot better 

## Doing an AIC check
AIC(binom_od1_egg, glmer.ovod1_f_egg)
   ## The model with random effects (Model 2) is a lot better, as expected from the other assumptions... 

## Using "Model 2" for now as I do not really know what else to do 
 
## Model choice: 
glmer.ovod1_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate/block)  , family = binomial, data = df2_ovod1_oviposition)

summary(glmer.ovod1_f_egg)

## Looking for significance in block
drop1(glmer.ovod1_f_egg, test = "Chisq") ## block is  significant with ratio

## Looking at results, keeping block in 

## Basic summary
summary(glmer.ovod1_f_egg_2) 

  ## What I am confused about here: 
# If I am interpreting the results right, why is there no 1:4 conditioned/unconditioned block 2 results?? 

glmer.ovod1_f_egg_2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  + (1|plate/block)  , family = binomial, data = df2_ovod1_oviposition)









############################## --
## VIRGIN Female Oviposition #### 
############################## --

## Model 1 

# Binomial model 
binom_virgin_egg <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin_oviposition)

# Assumption checking 

## qq checks 
performance::check_model(binom_virgin_egg, check = c("qq"))
  ## the points do not line up greatly, but it is not the worst 
  ## dispersion at the beginning and the end 

## another qq
residuals_v_binom <- residuals(binom_virgin_egg, type = "pearson")
qnorm(residuals_v_binom)
## Will generate a qq 
qqnorm(residuals_v_binom) 
qqline(residuals_v_binom, col = "green") 
   ## points sort of fall along the line, but again - not at the beginning and end 


## Testing homogeneity assumptions 
performance::check_model(binom_virgin_egg, check = c("homogeneity"))
   ## there seems to be quite a lot of variation 

## more ways to test assumptions
plot(binom_virgin_egg)
## the way that things line up isn't actually awful 


# DHARMa assumption checks 

## Looking for overdispersion 
testDispersion(binom_virgin_egg) 
  ## I think this shows to be quite a lot of overdsispersion 

## easystats overdispersion checker 
check_overdispersion(binom_virgin_egg)
  ## there is some overdispersion, quite a bit? 


## Doing more DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = binom_virgin_egg, plot = T)
    ## Things do not line up too great




## Model 2
## a glm mixed model
glmer.virgin_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_virgin_oviposition)

# Assumption checking 

# easystats
performance::check_model(glmer.virgin_f_egg, check = c("qq"))
   ## this seems to line up better than the binomial model 
   ## but there are still some dispersed points at the beginning and the end

## another qq
residuals_v_mixed <- residuals(glmer.virgin_f_egg, type = "pearson")
qnorm(residuals_v_mixed)
## Will generate a qq 
qqnorm(residuals_v_mixed) 
qqline(residuals_v_mixed , col = "green") 
   ## again, points sort of fall along the line but there is some dispersion at the beginning and end 


## Homogeneity assumption checks 
performance::check_model(glmer.virgin_f_egg, check = c("homogeneity"))
  ## there seems to be lots of variation in the points 

# Trying more assumption checks
plot(glmer.virgin_f_egg)
   ## Just shows lots of dispersion really


# DHARMa assumption checks
testDispersion(glmer.virgin_f_egg) # quite overdispersed

## easystats dispersion check
check_overdispersion(glmer.virgin_f_egg) 
  ## There is overdispersion check 

## More general assumption checks using DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmer.virgin_f_egg, plot = T)
 # Some points fall together, but some points are still quite dispersed 


## Comparing models
AIC(binom_virgin_egg, glmer.virgin_f_egg)
   ## the mixed model has a slightly lower AIC 

## Using the mixed model for now 
glmer.virgin_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate/block)  , family = binomial, data = df2_virgin_oviposition)

glmer.virgin_f_egg_2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  + (1|plate/block)  , family = binomial, data = df2_virgin_oviposition)

summary(glmer.virgin_f_egg_2)

## Looking for significance in block
drop1(glmer.virgin_f_egg, test = "Chisq") ## block is significant

## Looking at results, keeping block in 
summary(glmer.virgin_f_egg)
## block 3 sig with block 1
## block 2 not sig with block 1 

glmer.virgin_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate/block)  , family = binomial, data = df2_virgin_oviposition)










################################################ DATA ANNALYSIS PART 2 ###################################
######################################## The Combined 4:1 and 1:4 Assays ###################################
################################################################################################### ---




################################### --
####### OvoD1 Conditioning ####### 
################################### ---


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


# Model 1 
combined_od1_glm.p <- glm(egg_numbers ~ diet * block, family = poisson, combined_of_egg)

## Model assumption checking 

## easystats currently doesn't work 

## DHARMa
testDispersion(combined_od1_glm.p) ## Does this mean very overdispersed? 

simulationOutput <- simulateResiduals(fittedModel = combined_od1_glm.p, plot = T) ## ? bad 


## Checking model dispersion values
summary(combined_od1_glm.p) ## Very overdispersed 

# Checking for zero inflation
check_zeroinflation(combined_od1_glm.p) ## No zero inflation?? 

# Looking at qq
## Generating residuals 
residuals_p <- residuals(combined_od1_glm.p, type = "pearson")
qnorm(residuals_p)
## Will generate a qq 
qqnorm(residuals_p) ## looks pretty low 

# But model is still very overdispersed 
# Because of this, first trying a quassipoisson 

## quasipoisson model
combined_od1_glm_egg.qp <- glm(egg_numbers ~ diet * block, family = quasipoisson, combined_of_egg)


## can't do DHARma checks on a quasipoisson model? 
# Looking at qq
## Generating residuals 
residuals_qp <- residuals(combined_od1_glm.qp, type = "pearson")
qnorm(residuals_qp)
## Will generate a qq 
qqnorm(residuals_qp) ## looks the same as the poisson qq ?? 


## Don't really know how to interpret the quasipoisson model, doing a mixed model 
# trying a mixed GLM 
combined_glm_mm_od1_egg <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = combined_of_egg)

# Looking at qq
## Generating residuals 
residuals_glm_mm <- residuals(combined_glm_mm_od1_egg , type = "pearson")
qnorm(residuals_glm_mm )
## Will generate a qq 
qqnorm(residuals_glm_mm ) ## looks pretty low -- all qq looks the same?? 



## DHARMa checks 
testDispersion(combined_glm_mm_od1_egg) ## looks a lot better, still odd 

simulation0utput <- simulateResiduals(fittedModel = combined_glm_mm_od1_egg, plot = T) ## Looking slightly better 



## Comparing the AIC of the models 
AIC(combined_od1_glm.p, combined_od1_glm.qp, combined_glm_mm_od1_egg)
 ## Mixed model has the lowest AIC 

## Using this model for now 
combined_glm_mm_od1_egg <- glmmTMB(egg_numbers ~ diet * block + (1|plate/block) , family = poisson, data = combined_of_egg)


## Testing the significance of block
drop1(combined_glm_mm_od1_egg , test = "Chisq") # block is very signficiant 

summary(combined_glm_mm_od1_egg)
   ## everything is very significant - dodgy data? 

combined_glm_mm_od1_egg <- glmmTMB(egg_numbers ~ diet  + (1|plate/block) , family = poisson, data = combined_of_egg)


emmeans::emmeans(combined_glm_mm_od1_egg, pairwise ~ diet)






################################# --
####### Male Conditioning #######
################################# --

# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b2_oviposition.xlsx")
# Mutating a block variable
fourone_onefour_male_oviposition_b1 <- fourone_onefour_male_oviposition_b1 %>% mutate(block = "one")
fourone_onefour_male_oviposition_b2 <- fourone_onefour_male_oviposition_b2%>% mutate(block = "two")
# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the data different dataframes
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Model 1 
## Beginning with a glm with poisson
comb_m_egg_glm.p <- glm(egg_numbers ~ diet * block, family = poisson,  combined_ovi_m )

## Assumption checking 

## DHARMa
testDispersion(comb_m_egg_glm.p) ## Really really overdispersed 

simulationOutput <- simulateResiduals(fittedModel = comb_m_egg_glm.p, plot = T) ## not great


## Looking at a qq plot 
## Generating residuals 
residuals_glm_p_m <- residuals(combined_glm_mm_od1_egg , type = "pearson")
qnorm(residuals_glm_p_m)
## Will generate a qq 
qqnorm(residuals_glm_p_m) ## ?? 


## Checking for overdispersion
summary(comb_m_egg_glm.p) ## very overdispersed by the looks of it 

## Checking for zeroinflation 
check_zeroinflation(comb_m_egg_glm.p) ## There is zero inflation 








## Trying a negative binomial model
glm.nb_m_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_m)


## DHARMa checks 
testDispersion(glm.nb_m_comb_egg) ## model has changed a lot - underdispersed now? 


## Looking at a qq plot 
## Generating residuals 
residuals_glm_nb_m <- residuals(glm.nb_m_comb_egg , type = "pearson")
qnorm(residuals_glm_nb_m)
## Will generate a qq 
qqnorm(residuals_glm_nb_m) ## points go down compared to previous model



## Trying a mixed model
combined_glm_mm_m_egg <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = combined_ovi_m)



## DHARMa checks 
testDispersion(combined_glm_mm_m_egg) ## overdispersed 

## Looking at a qq plot 
## Generating residuals 
residuals_glm_mm_m <- residuals(combined_glm_mm_m_egg , type = "pearson")
qnorm(combined_glm_mm_m_egg)
## Will generate a qq 
qqnorm(combined_glm_mm_m_egg) ## points go down compared to previous model
   ## Can't do a qqplot with this code 


## Trying some zero inflation models as there is zero-inflation 

## zero inflated poisson
zi.p_m_comb_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_ovi_m)


## checks?? 

## zero inflated negative binomial 
zi.nb_m_comb_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_ovi_m)


AIC(comb_m_egg_glm.p, combined_glm_mm_m_egg, glm.nb_of_comb_egg, zi.p_m_comb_egg, zi.nb_m_comb_egg)
   ## the negative binomial glm has the lowest AIC by a while so is probably the best 


## checking the model out 
glm.nb_of_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_m)


## checking significance of block 
drop1(glm.nb_of_comb_egg, test = "F") ## says block is not significant!! 
summary(glm.nb_of_comb_egg)

## dropping block from the model
glm.nb_of_comb_egg_2 <- glm.nb(egg_numbers ~ diet, data =  combined_ovi_m)


## analysing results 
summary(glm.nb_of_comb_egg_2)

## pairwise test
emmeans::emmeans(glm.nb_of_comb_egg_2, pairwise ~ diet)






############################## --
#### Virgin Conditioning ####
############################# --


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




##### Data Analysis

# Model 1
# glm with poisson
comb_v_egg_glm.p <- glm(egg_numbers ~ diet * block, family = poisson,  combined_ovi_v)

## assumption checking 

## easystats
performance::check_model(comb_v_egg_glm.p, check = c("qq")) ## bit weird 
performance::check_model(comb_v_egg_glm.p, check = c("homogeneity")) ## slopey 
    ## Doesn't work for some reason - was "insight" 


## DHARMa
testDispersion(comb_v_egg_glm.p) # really overdispersed data

simulation_Output <- simulateResiduals(fittedModel = comb_v_egg_glm.p, plot = T)
   ## qq plot doesn't really work
   ## need to understand residuals vs predicted plot a bit better 


## generating a qqplot 
## Generating residuals 
residuals_glm_v_egg <- residuals(comb_v_egg_glm.p , type = "pearson")
qnorm(residuals_glm_v_egg)
## Will generate a qq 
qqnorm(residuals_glm_v_egg) ## looks okay? 


## Now checking for overdispersion values
summary(comb_v_egg_glm.p) ## very overdispersed 

## Checking for zero inflation 
check_zeroinflation(comb_v_egg_glm.p) 
  ## shows model is underfitting zeros 



# Model 2 
## Doing a negative binomial model 
glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet * block, data =  combined_ovi_v)

## Assumption checks for this 
## easystats
performance::check_model(glm.nb_v_comb_egg, check = c("qq")) ## pretty much the same as poisson
performance::check_model(glm.nb_v_comb_egg, check = c("homogeneity")) ## less slopey than poisson


## DHARMa checks
testDispersion(glm.nb_v_comb_egg) ## nor overdispersed or underdispersed now 

simulation_Output <- simulateResiduals(fittedModel = glm.nb_v_comb_egg, plot = T) ## this looks a lot better


# Model 3

## Trying a mixed glm 
glm_mm_v_egg <- glmmTMB(egg_numbers ~ diet * block + (1|factor(block)/plate) , family = poisson, data = combined_ovi_v)

## Assumption checks 


## easystats
## glmmTMB not supported 


## DHARMa checks
testDispersion(glm_mm_v_egg) ## looking overdispersed now 

simulateOutput <- simulateResiduals(fittedModel = glm_mm_v_egg, plot = T) # ? 

## trying a qq plot 
## Generating residuals 
residuals_glm_mm_v_egg <- residuals(glm_mm_v_egg , type = "pearson")
qnorm(residuals_glm_mm_v_egg)
## Will generate a qq 
qqnorm(residuals_glm_mm_v_egg) ## doesn't look that different to previous 


## trying zero inflation models:: 

# poisson
zif.p_v_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_ovi_m)

# negative binomial 
zif.nb_v_egg <- zeroinfl(egg_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_ovi_m)


## Comparing the models 
AIC(comb_v_egg_glm.p , glm.nb_v_comb_egg, glm_mm_v_egg, zif.p_v_egg, zif.nb_v_egg)

## the negative binomial models have the lowest AIC 
## go with negative binomial glm


## Using the negative binomial glm
glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet + block, data =  combined_ovi_v)

## tesing the signifiance of block 
drop1(glm.nb_v_comb_egg , test = "F") # block is significant 

## using the model 
summary(glm.nb_v_comb_egg)

glm.nb_v_comb_egg <- glm.nb(egg_numbers ~ diet + block, data =  combined_ovi_v)

glm.nb_v_comb_egg_2 <- glm.nb(egg_numbers ~ diet, data =  combined_ovi_v)

summary(glm.nb_v_comb_egg_2) 



emmeans::emmeans(glm.nb_v_comb_egg_2, pairwise ~ diet)

### why is it saying nothing is really significant now? 

