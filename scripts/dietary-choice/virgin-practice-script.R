## Analysis 

## Using Bin GLMM for now 

                                          #### FOUR-WAY INTERACTIONS  #### 
# First, testing for four-way interactions, using *
glmm.bin.v.01 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         ratio  * Conditioned * Unconditioned * block 
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## Looking for interaction in 4-way
drop1(glmm.bin.v.01, test = "Chisq")
 # No interaction found

# Then, testing four four-way interactions, using :
glmm.bin.v.011 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         ratio  : Conditioned : Unconditioned : block 
                       +  ratio  + Conditioned + Unconditioned + block 
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## Looking for interaction in 4-way
drop1(glmm.bin.v.011, test = "Chisq")
# No interaction found, but is close



         ## Why are these two so different? i get they arent the same model, 
# glmm.bin.v.01 also tests for two-way interactions




 



                                       #### THREE-WAY INTERACTIONS ####

## Testing for three-way interactions, using *
glmm.bin.v.02 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         ratio  * Conditioned * Unconditioned   
                       + Conditioned * Unconditioned * block  
                       + ratio * Conditioned * block  
                       + ratio * Unconditioned *  block 
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## results of interaction effects
drop1(glmm.bin.v.02, test = "Chisq")
## a 3-way interaction between ratio, Unconditioned, and block found 


## Now testing for three-way interactions, using : 
glmm.bin.v.021 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                          
                         ratio  : Conditioned : Unconditioned 
                       + Conditioned : Unconditioned : block  
                       + ratio : Conditioned : block 
                       + ratio : Unconditioned :  block 
                       
                       + ratio + Unconditioned +  block + Conditioned
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## results of interaction effects
drop1(glmm.bin.v.021, test = "Chisq")
  # 3-way interaction between ratio, Unconditioned, and block found 
  # 3-way interaction between ratio, Conditioned, and block found

## I dont understand why these are different, does glmm.bin.v.02 also show two- way interaction
## effects, because I am using * 







                                      #### TWO-WAY INTERACTIONS ####
         # For this, I first make a model that uses the previously relevant 3-way interactions for the relevant model. 
         # I then, see that this doesn'r work with drop1, so test for two-way interaction effects only. 


## using * , different 3 way interactions (including the previous 3-way interaction)
glmm.bin.v.03 <- glmer(cbind(Conditioned, Unconditioned)  ~ 
                       + ratio * Unconditioned *  block 
                       + Conditioned * Unconditioned 
                       + Conditioned * ratio 
                       + Conditioned * block 
                       + Unconditioned * ratio 
                       + Unconditioned *  block 
                       + ratio * block
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## results of interaction effects
drop1(glmm.bin.v.03, test = "Chisq")
## Only shows results of 3/6 2-way interaction effects 


## using * , different 3 way interactions (not including the previous 3-way interaction)
glmm.bin.v.030 <- glmer(cbind(Conditioned, Unconditioned)  ~ 
                       + Conditioned * Unconditioned 
                       + Conditioned * ratio 
                       + Conditioned * block 
                       + Unconditioned * ratio 
                       + Unconditioned *  block 
                       + ratio * block
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## results of interaction effects
drop1(glmm.bin.v.030, test = "Chisq")
  ## Shows all the 2-way interaction effects 
  ## Interaction effect between Conditioned and ratio found 






## using : , different 2 way interactions (including the previous 3-way interaction)
glmm.bin.v.032 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                          ratio : Conditioned : block 
                        +   ratio :  Unconditioned : block 
                        + Conditioned  : Unconditioned 
                        + Conditioned : block 
                        + Conditioned : ratio  
                        + Unconditioned : block
                        + Unconditioned : ratio  
                        + Conditioned : Unconditioned 
                        + Conditioned : ratio
                        + Conditioned : block 
                        + Unconditioned : ratio
                        + Unconditioned :  block 
                        + ratio : block 
                        + Conditioned + Unconditioned + ratio + block
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## testing for interaction effects
drop1(glmm.bin.v.032, test = "Chisq")



## using * , different 2 way interactions (including the previous 3-way interaction)
glmm.bin.v.0320 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         Conditioned  : Unconditioned  
                        + Conditioned : block 
                        + Conditioned : ratio  
                        + Unconditioned : block
                        + Unconditioned : ratio  
                        + Conditioned : Unconditioned 
                        + Conditioned : ratio 
                        + Conditioned : block 
                        + Unconditioned : ratio 
                        + Unconditioned :  block 
                        + ratio : block 
                        + Conditioned + Unconditioned + ratio + block
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## testing for interaction effects
drop1(glmm.bin.v.0320, test = "Chisq")
 ## An interaction effect only found between Conditioned and ratio





#### FINAL MODELS 

## Using * 
glmm.bin.v.04 <- glmer(cbind(Conditioned, Unconditioned) ~
                         + ratio * Unconditioned *  block + Conditioned 
                         + Conditioned * ratio + Unconditioned + block 
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)


drop1(glmm.bin.v.04, test = "Chisq")



## Using : 
glmm.bin.v.041 <- glmer(cbind(Conditioned, Unconditioned) ~
                         + ratio : Unconditioned :  block 
                        + ratio : Conditioned :  block 
                       + Conditioned : ratio + Unconditioned  
                       + Conditioned + Unconditioned + ratio + block
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)


drop1(glmm.bin.v.041, test = "Chisq")




                         
                       
