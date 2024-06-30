## Analysis 

## Using Bin GLMM for now 

                                          #### FOUR-WAY INTERACTIONS  #### 
# First, testing foR four-way interactions, using *
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

         ## Why are these two so different? 




 



                                       #### THREE-WAY INTERACTIONS ####

## Testing for three-way interactions, using *
glmm.bin.v.02 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         ratio  * Conditioned * Unconditioned + block   
                       + Conditioned * Unconditioned * block  + ratio  
                       + ratio * Conditioned * block  + Unconditioned
                       + ratio * Unconditioned *  block + Conditioned 
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## results of interaction effects
drop1(glmm.bin.v.02, test = "Chisq")
## a 3-way interaction between ratio, Unconditioned, and block found 


## Now testing for three-way interactions, using : 
glmm.bin.v.021 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         ratio  : Conditioned : Unconditioned + block   
                       + Conditioned : Unconditioned : block  + ratio  
                       + ratio : Conditioned : block  + Unconditioned
                       + ratio : Unconditioned :  block + Conditioned 
                       + ratio + Unconditioned +  block + Conditioned 
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## results of interaction effects
drop1(glmm.bin.v.021, test = "Chisq")
  # 3-way interaction between ratio, Unconditioned, and block found 
  # 3-way interaction between ratio, Conditioned, and block found








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
                          ratio : Conditioned : block + Unconditioned
                        +   ratio :  Unconditioned : block + Conditioned
                        + Conditioned  : Unconditioned  + block + ratio 
                        + Conditioned : block + ratio + Unconditioned 
                        + Conditioned : ratio + Unconditioned + block 
                        + Unconditioned : block + Conditioned + ratio
                        + Unconditioned : ratio + Conditioned + block 
                        + Conditioned : Unconditioned + ratio + block
                        + Conditioned : ratio + Unconditioned + block
                        + Conditioned : block + Unconditioned + ratio
                        + Unconditioned : ratio + Conditioned + block
                        + Unconditioned :  block + Conditioned + ratio
                        + ratio : block + Conditioned + Unconditioned
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## testing for interaction effects
drop1(glmm.bin.v.032, test = "Chisq")



## using * , different 2 way interactions (including the previous 3-way interaction)
glmm.bin.v.0320 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         Conditioned  : Unconditioned  + block + ratio 
                        + Conditioned : block + ratio + Unconditioned 
                        + Conditioned : ratio + Unconditioned + block 
                        + Unconditioned : block + Conditioned + ratio
                        + Unconditioned : ratio + Conditioned + block 
                        + Conditioned : Unconditioned + ratio + block
                        + Conditioned : ratio + Unconditioned + block
                        + Conditioned : block + Unconditioned + ratio
                        + Unconditioned : ratio + Conditioned + block
                        + Unconditioned :  block + Conditioned + ratio
                        + ratio : block + Conditioned + Unconditioned
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
                         + ratio : Unconditioned :  block + Conditioned 
                        + ratio : Conditioned :  block + Unconditioned 
                       + Conditioned : ratio + Unconditioned + block 
                       + Conditioned + Unconditioned + ratio + block
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)


drop1(glmm.bin.v.041, test = "Chisq")




                         
                       
