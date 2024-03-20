#### Male Conditioning 
source("")
#### Doing the raw data 

# doing both as I don't know which one is better
m_four_one_raw <- rbind(four_to_one_b1_raw, four_to_one_b2_raw)

m_four_one_raw_long <- rbind(four_to_one_b1_raw_long, four_to_one_b2_raw_long)

## binded four to one and one to four 

# doing both as I don't know which one is better
m_one_four_raw <- rbind(one_to_four_b1_raw, one_to_four_b2_raw)


## Making the data long
m_one_four_raw_long <- rbind(one_to_four_b1_raw_long, one_to_four_b2_raw_long)

#### binded the two binded scripts 
male_conditioned_experiments <- rbind(m_four_one_raw_long,m_one_four_raw_long)

## Making a model with this? 

bin_mod <- glm(cbind('4:1 Conditioned', '4:1 Unconditioned','1:4 Conditioned', '1:4 Unconditioned') ~ 4, data = male_conditioned_experiments)

#Error in terms.formula(formula, data = data) : invalid model formula in ExtractVars - cannot use cbind? 
#Tracey mentioned something about learning the analysis where you counter in they are also not feeding on any diet