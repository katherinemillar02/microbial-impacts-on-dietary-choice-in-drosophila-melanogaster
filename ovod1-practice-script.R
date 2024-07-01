## OvoD1 Conditioning 


###################################-
#### OvoD1 FEMALE CONDITIONING ----
####################################-

pathovod1 <- "data/female_conditioning/ovod1"

## This creates  function
## Path is interchangeable with path 2 
read_raw_ovod1 <-function(path = pathovod1, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathovod1,
                              pattern = "rawresults", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1(pathovod1) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1 <- pathovod1 %>% 
  map_df(~read_raw_ovod1(.x)) #.x is a string or NULL - only applies to dfr apparently

# Adding block variables
df_ovod1 <- df_ovod1 %>% 
  mutate( block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two"
  ))

# uses what was generated with "df"
df2_ovod1 <- df_ovod1 %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition, block) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")


## Analysis, choosing Bin GLMM for now 


glmm.bin.o.01 <- glmer(cbind(Conditioned, Unconditioned) ~
                         ratio  * Conditioned * Unconditioned * block 
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

drop1(glmm.bin.o.01, test = 'Chisq')
# No 4-way interaction effect 



# Model 
glmm.bin.o.02 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         
                         ratio  * Conditioned * Unconditioned 
                       + ratio   * Unconditioned * block 
                       + ratio  * Conditioned * block  
                       + block * ratio * Conditioned  
                       + block * ratio * Unconditioned 
                       + block * Conditioned * Unconditioned
                       
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

drop1(glmm.bin.o.02, test = 'Chisq')


## Testing for 2-way interaction effects 
# but includes the previous sig 3 - one 
glmm.bin.o.03 <- glmer(cbind(Conditioned, Unconditioned)
                       ~ Unconditioned * block * ratio + 
                         Conditioned * Unconditioned +   
                         Conditioned * block + 
                         Conditioned * ratio +
                         Unconditioned * block + 
                         Unconditioned * ratio +
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


drop1(glmm.bin.o.03, test = 'Chisq')


## withouth the previous sig 3-way one 
glmm.bin.o.031 <- glmer(cbind(Conditioned, Unconditioned)
                       ~  
                         Conditioned * Unconditioned +   
                         Conditioned * block + 
                         Conditioned * ratio +
                         Unconditioned * block + 
                         Unconditioned * ratio +
                         
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


drop1(glmm.bin.o.031, test = 'Chisq')
# no 2- way interactions? 


## Final Model? 
glmm.bin.o.04 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         Unconditioned * block * ratio + Conditioned
                         
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Using drop1 to find the approprirate interaction effects 
drop1(glmm.bin.o.04, test = 'Chisq')
## Confused as to why I am getting different results.














## doing the same, but with : 
glmm.bin.o.001 <- glmer(cbind(Conditioned, Unconditioned) ~
                         ratio  : Conditioned : Unconditioned : block
                        + ratio  + Conditioned + Unconditioned + block
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

drop1(glmm.bin.o.001, test = 'Chisq')
# No 4-way interaction effect 



# Model 
glmm.bin.o.002 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         
                         ratio  : Conditioned : Unconditioned 
                       + ratio   : Unconditioned : block 
                       + ratio  : Conditioned : block  
                       + block : ratio : Conditioned  
                       + block : ratio : Unconditioned 
                       + block : Conditioned : Unconditioned
                       + Conditioned + Unconditioned + ratio + block
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

drop1(glmm.bin.o.002, test = 'Chisq')


## Testing for 2-way interaction effects 
# but includes the previous sig 3 - one 
glmm.bin.o.003 <- glmer(cbind(Conditioned, Unconditioned)
                       ~ Unconditioned : block : ratio + 
                         Conditioned : Unconditioned +   
                         Conditioned : block + 
                         Conditioned : ratio +
                         Unconditioned : block + 
                         Unconditioned : ratio +
                         + Conditioned + Unconditioned + ratio + block
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


drop1(glmm.bin.o.003, test = 'Chisq')


## withouth the previous sig 3-way one 
glmm.bin.o.0031 <- glmer(cbind(Conditioned, Unconditioned)
                        ~  
                          Conditioned : Unconditioned +   
                          Conditioned : block + 
                          Conditioned : ratio +
                          Unconditioned : block + 
                          Unconditioned : ratio +
                          + Conditioned + Unconditioned + ratio + block
                          
                         + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


drop1(glmm.bin.o.0031, test = 'Chisq')
# no 2- way interactions? 


## Final Model? 
glmm.bin.o.004 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         Unconditioned : block : ratio
                       + Conditioned + Unconditioned + ratio + block
                       
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Using drop1 to find the approprirate interaction effects 
drop1(glmm.bin.o.004, test = 'Chisq')
## Confused as to why I am getting different results.

