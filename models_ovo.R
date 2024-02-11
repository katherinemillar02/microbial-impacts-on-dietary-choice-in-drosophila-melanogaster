library(tidyverse)
library(lmerTest)
library(readxl)

# Raw data
## data on treatment 2 block 1 male feeding

## Creating a path to the scripts within block 1 and block 2 paths
pathovo <- "data/female_conditioning/ovod1/block_1"

## This creates  function
## Path is interchangeable with path 2 
read_raw_ovo<-function(pathovo = pathovo, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = path,
                              pattern = "rawdata", full.names = T)
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovo(path) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovo<-path%>% map_df(~read_raw_ovo(.x)) #.x is a string or NULL - only applies to dfr apparently

## "second" data frame
# uses what was generated with "df"
df2_ovo<-df_ovo%>% separate(diet, into = c("ratio", "condition"), sep = " ") %>% #separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition)%>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

df2_ovo # does it recognise condition from the long data?

## "basic" binomial model
binomial_model_ovo <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_ovo)
summary(binomial_model_ovo)
emmeans::emmeans(binomial_model_ovo, pairwise ~ ratio)

# mixed model 
mixed_model_ovo<-glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate/observation) + (1|id), family = binomial, data = df2)
summary(mixed_model_ovo)
emmeans::emmeans(mixed_model_ovo, pairwise ~ ratio)


