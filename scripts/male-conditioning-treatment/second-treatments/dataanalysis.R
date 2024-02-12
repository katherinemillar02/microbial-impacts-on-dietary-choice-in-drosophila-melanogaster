# Packages
library(tidyverse)
library(lmerTest)
library(readxl)

# Raw data
## data on treatment 2 block 1 male feeding

## Creating a path to the scripts within block 1 and block 2 paths
path<- "data/male_conditioning/treatment_2/block_1"

path2<-"data/male_conditioning/treatment_2/block_2"

## This creates  function
## Path is interchangeable with path 2 
read_raw<-function(path = path, pattern_to_exclude = "4-1_1-4"){
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
read_raw(path) 
read_raw(path2)

## this creates a list were both data from path 1 and path 2 
paths<-list(path,path2)

## creating an actual data set that will read the paths
# first data frame - purr package 
df<-paths%>% map_df(~read_raw(.x)) #.x is a string or NULL - only applies to dfr apparently

## "second" data frame
# uses what was generated with "df"
df2<-df%>% separate(diet, into = c("ratio", "condition"), sep = " ") %>% #separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition)%>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

df2 # does it recognise condition from the long data? 

## should now not include 4:1_1:4 assay but I don't know if this worked
head(df2)


###########

# binomial model
## The Statistics!!!!
binomial_model <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2)

# mixed model 
mixed_model<-glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate/observation) + (1|id), family = binomial, data = df2)

## still says it is not significant

## using the DHARMa package 
library(DHARMa)

resid.mm<-simulateResiduals(mixed_model)
plot(resid.mm)



## creating a data column where flies are not on the plate 
df2 <- df2 %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))



## Creating a path to the scripts within block 1 and block 2 paths
## use for when you get both blocks
pathvirgin <- "data/female_conditioning/virgin"

## This creates  function
## Path is interchangeable with path 2 
read_raw_virgin <-function(path = pathvirgin, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathvirgin,
                              pattern = "rawresults", full.names = T)
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_virgin(pathvirgin) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin <- pathvirgin%>% map_df(~read_raw_virgin(.x)) #.x is a string or NULL - only applies to dfr apparently



## creating a function to pull the median
fly_numbers_summary()<- function(data, group_col) {
  summary <- data %>%
    group_by(plate, {{ group_col }}) %>%
    summarise(median = median(fly_numbers))
  return(summary)
}

four_one_virgin_summary <- fly_numbers_summary(four_to_one_virgin_long, diet)
one_four_virgin_summary <- fly_numbers_summary(one_to_four_virgin_long, diet)
fourone_onefour_virgin_summary <- fly_numbers_summary(fourone_onefour_virgin_long, diet)
