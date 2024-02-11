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

## this creates a list were both data fr