## Packages
library(tidyverse)
library(ggplot2)
library(readxl)

## Read data in
pupae_fitness <- read_excel("data/fitness_experiment/pupae_data.xlsx")



## Creating a plot 
yes <- ggplot(pupae_fitness, aes(x = `time (hours)`, y = pupae, color = treatment))
 




