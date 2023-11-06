#### INSTALL PACKAGES 
library(tidyverse)  
library(readxl)
library(patchwork)

#### CALCULATING THE MEAN AVERAGE 

## Import Data
conditioned_and_unconditioned_diets_2 <- read_excel("data/male_conditionedandunconditioned_rep2_mean.xlsx")

## Making the data long 
conditioned_and_unconditioned_diets_2_long <- conditioned_and_unconditioned_diets_2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



## Summarising the data; mean, sd, se
conditioned_and_unconditioned_diets_2_summary <- conditioned_and_unconditioned_diets_2_long   %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


## Visualising the mean average with a bar plot
# creating a boxplot with the data that has been summarised
conditioned_and_unconditioned_diets_2_plot <- conditioned_and_unconditioned_diets_2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#E5B457",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#E5B457",
                width = 0.2)+
  geom_jitter(data = conditioned_and_unconditioned_diets_2_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet Condition",
       y = "Mean +/- S.E. Number of flies per diet patch",
       title = "Conditioned and Unconditioned Diet Patches Rep 2")+
  theme_classic() 

ggsave("plots/conditioned_and_unconditioned_diets_2_plot.png", dpi=300)

