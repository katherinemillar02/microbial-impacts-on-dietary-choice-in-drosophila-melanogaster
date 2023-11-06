#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)


## Import Data
unconditioned_diets_rep1 <- read_excel("data/not_conditioned.xlsx")

## Making the data long 
unconditioned_diets_rep1_long <- unconditioned_diets_rep1 %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
unconditioned_diets_rep1_summary <- unconditioned_diets_rep1_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
unconditioned_diets_rep1_plot <- unconditioned_diets_rep1_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#E5B457",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#E5B457",
                width = 0.2)+
  geom_jitter(data = unconditioned_diets_rep1_long ,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.5)+
  labs(x = "Diet condition",
       y = "Mean +/- S.E. Number of flies per diet patch",
       title = "Unconditioned Diet Patches Rep 1")+
  theme_classic() 

## Combining the two plots 
conditioned_notconditioned_treatments <- conditioned_diets_rep1_plot + unconditioned_diets_rep1_plot
 
 # Saving the plots to a plots file
# not conditioned plot
ggsave("plots/unconditioned_diets_rep1_plot.png", dpi=300)
# a patchwork of male conditioned and not conditioned plot
ggsave("plots/conditioned_notconditioned_treatments", dpi=300)
 
 