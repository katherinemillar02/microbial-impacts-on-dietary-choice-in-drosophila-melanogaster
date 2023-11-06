#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)


## Import Data
unconditioned_rep2 <- read_excel("data/male_unconditioned_rep2.xlsx")

## Making the data long 
unconditioned_rep2_long <- unconditioned_rep2%>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
unconditioned_rep2_summary <- unconditioned_rep2_long   %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
unconditioned_rep2_plot <- unconditioned_rep2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = unconditioned_rep2_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 5.0)+
  labs(x = "Diet condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 2")+
  theme_classic() 




##  Saving the plot to a plots file
ggsave("plots/unconditioned_rep2_plot.png", dpi=300)
