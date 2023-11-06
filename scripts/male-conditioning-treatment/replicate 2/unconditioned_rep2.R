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
  ylim(0.0, 4.5)+
  labs(x = "Diet condition",
       y = "Male Conditioned Diet Patches",
       title = "Unconditioned and Conditioned Treatment Rep 2")+
  theme_classic() 


## Testing patchwork of Conditioned and Unconditioned plots together
conditioned_and_unconditioned_rep2 <- conditioned_diets_2_plot + unconditioned_rep2_plot 





##  Saving the plot to a plots file
ggsave("plots/unconditioned_rep2_plot.png", dpi=300)
ggsave("plots/conditioned_and_unconditioned_rep2.png", dpi=300)


#### Data Analysis 

# First testing a linear model 
unconditioned_rep2_lm <- lm(fly_numbers ~  diet, data = unconditioned_rep2_long)

# Assumption Checking of the model 
performance::check_model(unconditioned_rep2_lm, check = c("qq")) # a bit poor - dots around
performance::check_model(unconditioned_rep2_lm, check = c("homogeneity")) # not great
performance::check_model(unconditioned_rep2_lm, check = c("linearity")) # not great
performance::check_model(unconditioned_rep2_lm, check = c("outliers"))


# Trying a generalised linear model
unconditioned_rep2_glm <- glm(fly_numbers ~  diet, family = quasipoisson(link = "log"), data = unconditioned_rep2_long)


performance::check_model(unconditioned_rep2_glm, check = c("qq")) # dots seem to match to line better than lm rep 1 and 2
performance::check_model(unconditioned_rep2_glm, check = c("homogeneity")) # not great either but dots do appear? - line isn't flat
performance::check_model(unconditioned_rep2_glm, check = c("outliers"))

# glm qq a lot better 

# summary function, shows t test
summary(unconditioned_rep2_glm)

# using anova 
anova(unconditioned_rep2_glm)

# emmeans for tukey
emmeans::emmeans(unconditioned_rep2_glm, pairwise ~ diet)
