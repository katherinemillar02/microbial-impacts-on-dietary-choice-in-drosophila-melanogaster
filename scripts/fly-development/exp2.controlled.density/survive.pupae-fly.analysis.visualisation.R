source("packages.R")
########################


#### Reading data in: ####

## Fly data
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")

## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 
## Summing up total flies
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  group_by(id, vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = FALSE)) 

## making it a dataframe 
overallflies_MFE <- as.data.frame(overallflies_MFE)





## Pupae data
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Summing up total pupae
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = FALSE))

## Changing it to a dataframe
total_pupae <- as.data.frame(total_pupae)



#### Combining data frames ####


## Calculating the data frames together, making a variable on the left of total pupae
flies_and_pupae <- overallflies_MFE %>%
  left_join(total_pupae, by = c("id", "vial", "treatment"))



## Calculating the survivability percentages of flies to pupae 
survivability_between <- flies_and_pupae %>%
  mutate(survivability = (total_count / total_pupae) * 100)




## Final chosen model:  Poisson Negative Binomial
glm.zi.nb.MFE.surviveboth <- glmmTMB(
  survivability ~  treatment + (1 | vial),
  ziformula =  ~ treatment,
  family = nbinom2(),
  data = survivability_between
)

#### Data Analysis ####

# Basic analysis
summary(glm.zi.nb.MFE.surviveboth)

# condidence intervals 
exp(confint(glm.zi.nb.MFE.surviveboth))

# getting values for the write-up
emmeans::emmeans(glm.zi.nb.MFE.surviveboth, specs = ~ treatment, type = "response")

# getting a table for the write up
tab_model(glm.zi.nb.MFE.surviveboth, CSS = list(css.table = '+font-family: Arial;'))







## Getting colours 
viridis_colors <- viridis(10)


## The plot code 
pupae_flies <- ggplot(survivability_between, aes(x = treatment, y = survivability, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.75)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Conditioned", "Unconditioned")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Survivability (%) of Flies from the Pupal Stage", 
       fill = "Treatment") +
  ylim(0, 100)


## Run plot
pupae_flies


## Saving a plot
ggsave(filename = "pupae_flies .png", 
       plot = pupae_flies , 
       width = 10, 
       height = 6, 
       dpi = 300)

