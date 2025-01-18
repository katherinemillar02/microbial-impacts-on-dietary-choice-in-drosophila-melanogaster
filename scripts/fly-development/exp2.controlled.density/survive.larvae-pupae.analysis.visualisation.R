source("packages.R")
source("scripts/fly-development/exp2.controlled.density/c.density.dataread.R")


## Reading pupae data in:
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")

#### Pupae data check:
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = FALSE))


## Changing it to a dataframe:
total_pupae <- as.data.frame(total_pupae)


## Working out survivability code:
survivability_pupae <- total_pupae %>%
  mutate(fixed_total = 63, 
         survivability = (total_pupae / fixed_total) * 100)





#### Data Analysis #### 


#### Poisson GLMM ####
glmm.p.pupaesurvive.MFE <- glmmTMB(survivability ~ 
                                     
                                     treatment +
                                     
                                     + (1|vial) + (1|id), 
                                   
                                   data = survivability_pupae)
                                   
       

#### Final Data Analysis ####

# Simple model test 
summary(glmm.p.pupaesurvive.MFE)


## Getting numbers for the write-up
emmeans::emmeans(glmm.p.pupaesurvive.MFE, specs =  ~ treatment, type = "response")


# Table for write-up
tab_model(glmm.p.pupaesurvive.MFE, CSS = list(css.table = '+font-family: Arial;'))



larvae_pupae <- ggplot(survivability_pupae, aes(x = treatment, y = survivability, fill = treatment)) +
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
       y = "Survivability (%) of pupae from larval stage", 
       fill = "Treatment") +
  ylim(0, 100)


## Saving a plot
ggsave(filename = "larvae_pupae.png", 
       plot = larvae_pupae, 
       width = 10, 
       height = 6, 
       dpi = 300)


