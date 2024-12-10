## Uploading Packages using source
source("packages.R")
source()

                                 #### DATA VISUALISATION #### 

## Setting a colour palette from the "viridis" package
viridis_colors <- viridis(10)

# Subsetting a female dataset, so sex can be visualised separatley. 
female.dev.3 <- subset(fly.dev.3, select = c(time_hours, females, treatment))

# Visualising the development of female flies. 
female.dev.3.boxplot <- ggplot(female.dev.3, aes(x = factor(time_hours), y = females, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(female.dev.3$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since L1 on diets", 
       y = "Number of Females Emerged",
       fill = "Treatment")

## Running plot 
female.dev.3.boxplot



# Subsetting a male dataset, so sex can be visualised separatley. 
male.dev.3 <- subset(fly_fitness_adulttraits, select = c(time_hours, males, treatment))

# Visualising the development of male flies. 
male.dev.3.boxplot <- ggplot(male.dev.3, aes(x = factor(time_hours), y = males, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(males_data$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since L1 on diets", 
       y = "Number of Males Emerged",
       fill = "Treatment")

## Running plot 
male.dev.3.boxplot


## Using patchwork to combine the female and male plots
fly.dev.3.boxplot <- female.dev.3.boxplot / 
  male.dev.3.boxplot

# Running the plot 
fly.dev.3.boxplot

## Saving the plot
ggsave(filename = "fly.dev.3.boxplot.png", 
       plot = fly.dev.3.boxplot, 
       width = 10, 
       height = 6, 
       dpi = 300)






                                 #### DATA ANALYSIS ####


## Separating the data into female and male columns 
fly.dev.3.tidy <- tidyr::pivot_longer(data = fly.dev.3 ,
                                      cols = c( females, males),
                                      names_to = "sex",
                                      values_to = "count")




## Cleaning the data into a tidier format, for correct data analysis

# Making NAs 0, so this code works 
fly.dev.3.tidy <- fly.dev.3.tidy %>% 
  filter(!is.na(count))

# Changing count to uncount, so the format is better 
fly.dev.3.tidy <- uncount(fly.dev.3.tidy, count)

# Understanding the data
summary_data <- fly_fitness_adulttraits %>%
  group_by(treatment, time_hours, vial) %>%
  summarise(total_females = sum(females),
            total_males = sum(males))



#### DATA ANALYSIS RESULTS #### 



#### BEST MODEL, from models tried in fly.dev.exp3.scraps: 
glmm.nb.flydev.3 <- glmmTMB(time_hours ~ 
                              
                              treatment * sex
                            
                            + (1|sex/vial) ,
                            
                            family = nbinom2(), data = fly.dev.3.tidy)




# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.flydev.3, test = "Chisq")
# No 2-way interaction effect


#### Chosen model: NegBin ####
glmm.nb.flydev.3.2 <- glmmTMB(time_hours ~ 
                              
                              treatment + sex
                            
                            + (1|sex/vial) ,
                            
                            family = nbinom2(), data = fly.dev.3.tidy)





#### Data Analysis for write-up #### 

# Basic analysis 
summary(glmm.nb.flydev.3.2)

# Real values for write-up
emmeans::emmeans(glmm.nb.flydev.3.2, specs = ~ sex + treatment, type = "response")

# Confidence intervals
exp(confint(glmm.nb.flydev.3.2))

# Table for write-up 
tab_model(glmm.nb.flydev.3.2, CSS = list(css.table = '+font-family: Arial;'))




