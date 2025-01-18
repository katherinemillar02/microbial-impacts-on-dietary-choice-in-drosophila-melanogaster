source("packages.R")
source("scripts/fly-development/exp2.controlled.density/c.density.dataread.R")

## Testing for a two-way interaction effect 
glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_MFE)

## Looking for significance in the two-way interaction effect using drop1()
drop1(glm.nb.MFE.weight, test = "Chisq")
 # No significance 

## Using the model without the two-way interaction effect 
glm.nb.MFE.weight.2 <- glm.nb(weight_mg ~ treatment + sex  
                            
                            , data = bodyweight_MFE)


##### Data analysis for write-up ####

# Basic analysis 
summary(glm.nb.MFE.weight.2)

## Looking for confidence intervals 
exp(confint(glm.nb.MFE.weight.2))


## Real values for write-up
emmeans::emmeans(glm.nb.MFE.weight.2, specs =  ~ treatment + sex , type = "response")


## Table of model for write-up
tab_model(glm.nb.MFE.weight.2, CSS = list(css.table = '+font-family: Arial;'))



## Visualising the data
bodyweight_plot_MFE <- ggplot(bodyweight_MFE, aes(x = sex, y = weight_mg, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colours[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical")+
  labs(x = "Sex", 
       y = "Body Weight (μg) of fly") +
  labs(fill = "Treatment")+
  ylim(0,700)

## Displaying plot 
bodyweight_plot_MFE 



## Saving a plot
ggsave(filename = "bodyweight_plot_MFE.png", 
       plot = bodyweight_plot_MFE, 
       width = 10, 
       height = 6, 
       dpi = 300)





