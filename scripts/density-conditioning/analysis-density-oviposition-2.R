#### OVIPOSITION ANALYSIS 


#### Reading the data in 
ovipositiondensitypath <- "data/density_experiment"


## Creating a data file path 
read_raw_oviposition_density <- function(path = ovipositiondensitypath, pattern_to_exclude = "combined"){
  list_of_files <- list.files(path = ovipositiondensitypath,
                              pattern = "oviposition_2", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_oviposition_density(ovipositiondensitypath) # this will show the new data set 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_oviposition_density <- ovipositiondensitypath %>% 
  map_df(~read_raw_oviposition_density(.x))

# mutating a variable for block from the data id 
df_oviposition_density <- df_oviposition_density %>%
  mutate(density = case_when(
    str_detect(id, "50") ~ "50",
    str_detect(id, "90") ~ "90",
    
  ))



# Separate diet column and group by relevant variables
df_oviposition_density_2 <- df_oviposition_density %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%
  group_by(id, plate, ratio, condition, density) %>%
  summarise(count = sum(egg_numbers)) %>%
  pivot_wider(names_from = "condition", values_from = "count")

## The data set 
df_oviposition_density_2


glmer.mm_density <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * density + (1|plate) , family = binomial, data = df_oviposition_density_2)

drop1(glmer.mm_density, test = "Chisq")
summary(glmer.mm_density)









# COMBINED ANALYSIS

fourone_onefour_90mm <- read_excel("data/density_experiment/90mm_combined_oviposition_2.xlsx")

fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")



fourone_onefour_50mm <- read_excel("data/density_experiment/50mm_combined_oviposition_2.xlsx")

fourone_onefour_50mm_long <- fourone_onefour_50mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

fourone_onefour_90mm_long <- fourone_onefour_90mm_long %>%
  mutate(density = "90mm")

fourone_onefour_50mm_long <- fourone_onefour_50mm_long %>%
  mutate(density = "50mm")


combinedoviposition <- rbind(fourone_onefour_90mm_long, fourone_onefour_50mm_long)



## mutate density variable


combined_glm_mm_m_egg <- glmmTMB(egg_numbers ~ diet * density + (1|plate) , family = poisson, data = combinedoviposition)

drop1(combined_glm_mm_m_egg, test = "Chisq")

summary(combined_glm_mm_m_egg)
