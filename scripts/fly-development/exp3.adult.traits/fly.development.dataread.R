##### #### #### ##### #### #### ##### #### #### ##### #### #### 

reproductive_adultstraits_f <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "females")

reproductive_adultstraits_f$ageofflyeggslaid <- as.character(reproductive_adultstraits_f$ageofflyeggslaid)

reproductive_adultstraits_f$ageofflyeggslaid <- factor(reproductive_adultstraits_f$ageofflyeggslaid,
                                          levels = c("5","12","22"))

## Reading pupae data in
reproductive_adultstraits_m <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "males")
reproductive_adultstraits_m$ageofflyeggslaid <- factor(reproductive_adultstraits_m$ageofflyeggslaid,
                                             levels = c("5","12","22","25","29","32","36","39","43","45"))




## Reading the data in:
survival <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx", range = cell_cols("A:E"))

## Adding a sex section:
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")

fly.dev.3 <- read_excel("data/fitness_development/adulttraits_flydev.xlsx")
