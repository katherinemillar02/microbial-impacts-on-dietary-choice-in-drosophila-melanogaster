##### #### #### ##### #### #### ##### #### #### ##### #### #### 

reproductive_adultstraits_f <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "females")
reproductive_adultstraits_f$day <- as.character(reproductive_adultstraits_f$day)

## Reading pupae data in
reproductive_adultstraits_m <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "males")
reproductive_adultstraits_m$day <- as.character(reproductive_adultstraits_m$day)




## Reading the data in:
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx", range = cell_cols("A:E"))

## Adding a sex section:
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")

fly.dev.3 <- read_excel("data/fitness_development/adulttraits_flydev.xlsx")
