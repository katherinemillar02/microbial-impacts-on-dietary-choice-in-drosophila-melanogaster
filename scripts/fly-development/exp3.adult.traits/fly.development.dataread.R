##### #### #### ##### #### #### ##### #### #### ##### #### #### 

reproductive_adultstraits_f <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "females")
reproductive_adultstraits_f$day <- as.character(reproductive_adultstraits_f$day)

## Reading pupae data in
reproductive_adultstraits_m <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "males")
reproductive_adultstraits_m$day <- as.character(reproductive_adultstraits_m$day)


