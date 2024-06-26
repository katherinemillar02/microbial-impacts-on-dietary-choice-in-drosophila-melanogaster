
malepath <- "data/male_conditioning"



read_raw_male <- function(path = malepath, patterns_to_exclude = c("m4-1_1-4", "m1-4")){
  list_of_files <- list.files(path = malepath,
                              pattern = "rawdata", full.names = T)
  
  list_of_files <- list_of_files[!grepl(patterns_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

df_male <- malepath %>% 
  map_df(~read_raw_male(.x))


df_male <- df_male %>%
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    
  ))



