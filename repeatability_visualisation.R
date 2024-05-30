d1<- tibble(
  fly = c("f1", "f2"),
  weight = c(0.937, 0.920))

m1 <- ggplot(d1, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  


d2<- tibble(
  fly = c("f2", "f3"),
  weight = c(0.920, 0.833))

m2 <- ggplot(d2, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  



d3<- tibble(
  fly = c("f3", "f1"),
  weight = c(0.833, 0.937))

m3 <- ggplot(d3, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  

male_fly_1 <- m1 + m2 + m3




d4<- tibble(
  fly = c("f4", "f5"),
  weight = c(0.733, 0.717))

m4 <- ggplot(d4, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  


d5<- tibble(
  fly = c("f5", "f6"),
  weight = c(0.717, 0.699))

m5 <- ggplot(d5, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  



d6<- tibble(
  fly = c("f6", "f4"),
  weight = c(0.699, 0.733))

m6 <- ggplot(d6, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  

male_fly_2 <- m4 + m5 + m6

male_fly_1 / 
  male_fly_2


d1<- tibble(
  fly = c("f1", "f2"),
  weight = c(1.496, 1.457))

f1 <- ggplot(d1, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  


d2<- tibble(
  fly = c("f2", "f3"),
  weight = c(1.457, 1.477))

f2 <- ggplot(d2, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  



d3<- tibble(
  fly = c("f3", "f1"),
  weight = c(1.477, 1.496))

f3 <- ggplot(d3, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +         
  theme_minimal()  

female_fly_1 <- f1 + f2 + f3






