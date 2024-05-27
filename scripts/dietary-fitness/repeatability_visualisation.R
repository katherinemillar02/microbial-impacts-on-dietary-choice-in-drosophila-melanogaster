
#### FLY 1 

d1<- tibble(
  fly = c("f1", "f2"),
  weight = c(0.937, 0.920))

0.937 - 0.920 = 0.017

m1 <- ggplot(d1, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +    
  ylim(0.6,0.95) +
  theme_minimal()  


d2<- tibble(
  fly = c("f2", "f3"),
  weight = c(0.920, 0.833))

0.920 - 0.833 = 0.087

m2 <- ggplot(d2, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(0.6,0.95) +
  theme_minimal()  



d3<- tibble(
  fly = c("f3", "f1"),
  weight = c(0.833, 0.937))

0.937 - 0.833 = 0.104 

m3 <- ggplot(d3, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(0.6,0.95) +
  theme_minimal()  

male_fly_1 <- m1 + m2 + m3

male_fly_1 + ggtitle("Fly 1")

## 0.06933333

#### FLY 2

d4<- tibble(
  fly = c("f4", "f5"),
  weight = c(0.733, 0.717))

0.733 - 0.717 = 0.016

m4 <- ggplot(d4, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(0.6,0.95) +
  theme_minimal()  


d5<- tibble(
  fly = c("f5", "f6"),
  weight = c(0.717, 0.699))

 0.717 - 0.699 = 0.018

m5 <- ggplot(d5, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +
  ylim(0.6,0.95) +
  theme_minimal()  





d6<- tibble(
  fly = c("f6", "f4"),
  weight = c(0.699, 0.733))

m6 <- ggplot(d6, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +
  ylim(0.6,0.95) +
  theme_minimal()  

0.733 - 0.699  = 0.034

male_fly_2 <- m4 + m5 + m6



male_fly_2 + ggtitle("Fly 2")



#### FLY 3

d7<- tibble(
  fly = c("f7", "f8"),
  weight = c(0.739, 0.716))

0.739 - 0.716 = 0.023 

m7 <- ggplot(d7, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(0.6,0.95) +
  theme_minimal()  


d8<- tibble(
  fly = c("f8", "f9"),
  weight = c(0.716, 0.718))


0.718 - 0.716 = 0.002

m8 <- ggplot(d8, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +
  ylim(0.6,0.95) +
  theme_minimal()  



d9<- tibble(
  fly = c("f9", "f7"),
  weight = c(0.718, 0.739))

0.739 - 0.718 = 0.021

m9 <- ggplot(d9, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(0.6,0.95) +
  theme_minimal()  

male_fly_3 <- m7 + m8 + m9

male_fly_3 + ggtitle("Fly 3")


#### FLY 4


d10<- tibble(
  fly = c("f10", "f11"),
  weight = c(0.782, 0.773))

0.782 - 0.773 = 0.009 

m10 <- ggplot(d10, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +
  ylim(0.6,0.95) +
  theme_minimal()  


d11<- tibble(
  fly = c("f11", "f12"),
  weight = c(0.773, 0.762))

0.773 - 0.762 = 0.011

m11 <- ggplot(d11, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +
  ylim(0.6,0.95) +
  theme_minimal()  



d12<- tibble(
  fly = c("f12", "f10"),
  weight = c(0.762, 0.782))

0.782 - 0.762 = 0.02

m12 <- ggplot(d12, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(0.6,0.95) +
  theme_minimal()  

male_fly_4 <- m10 + m11 + m12


#### FLY 5
d13<- tibble(
  fly = c("f10", "f11"),
  weight = c(0.861, 0.852))

0.861 - 0.852 = 0.009

m13 <- ggplot(d13, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +   
  ylim(0.6,0.95) +
  theme_minimal()  


d14<- tibble(
  fly = c("f14", "f15"),
  weight = c(0.852, 0.712))

0.852 - 0.712 = 0.14
 
m14 <- ggplot(d14, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(0.6,0.95) +
  theme_minimal()  



d15 <- tibble(
  fly = c("f15", "f16"),
  weight = c(0.712, 0.861))

0.861 - 0.712 = 0.149


m15 <- ggplot(d15, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(0.6,0.95) +
  theme_minimal()  

male_fly_5 <- m13 + m14 + m15

male_fly_1 / 
  male_fly_2 /
  male_fly_3 /
  male_fly_4 /
  male_fly_5 







d1<- tibble(
  fly = c("f1", "f2"),
  weight = c(1.496, 1.457))


1.496 -  1.457 = 0.039

f1 <- ggplot(d1, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +   
  ylim(1.2,1.5) +
  theme_minimal()  


d2<- tibble(
  fly = c("f2", "f3"),
  weight = c(1.457, 1.477))

1.477 - 1.457 = 0.02

f2 <- ggplot(d2, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +   
  ylim(1.2,1.5) +
  theme_minimal()  



d3<- tibble(
  fly = c("f3", "f1"),
  weight = c(1.477, 1.496))

1.496 - 1.477 = 0.019

f3 <- ggplot(d3, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(1.2,1.5) +
  theme_minimal()  


female_fly_1 <- f1 + f2 + f3



d4 <- tibble(
  fly = c("f4", "f5"),
  weight = c(1.508, 1.118))

1.508 - 1.118 = 0.39


f4 <- ggplot(d4, aes(x = fly, y = weight, group = 1)) +
  geom_point()+         
  geom_line() +  
  ylim(1.2,1.5) +
  theme_minimal()  



female_fly_2 <- f4 









d7<- tibble(
  fly = c("f7", "f8"),
  weight = c(1.286, 1.325))

1.325 - 1.286 =  0.039

f7 <- ggplot(d7, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(1.2,1.5) +
  theme_minimal()  


d8<- tibble(
  fly = c("f2", "f3"),
  weight = c(1.325, 1.398))

1.398 - 1.325 = 0.073

f8 <- ggplot(d8, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(1.2,1.5) +
  theme_minimal()  



d9<- tibble(
  fly = c("f9", "f7"),
  weight = c(1.398, 1.286))

1.398 - 1.286 = 0.112

f9 <- ggplot(d9, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(1.2,1.5) +
  theme_minimal()  




female_fly_3 <- f7 + f8 + f9



d10 <- tibble(
  fly = c("f10", "f11"),
  weight = c(1.391, 1.416))

1.416 - 1.391 = 0.025

f10 <- ggplot(d10, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +   
  ylim(1.2,1.5) +
  theme_minimal()  


d11 <- tibble(
  fly = c("f11", "f12"),
  weight = c(1.416, 1.369))

1.416 - 1.369 = 0.047

f11 <- ggplot(d11, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +  
  ylim(1.2,1.5) +
  theme_minimal()  



d12 <- tibble(
  fly = c("f12", "f10"),
  weight = c(1.369, 1.391))


1.391 - 1.369 = 0.022

f12 <- ggplot(d12, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(1.2,1.5) +
  theme_minimal()  


female_fly_4 <- f10 + f11 + f12 






d13 <- tibble(
  fly = c("f13", "f14"),
  weight = c(1.454, 1.388))

1.454 - 1.388 = 0.066

f13 <- ggplot(d13, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(1.2,1.5) +
  theme_minimal()  


d14 <- tibble(
  fly = c("f14", "f15"),
  weight = c(1.388, 1.387))


1.388 - 1.387 = 0.001

f14 <- ggplot(d14, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() +      
  ylim(1.2,1.5) +
  theme_minimal()  



d15 <- tibble(
  fly = c("f15", "f13"),
  weight = c(1.387, 1.454))

1.454 - 1.387 = 0.067


f15 <- ggplot(d15, aes(x = fly, y = weight, group = 1)) +
  geom_point() +         
  geom_line() + 
  ylim(1.2,1.5) +
  theme_minimal()  


female_fly_5 <- f13 + f14 + f15 


female_fly_1 /
#  female_fly_2 /
  female_fly_3 /
  female_fly_4 /
  female_fly_5
