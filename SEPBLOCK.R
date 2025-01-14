

## Combined Block Data Visualisation ####


## Code will allow one to see each of the plots
################## --
## OvoD1 Female ####


# 1:4 
ov1_egg_1_4 <- oviposition_results(onefour_ovi_of, boxplot_fill_colour = viridis_colors[c(9,9)])

# 4:1 
ov1_egg_4_1 <- oviposition_results(fourone_ovi_of, boxplot_fill_colour = viridis_colors[c(7,7)])


ov1_egg_combined <- oviposition_results(combined_ovi_of, boxplot_fill_colour = viridis_colors[c(9, 9, 7, 7)])
################ --

viridis_colors <- inferno(10)


## Using grid.arrange to put the plots together
ovod1_female_oviposition <- grid.arrange(ov1_egg_1_4, ov1_egg_4_1,
                                         nrow = 1,
                                         widths = c(0.5,0.5),
                                         heights = c(1))


ggsave(filename = "ovod1_female_oviposition.png", 
       plot = ovod1_female_oviposition, 
       width = 10, 
       height = 6, 
       dpi = 300)

ggsave(filename = "ov1_egg_combined.png", 
       plot = ov1_egg_combined, 
       width = 10, 
       height = 6, 
       dpi = 300)




############################# --

## Wild Type Virgin Female ####

# 1:4 
v1_egg_1_4  <- oviposition_results(onefour_ovi_vf, boxplot_fill_colour = viridis_colors[c(9,9)])

# 4:1 
v1_egg_4_1 <- oviposition_results(fourone_ovi_vf, boxplot_fill_colour = viridis_colors[c(7,7)])

# 4:1 and 1:4 
v1_egg_combined <- oviposition_results(combined_ovi_vf, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])






## Using grid arrange to put the plots together
virgin_female_oviposition <- grid.arrange(v1_egg_1_4, v1_egg_4_1 ,
                                          nrow = 1,
                                          widths = c(0.5,0.5),
                                          heights = c(1))




ggsave(filename = "v1_egg_combined.png", 
       plot = v1_egg_combined, 
       width = 10, 
       height = 6, 
       dpi = 300)

ggsave(filename = "virgin_female_oviposition.png", 
       plot = virgin_female_oviposition, 
       width = 10, 
       height = 6, 
       dpi = 300)




#################### --
## Wild type Male ####

# 1:4 
m_egg_1_4 <- oviposition_results(onefour_ovi_m, boxplot_fill_colour  = viridis_colors[c(9,9)])

# 4:1 
m_egg_4_1 <- oviposition_results(fourone_ovi_m, boxplot_fill_colour  = viridis_colors[c(7,7)])

# 1:4 and 4:1 
m_egg_combined <- oviposition_results(combined_ovi_m, boxplot_fill_colour  = viridis_colors[c(9,9,7,7)])





## Using grid arrange to put the plots together
male_oviposition <- grid.arrange(m_egg_1_4, m_egg_4_1,
                                 nrow = 1,
                                 widths = c(0.5,0.5),
                                 heights = c(1))



ggsave(filename = "male_oviposition.png", 
       plot = male_oviposition, 
       width = 10, 
       height = 6, 
       dpi = 300)



ggsave(filename = "m_egg_combined.png", 
       plot = m_egg_combined, 
       width = 10, 
       height = 6, 
       dpi = 300)




##### ALL THE PLOTS TOGETHER 
overall_oviposition <- grid.arrange(m_egg_1_4 + ggtitle("Male Conditioning"), m_egg_4_1 + ggtitle("Male Conditioning"), m_egg_combined + ggtitle("Male Conditioning"),
                                    ov1_egg_1_4 + ggtitle("OvoD1 Female Conditioning"), ov1_egg_4_1 + ggtitle("OvoD1 Female Conditioning"), ov1_egg_combined + ggtitle("OvoD1 Female Conditioning"),
                                    v1_egg_1_4 + ggtitle("Virgin Female Conditioning"), v1_egg_4_1 + ggtitle("Virgin Female Conditioning"), v1_egg_combined + ggtitle("Virgin Female Conditioning"),
                                    nrow = 3,
                                    widths = c(0.5,0.5,1),
                                    heights = c(1,1,1))









###### ###### Individual Block Data Visualisation ####
viridis_colours <- viridis(10)
## - 
############ --
## OvoD1 Female ####
############ --


## Visualising the individual blocks as data shows these to be significant ## 




# For 1:4 OvoD1

# Block 1 
ovod1_b1_onefour <- oviposition_results(one_four_b1_egg, boxplot_fill_colour = viridis_colours[1:2])

# Block 2
ovod1_b2_onefour <- oviposition_results(one_four_b2_egg, boxplot_fill_colour = viridis_colours[1:2])


# For 4:1 OvoD1

# Block 1 
ovod1_b1_fourone <- oviposition_results(four_one_b1_egg, boxplot_fill_colour = viridis_colours[3:4])

# Block 2
ovod1_b2_fourone <- oviposition_results(four_one_b2_egg, boxplot_fill_colour = viridis_colours[3:4])


## Combined 4:1 and 1:4 

# Block 1
ovod1_b1_combined <- oviposition_results(fourone_onefour_ovi_od1_b1 , boxplot_fill_colour = viridis_colours[c(1:4)])

# Block 2 
ovod1_b2_combined <- oviposition_results(fourone_onefour_ovi_od1_b2 ,  boxplot_fill_colour = viridis_colours[c(1:4)])



## Adding titles to the plots 
ovod1_b1_onefour <- ovod1_b1_onefour + ggtitle("OvoD1 Block 1")
ovod1_b1_fourone <- ovod1_b1_fourone + ggtitle("OvoD1 Block 1")
ovod1_b1_combined <- ovod1_b1_combined + ggtitle("OvoD1 Block 1")

ovod1_b2_onefour <- ovod1_b2_onefour + ggtitle("OvoD1 Block 2")
ovod1_b2_fourone <- ovod1_b2_fourone + ggtitle("OvoD1 Block 2")
ovod1_b2_combined <- ovod1_b2_combined + ggtitle("OvoD1 Block 2")




## OvoD1 Block plot 
ovod1_female_oviposition <- grid.arrange(
  ovod1_b1_onefour, ovod1_b1_fourone, ovod1_b1_combined,
  ovod1_b2_onefour, ovod1_b2_fourone, ovod1_b2_combined,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
)





############ --
## Virgin Female ####
############ --

## 4:1 Virgin 

# Block 2 
virgin_b2_fourone <- oviposition_results( fourone_ovi_vf_b2 , boxplot_fill_colour  = viridis_colours[3:4])

# Block 3 
virgin_b3_fourone <- oviposition_results( fourone_ovi_vf_b3, boxplot_fill_colour = viridis_colours[3:4])

# Block 4 
virgin_b4_fourone <- oviposition_results( fourone_ovi_vf_b4, boxplot_fill_colour = viridis_colours[3:4])



# 1:4 Virgin

# Block 2 
virgin_b2_onefour <- oviposition_results(onefour_ovi_vf_b2  , boxplot_fill_colour = viridis_colours[1:2])

# Block 3
virgin_b3_onefour <- oviposition_results(onefour_ovi_vf_b3 , boxplot_fill_colour  = viridis_colours[1:2])

# Block 4
virgin_b4_onefour <- oviposition_results(onefour_ovi_vf_b4 , boxplot_fill_colour  = viridis_colours[1:2])


## 4:1 + 1:4 Virgin 

# Block 2
virgin_b2_combined <- oviposition_results(fourone_onefour_ovi_vf_b2 , boxplot_fill_colour  = viridis_colours[c(1:4)])

# Block 3
virgin_b3_combined <- oviposition_results(fourone_onefour_ovi_vf_b3 , boxplot_fill_colour  = viridis_colours[c(1:4)])

# Block 4
virgin_b4_combined <- oviposition_results(fourone_onefour_ovi_vf_b4, boxplot_fill_colour  = viridis_colours[c(1:4)])



## virgin


v_b2_onefour <- virgin_b2_onefour + ggtitle("Virgin Block 2")
v_b2_fourone <- virgin_b2_fourone + ggtitle("Virgin Block 2")
v_b2_combined <- virgin_b2_combined + ggtitle("Virgin Block 2")

v_b3_onefour <- virgin_b3_onefour + ggtitle("Virgin Block 3")
v_b3_fourone <- virgin_b3_fourone + ggtitle("Virgin Block 3")
v_b3_combined <- virgin_b3_combined + ggtitle("Virgin Block 3")

v_b4_onefour <- virgin_b4_onefour + ggtitle("Virgin Block 4")
v_b4_fourone <- virgin_b4_fourone + ggtitle("Virgin Block 4")
v_b4_combined <- virgin_b4_combined + ggtitle("Virgin Block 4")


## this code works



virgin_female_oviposition <- grid.arrange(
  v_b2_onefour, v_b2_fourone, v_b2_combined,
  v_b3_onefour, v_b3_fourone, v_b3_combined,
  v_b4_onefour, v_b4_fourone, v_b4_combined,
  ncol = 3,
  nrow = 3,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1, 1)
)

############ --
## Male ####
############ -- 

## 1:4 

# Block 1
male_b1_onefour <- oviposition_results(onefour_ovi_m_b1, boxplot_fill_colour = viridis_colours[1:2])

# Block 2
male_b2_onefour <- oviposition_results(onefour_ovi_m_b2, boxplot_fill_colour = viridis_colours[1:2])

## 4:1

# Block 1 
male_b1_fourone <- oviposition_results(fourone_ovi_m_b1, boxplot_fill_colour = viridis_colours[3:4])

# Block 2 
male_b2_fourone <- oviposition_results(fourone_ovi_m_b2, boxplot_fill_colour = viridis_colours[3:4])


## 1:4 and 4:1

# Block 1 
male_b1_fourone_onefour <- oviposition_results(fourone_onefour_ovi_m_b1, boxplot_fill_colour = viridis_colours[1:4])

# Block 2
male_b2_fourone_onefour <- oviposition_results(fourone_onefour_ovi_m_b2, boxplot_fill_colour = viridis_colours[1:4])


## Adding titles to blocks 
male_b1_fourone <- male_b1_fourone + ggtitle("Male Block 1")
male_b2_fourone <- male_b2_fourone + ggtitle("Male Block 2")
male_b1_onefour <- male_b1_onefour + ggtitle("Male Block 1")
male_b2_onefour <- male_b2_onefour + ggtitle("Male Block 2")
male_b1_fourone_onefour <- male_b1_fourone_onefour + ggtitle("Male Block 1")
male_b2_fourone_onefour <- male_b2_fourone_onefour + ggtitle("Male Block 2")



# male 
male_oviposition <- grid.arrange(
  male_b1_fourone, male_b1_onefour, male_b1_fourone_onefour,
  male_b2_fourone, male_b2_onefour, male_b2_fourone_onefour,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
)













