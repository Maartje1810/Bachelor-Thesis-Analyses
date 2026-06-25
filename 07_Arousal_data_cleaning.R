library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggdist)
library(cowplot)
library(readxl)
library(car)
library(effects)
library(afex)
library(readxl)


#1Prepare dataset
AffectGriddata <- read_excel("AffectGriddata.xlsx")
view(AffectGriddata)

#A Select variables of interest
AffectGrid_arousal <- AffectGriddata %>% select(ID, Q37, Q42, Q61, Q62) 

#B From wide format to long format 
arousal_long <- pivot_longer(
  AffectGrid_arousal,
  cols = c(Q37, Q42, Q61, Q62),
  names_to = "measure",
  values_to = "score"
)

#C Remove lines without data 
arousal_long <- arousal_long[-c(1:4), ]

#D Recode measure to numeric
arousal_long$measure <- dplyr::recode(arousal_long$measure,
                                      "Q37" = 1,
                                      "Q42" = 2,
                                      "Q61" = 3,
                                      "Q62" = 4)


#E Remove participants 
arousal_long_filtered <- arousal_long[ !arousal_long$ID %in% c(1,4,5,8,12,13,23,35,46,47,3510247), ]

#F add variable condition 

arousal_long_filtered <- arousal_long_filtered %>%
  mutate(condition = case_when(
    ID %in% c(3,14,19,24,25,26,28,36,39,42,43,44,48,53,55) ~ "VR",
    ID %in% c(7,10,11,13,16,17,20,22,27,29,32,34,38,40,49,52) ~ "2D",
    ID %in% c(2,6,9,15,18,21,30,31,33,37,41,45,50,51,54) ~ "control"
  ))

#G Final preparation 
arousal_long_filtered <- arousal_long_filtered %>%
  rename(Arousal = score)

arousal_long_filtered$Arousal <- as.numeric(sub("/.*", "", arousal_long_filtered$Arousal))