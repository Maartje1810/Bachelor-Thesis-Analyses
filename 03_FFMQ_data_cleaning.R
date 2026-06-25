if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lme4)) install.packages("lme4")
if(!require(lmerTest)) install.packages("lmerTest")
if(!require(emmeans)) install.packages("emmeans")
if(!require(ggdist)) install.packages("ggdist")
if(!require(cowplot)) install.packages("cowplot")
if(!require(readxl)) install.packages("readxl")
if(!require(car)) install.packages("car")
if(!require(effects)) install.packages("effects")
if(!require(afex)) install.packages("afex")
install.packages("readxl")

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

FFMQdata <- read_excel("FFMQdata.xlsx")
view(FFMQdata)

FFMQdata <- FFMQdata[-1, ]
head(FFMQdata)
view(FFMQdata)


FFMQdata_selected<- FFMQdata[, c(1, 41:46)]

FFMQdata_long <- FFMQdata_selected %>%
  pivot_longer(
    cols = -1,                 
    names_to = "variable",      
    values_to = "score"         
  )

FFMQdata_long <- FFMQdata_long%>%
  mutate(ID = as.numeric(trimws(ID)))

FFMQdata_long <- FFMQdata_long %>%
  mutate(condition = case_when(
    ID %in% c(3,14,19,24,25,26,28,36,39,42,43,44,48,53,55) ~ "VR",
    ID %in% c(7,10,11,13,16,17,20,22,27,29,32,34,38,40,49,52) ~ "2D",
    ID %in% c(2,6,9,15,18,21,30,31,33,37,41,45,50,51,54) ~ "control",
    TRUE ~ NA_character_
  ))

View(FFMQdata_long)

FFMQ_final <- FFMQdata_long %>%
  pivot_wider(
    names_from = variable,
    values_from = score
  )

table(FFMQ_final$ID)
length(unique(FFMQ_final$ID))
table(FFMQ_final$condition)
FFMQ_final$ID <- as.factor(FFMQ_final$ID)

install.packages("writexl")
library(writexl)

write.csv(FFMQ_final, "FFMQ_final.csv", row.names = FALSE)


write_xlsx(FFMQ_final, "FFMQ_final.xlsx")

