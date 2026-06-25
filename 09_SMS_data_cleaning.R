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
library(tidyr)
library(dplyr)
library(stringr)

#Load Data
DataSMS <- read_excel("DataSMS.xlsx")
view(DataSMS)

#Remove first row
DataSMS <- DataSMS[-1, ]
head(DataSMS)
view(DataSMS)


DataSMS_selected <- DataSMS[, c(1, 86:100)]

#From wide to long format

DataSMS_long <- DataSMS_selected %>%
  pivot_longer(
    cols = 2:16,
    names_to = "measure",
    values_to = "score"
  )

DataSMS_long <- DataSMS_long %>%
  mutate(ID = as.numeric(trimws(ID)))

DataSMS_long <- DataSMS_long %>%
  mutate(condition = case_when(
    ID %in% c(3,14,19,24,25,26,28,36,39,42,43,44,48,53,55) ~ "VR",
    ID %in% c(7,10,11,13,16,17,20,22,27,29,32,34,38,40,49,52) ~ "2D",
    ID %in% c(2,6,9,15,18,21,30,31,33,37,41,45,50,51,54) ~ "control",
    TRUE ~ NA_character_
  ))

View(DataSMS_long)

DataSMS_clean <- DataSMS_long %>%
  # Extract timepoint (T1–T4)
  mutate(time = str_extract(measure, "T[1-4]")) %>%
  
  mutate(type = str_extract(measure, "Total|Mind|Body")) %>%
  
  # Remove rows without timepoints (i.e., overall totals)
  filter(!is.na(time)) %>%
  
  # Reshape to wide format
  pivot_wider(
    names_from = type,
    values_from = score,
    names_prefix = "SMS_"
  ) %>%
  
  rename(
    SMS_Total = SMS_Total,
    SMS_Mind  = SMS_Mind,
    SMS_Body  = SMS_Body
  )

DataSMS_final <- DataSMS_clean %>%
  group_by(ID, condition, time) %>%
  summarise(
    Total = sum(SMS_Total, na.rm = TRUE),
    Mind  = sum(SMS_Mind, na.rm = TRUE),
    Body  = sum(SMS_Body, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(measure = time)


DataSMS_long$score <- as.numeric(DataSMS_long$score)

DataSMS_long$condition <- as.factor(DataSMS_long$condition)

table(DataSMS_final$ID)
length(unique(DataSMS_final$ID))
table(DataSMS_final$condition)

install.packages("writexl")

# Save as CSV
write.csv(DataSMS_final, "DataSMS_final.csv", row.names = FALSE)

# Save as Excel
library(writexl)
write_xlsx(DataSMS_final, "DataSMS_final.xlsx")