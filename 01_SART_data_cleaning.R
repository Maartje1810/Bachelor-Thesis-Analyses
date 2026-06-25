# Load packages for analysis
library(rmarkdown)
library(knitr)
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
library(tinytex)
library(writexl)
```

```{r}
#Combine all participants files into one dataset
read_sart <- function(file, group, timepoint) {
  read_csv(file, col_types = cols(.default = "c")) %>%  # everything as character
    mutate(across(everything(), ~ na_if(.x, "None"))) %>%
    mutate(across(everything(), ~ na_if(.x, ""))) %>%
    filter(!is.na(digit_value)) %>%
    mutate(
      digit_value = as.numeric(digit_value),
      key_resp.rt = as.numeric(key_resp.rt),
      key_resp.duration = as.numeric(key_resp.duration),
      key_resp.corr = as.numeric(key_resp.corr),
      participant = as.numeric(str_extract(basename(file), "(?<=ID)\\d+")),
      group = group,
      timepoint = timepoint
    )
}

groups <- c("2D", "VR", "Control")
timepoints <- c("Baseline", "Post")

full_data <- map_df(groups, function(g) {
  map_df(timepoints, function(t) {
    
    files <- list.files(
      file.path("data", g, t),
      full.names = TRUE
    )
    
    map_df(files, ~ read_sart(.x, g, t))
  })
})

View(full_data)
```

```{r}
#Data cleaning 

#1 Remove first trial (trial 0) for each participant
full_data_clean<- full_data %>%
  filter(thisTrialN != 0)

#2Select variables of interest
full_data_clean <- full_data_clean %>%
  select(participant, group, timepoint, digit_value, key_resp.rt, key_resp.corr) 

#3 #Define errors 
full_data_clean <- full_data_clean %>%
  mutate(
    trial_type = if_else(digit_value == 3, "NoGo", "Go"),
    
    accuracy = case_when(
      trial_type == "Go" & key_resp.corr == 1 ~ 1,
      trial_type == "Go" & key_resp.corr == 0 ~ 0,
      trial_type == "NoGo" & key_resp.corr == 1 ~ 1,
      trial_type == "NoGo" & key_resp.corr == 0 ~ 0
    ),
    
    error_type = case_when(
      trial_type == "Go" & accuracy == 0 ~ "Omission",
      trial_type == "NoGo" & accuracy == 0 ~ "Commission",
      TRUE ~ "Correct"
    )
  )

#4 Only RT recorded for Go trials where button is pressed
full_data_clean <- full_data_clean %>%
  mutate(key_resp.rt = if_else(trial_type == "Go" & accuracy == 1, key_resp.rt, NA_real_))

#5 Remove RTs < 150ms for Go trials

full_data_clean <- full_data_clean %>%
  mutate(
    key_resp.rt = if_else(trial_type == "Go" & key_resp.rt < 0.150, NA_real_, key_resp.rt)
  )

#Checking how many trials are flagged as NA 

trials_per_participant <- full_data_clean %>%
  group_by(participant) %>%
  summarise(
    total_trials = n(),
    trials_flagged = sum(trial_type == "Go" & is.na(key_resp.rt)),
    percent_flagged = (trials_flagged / total_trials) * 100
  )

print(trials_per_participant, n = Inf)

rt_removed_by_condition <- full_data_clean %>%
  group_by(group) %>%
  summarise(
    total_go_trials = sum(trial_type == "Go"),
    trials_flagged = sum(trial_type == "Go" & is.na(key_resp.rt)),
    percent_flagged = (trials_flagged / total_go_trials) * 100
  )

print(rt_removed_by_condition, n = Inf)

rt_summary <- full_data_clean %>%
  group_by(participant, group) %>%
  summarise(
    total_go_trials = sum(trial_type == "Go"),
    trials_flagged = sum(trial_type == "Go" & is.na(key_resp.rt)),
    percent_flagged = (trials_flagged / total_go_trials) * 100
  )

model <- aov(percent_flagged ~ group, data = rt_summary)
summary(model)


#C From seconds to miliseconds
full_data_clean <- full_data_clean %>%
  mutate(key_resp.rt = key_resp.rt  * 1000)

#6 Create LogRT column
full_data_clean <- full_data_clean %>%
  mutate(LogRT = if_else(trial_type == "Go",
                         log10(key_resp.rt),
                         NA_real_))



#7 remove redunant columns 
full_data_clean <- full_data_clean %>%
  select(-digit_value)

full_data_clean <- full_data_clean %>%
  select(-key_resp.corr)

#B Rename columns 
full_data_clean <- full_data_clean %>%
  rename(
    Participant_ID = participant,
    RT = key_resp.rt,
    Group = group,
    Timepoint = timepoint
  )

#Inspect data
table(full_data_clean$Participant_ID)
length(unique(full_data_clean$Participant_ID))
table(full_data_clean$Group)


#8 Save as CSV
write.csv(full_data_clean, "SART_data", row.names = FALSE)

#9 Save as Excel
write_xlsx(full_data_clean, "SART_data.xlsx")

```