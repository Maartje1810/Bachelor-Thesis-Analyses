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
library(dplyr)

#Load data for analysis
ArousalData <- read_excel("ArousalData.xlsx")
view(ArousalData)

#Recode variables
ArousalData <- ArousalData %>%
  rename(
    Participant_ID = ID,
    Group = condition,
    Timepoint = measure
  )

ArousalData <- ArousalData %>%
  mutate(Group = dplyr::recode(Group, "control" = "Control"))

ArousalData$Participant_ID <- as.factor(ArousalData$Participant_ID)
ArousalData$Timepoint <- as.factor(ArousalData$Timepoint)
ArousalData$Group <- as.factor(ArousalData$Group)


levels(ArousalData$Group)

ArousalData$Group <- relevel(
  ArousalData$Group,
  ref = "Control" # change to your baseline condition
)

levels(ArousalData$Timepoint)

ArousalData$Timepoint <- factor(ArousalData$Timepoint,
                                levels = c(1, 2, 3, 4),
                                labels = c("T1", "T2", "T3", "T4"))

str(ArousalData)

#Linear Mixed Model (Group x Phase)
print("--- MODEL: Arousal (Group x Phase) ---")
m_Arousal <- lmer(Arousal ~ Group * Timepoint + (1|Participant_ID), 
                  data = ArousalData, REML = FALSE)
print(Anova(m_Arousal, type="II"))

#Checking model assumptions 

#1. Normality
qqnorm(resid(m_Arousal))
qqline(resid(m_Arousal))

#2. Distribution
hist(resid(m_Arousal), breaks = 30)

#3. Homoscedasticity
plot(fitted(m_Arousal), resid(m_Arousal))
abline(h = 0, lty = 2)


#Exploratory pairwise contrasts
print("--- PAIRWISE CONTRASTS (Group by Phase) ---")
emm_Arousal <- emmeans(m_Arousal, ~ Group | Timepoint)
print(pairs(emm_Arousal, adjust="none")) 

# Plot (Bar Chart + Individual Points)
p_Arousal <- ggplot(ArousalData, aes(x = Timepoint, y = Arousal, fill = Group)) +
  
  # Mean bars
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(0.8),
               width = 0.7,
               color = "black",
               alpha = 0.6) +
  
  # Individual points
  geom_point(position = position_jitterdodge(
    jitter.width = 0.15,
    dodge.width = 0.8),
    size = 1.5,
    alpha = 0.4,
    show.legend = FALSE) +
  
  # Standard error bars
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(0.8),
               color = "black") +
  
  scale_fill_manual(values = c(
    "Control" = "#999999",
    "2D" = "#E69F00",
    "VR" = "#56B4E9"
  )) +
  
  scale_x_discrete(labels = c(
    "T1" = "T1\nBaseline",
    "T2" = "T2\nAfter\nCognitive Tasks I",
    "T3" = "T3\nAfter\nIntervention",
    "T4" = "T4\nAfter\nCognitive Tasks II"
  )) +
  
  scale_y_continuous(limits = c(NA, 10)) +
  
  labs(
    x = NULL,
    y = "Arousal Score"
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      margin = margin(t = 8)
    )
  )

print(p_Arousal)