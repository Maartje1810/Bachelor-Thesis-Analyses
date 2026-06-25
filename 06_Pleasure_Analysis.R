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

install.packages("ggsignif")
library(ggsignif)

#Load data for analysis
PleasureData <- read_excel("pleasure.xlsx")
view(PleasureData)

#Recode variables
PleasureData <- PleasureData %>%
  rename(
    Participant_ID = ID,
    Group = condition,
    Timepoint = measure
  )

PleasureData <- PleasureData %>%
  mutate(Group = dplyr::recode(Group, "control" = "Control"))

PleasureData$Participant_ID <- as.factor(PleasureData$Participant_ID)
PleasureData$Timepoint <- as.factor(PleasureData$Timepoint)
PleasureData$Group <- as.factor(PleasureData$Group)

levels(PleasureData$Group)

PleasureData$Group <- relevel(
  PleasureData$Group,
  ref = "Control" 
)

levels(PleasureData$Timepoint)

PleasureData$Timepoint <- factor(PleasureData$Timepoint,
                                 levels = c("1", "2", "3", "4"))

PleasureData$Timepoint <- factor(PleasureData$Timepoint,
                                 levels = c(1, 2, 3, 4),
                                 labels = c("T1", "T2", "T3", "T4"))

str(PleasureData)

View(PleasureData)


#Linear Mixed Model (Group x Phase)
print("--- MODEL: Pleasure (Group x Phase) ---")
m_Pleasure <- lmer(Pleasure ~ Group * Timepoint + (1|Participant_ID), 
                   data = PleasureData, REML = FALSE)
print(Anova(m_Pleasure, type="II"))

#Checking model assumptions 

#1. Normality
qqnorm(resid(m_Pleasure))
qqline(resid(m_Pleasure))

#2. Distribution
hist(resid(m_Pleasure), breaks = 30)

#3. Homoscedasticity
plot(fitted(m_Pleasure), resid(m_Pleasure))
abline(h = 0, lty = 2)

#Exploratory pairwise contrasts
print("--- PAIRWISE CONTRASTS (Pleasure by Phase) ---")
emm_Pleasure <- emmeans(m_Pleasure, ~ Group | Timepoint)
print(pairs(emm_Pleasure, adjust="none")) 


# 5. Plot (Bar Chart + Individual Points)
p_Pleasure <- ggplot(PleasureData, aes(x = Timepoint, y = Pleasure, fill = Group)) +
  
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
    y = "Pleasure Score"
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = "top",
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      margin = margin(t = 8)
    )
  )

print(p_Pleasure)
