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


#load data for analysis
SMS <- read_excel("DataSMS_final.xlsx")
view(SMS)

#1 Prepare dataset

#recode variables

SMS <- SMS %>%
  rename(
    Participant_ID = ID,
    Group = condition,
    Timepoint = measure
  )

SMS <- SMS %>%
  mutate(Group = dplyr::recode(Group, "control" = "Control"))


SMS$Participant_ID <- as.factor(SMS$Participant_ID)

SMS$Timepoint <- as.factor(SMS$Timepoint)

SMS$Group <- as.factor(SMS$Group)

levels(SMS$Group)

SMS$Group <- relevel(
  SMS$Group,
  ref = "Control"   # change to your baseline condition
)

SMS$Group<- factor(SMS$Group, 
                   levels = c("Control", "2D", "VR"))

levels(SMS$Timepoint)

SMS$Timepoint <- factor(SMS$Timepoint, 
                        levels = c("T1", "T2", "T3", "T4"))

SMS$Timepoint <- relevel(
  SMS$Timepoint,
  ref = "T1"   # change to your baseline condition
)

str(SMS)

#Linear Mixed Model (Grpup x Phase)

print("--- MODEL: Total SMS (Group x Phase) ---")
m_total <- lmer(Total ~ Group * Timepoint + (1|Participant_ID), 
                data = SMS, REML = FALSE)
print(Anova(m_total, type="II"))

print("--- MODEL: Mind SMS (Group x Phase) ---")
m_mind <- lmer(Mind ~ Group * Timepoint + (1|Participant_ID), 
               data = SMS, REML = FALSE)
print(Anova(m_mind, type="II"))

print("--- MODEL: Body SMS (Group x Phase) ---")
m_Body <- lmer(Body ~ Group * Timepoint + (1|Participant_ID), 
               data = SMS, REML = FALSE)
print(Anova(m_Body, type="II"))

#Checking model assumptions Mind subscale 

#1.Normality
qqnorm(resid(m_mind))
qqline(resid(m_mind))

#2.Distribution
hist(resid(m_mind), breaks = 30)

#3. Homoscedasticity
plot(fitted(m_mind), resid(m_mind))
abline(h = 0, lty = 2)

#Checking model assumptions Body subscale 

#1. Normality
qqnorm(resid(m_Body))
qqline(resid(m_Body))

#2. Distribution
hist(resid(m_Body), breaks = 30)

#3. Homoscedasticity
plot(fitted(m_Body), resid(m_Body))
abline(h = 0, lty = 2)

#Exploratory Pairwise Contrasts

print("--- PAIRWISE CONTRASTS (Total by Phase) ---")
emm_total <- emmeans(m_total, ~ Group | Timepoint)
print(pairs(emm_total, adjust="none")) 

print("--- PAIRWISE CONTRASTS (Mind by Phase) ---")
emm_mind <- emmeans(m_mind, ~ Group | Timepoint)
print(pairs(emm_mind, adjust="none")) 

print("--- PAIRWISE CONTRASTS (Body by Phase) ---")
emm_body <- emmeans(m_Body, ~ Group | Timepoint)
print(pairs(emm_body, adjust="none")) 

# Plot of Mind Subscale (Bar Chart + Individual Points)
p_Mind <- ggplot(SMS, aes(x=Timepoint, y=Mind, fill=Group)) +
  
  # A. The Bars (Mean) - Semi-transparent so points pop out
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.8), 
               width = 0.7, color="black", alpha=0.6) +
  
  # B. The Individual Points (Jittered & Dodged to match bars)
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
             size = 1.5, alpha = 0.4, show.legend = FALSE) +
  
  # C. The Error Bars (SE)
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(0.8), color="black") +
  
  scale_fill_manual(
    name = "Group:",
    values = c(
    "Control" = "#7A7A7A",  
    "2D"      = "#CC79A7",  
    "VR"      = "#009E73"   
  ))+
  
  scale_x_discrete(labels = c(
    "T1" = "T1\nBaseline",
    "T2" = "T2\nAfter\nCognitive Tasks I",
    "T3" = "T3\nAfter\nIntervention",
    "T4" = "T4\nAfter\nCognitive Tasks II"
  )) +
  
  labs(title=NULL, 
       y="Mind Score", x=NULL) +
  theme_classic() +
  theme(legend.position="top", axis.text.x = element_text(angle=0, hjust=0.5))

print(p_Mind)

ggsave(
  filename = "Mind_Figure2.png",
  plot = p_Mind,
  width = 13,
  height = 8,
  units = "cm",
  dpi = 300
)

#Plot of Body Subscale
p_Body <- ggplot(SMS, aes(x=Timepoint, y=Body, fill=Group)) +
  
  # A. The Bars (Mean) - Semi-transparent so points pop out
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.8), 
               width = 0.7, color="black", alpha=0.6) +
  
  # B. The Individual Points (Jittered & Dodged to match bars)
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
             size = 1.5, alpha = 0.4, show.legend = FALSE) +
  
  # C. The Error Bars (SE)
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(0.8), color="black") +
  
  scale_fill_manual(values = c(
    "Control" = "#7A7A7A",  
    "2D"      = "#CC79A7",  
    "VR"      = "#009E73"   
  ))+
  
  scale_x_discrete(labels = c(
    "T1" = "T1\nBaseline",
    "T2" = "T2\nAfter\nCognitive Tasks I",
    "T3" = "T3\nAfter\nIntervention",
    "T4" = "T4\nAfter\nCognitive Tasks II"
  )) +
  
  labs(title=NULL, 
       y="Body Score", x=NULL) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_text(angle=0, hjust=0.5))

print(p_Body)
