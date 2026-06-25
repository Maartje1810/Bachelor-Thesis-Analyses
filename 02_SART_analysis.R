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

#Load data
SARTdata <- read_excel("SART_data.xlsx")
view(SARTdata)

#Prepare variables 
SARTdata$Participant_ID <- as.factor(SARTdata$Participant_ID)
SARTdata$Group <- as.factor(SARTdata$Group)
SARTdata$Group <- relevel(SARTdata$Group, ref = "Control")
SARTdata$Timepoint <- as.factor(SARTdata$Timepoint)
SARTdata$Timepoint <- relevel(SARTdata$Timepoint, ref = "Baseline")
SARTdata$trial_type <- as.factor(SARTdata$trial_type)
SARTdata$error_type <- as.factor(SARTdata$error_type)
SARTdata$accuracy <- as.numeric(as.character(SARTdata$accuracy))

str(SARTdata)
table(SARTdata$accuracy)


#Filter dataset for LMER
SARTdata_lmer <- SARTdata %>%
  filter(trial_type == "Go", RT > 0, !is.na(LogRT))

table(SARTdata_lmer$trial_type)

summary(SARTdata_lmer$LogRT)

#LMER model (reaction time)
lmer_model <- lmer(LogRT ~ Group * Timepoint + (1 | Participant_ID),
                   data = SARTdata_lmer)

summary(lmer_model)

#Checking model assumptions 

#1. Normality
qqnorm(resid(lmer_model))
qqline(resid(lmer_model))

#2. Distribution
hist(resid(lmer_model), breaks = 30)

#3. Homoscedasticity
plot(fitted(lmer_model), resid(lmer_model))
abline(h = 0, lty = 2)


#GLMM model (response inhibition)

glmm_model <- glmer(accuracy ~ Group * Timepoint + (1 | Participant_ID),
                    data = SARTdata,
                    family = binomial,
                    subset = (trial_type == "NoGo"))

summary(glmm_model)

#GLMM model on omission errors (Missed on a GO trial)
SARTdata$omission <- ifelse(SARTdata$error_type == "Omission", 1, 0)

omission_model <- glmer(omission ~ Group * Timepoint + (1 | Participant_ID),
                        data = SARTdata,
                        family = binomial,
                        subset = (trial_type == "Go"))

summary(omission_model)
```

```{r}
#Exploratory Pairwise Comparisons - RT

#Changes over time within each group 
emmeans(lmer_model, pairwise ~ Timepoint | Group)

#Group differences at each timepoint
emmeans(lmer_model, pairwise ~ Group | Timepoint)

#Graph of RTs for correct Go Trials

custom_colors <- c(
  "Control" = "#4E79A7",
  "2D"      = "#F28E2B",
  "VR"      = "#59A14F"
)

# RT only (RAW ms)
df_rt_plot <- SARTdata %>%
  filter(trial_type == "Go",
         RT > 0,
         !is.na(RT))

p_rt <- ggplot(df_rt_plot,
               aes(x = Timepoint,
                   y = RT,
                   fill = Group,
                   color = Group)) +
  
  stat_halfeye(
    adjust = .5,
    width = .5,
    .width = 0,
    justification = -.2,
    alpha = 0.6,
    position = position_dodge(width = 0.6)
  ) +
  
  geom_boxplot(
    width = .12,
    outlier.shape = NA,
    alpha = 0.5,
    position = position_dodge(width = 0.6)
  ) +
  
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  
  labs(
    title = NULL,
    y = "Reaction Time (ms)",
    x = "Timepoint",
    fill = "Group",
    color = "Group"
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

print(p_rt)
