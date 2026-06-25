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

FFMQ <- read_excel("FFMQ_final.xlsx")


FFMQ <- FFMQ %>% rename(Total= `FFMQ Total`)

view(FFMQ)

#Check data types

FFMQ$ID <- as.factor(FFMQ$ID)

FFMQ$condition <- as.factor(FFMQ$condition)

levels(FFMQ$condition)

FFMQ$condition <- relevel(
  FFMQ$condition,
  ref = "control"   # change to your baseline condition
)

str(FFMQ)

#One way anova
scales <- c("Total", "Observing", "Describing", "Acting", "Nonjudging", "Nonreactivity")

for (s in scales) {
  cat("\n---", s, "---\n")
  model <- aov(as.formula(paste(s, "~ condition")), data = FFMQ)
  print(summary(model))
}
