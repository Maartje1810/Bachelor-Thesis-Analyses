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

AffectGriddata <- read_excel("AffectGriddata.xlsx")

view(AffectGriddata)

AffectGrid_pleasure <- AffectGriddata %>% select(ID, Q37, Q42, Q61, Q62) 

view(AffectGrid_pleasure)

AffectGrid_pleasure %>%
  pivot_longer(cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
               names_to = "variable", values_to = "score", values_drop_na = TRUE)
library(tidyr)

pleasure_long <- pivot_longer(
  AffectGrid_pleasure,
  cols = c(Q37, Q42, Q61, Q62),
  names_to = "measure",
  values_to = "score"
)

view(pleasure_long)

pleasure_long <- pleasure_long[-c(1:4), ]

pleasure_long$measure <- recode(pleasure_long$measure,
                                "Q37" = 1,
                                "Q42" = 2,
                                "Q61" = 3,
                                "Q62" = 4)

class(pleasure_long) 

pleasure_long$condition <- ifelse(
  pleasure_long$participant %in% c(1,4), "VR",
  ifelse(pleasure_long$participant %in% c(2), "2D", "control")
)

pleasure_long_filtered <- pleasure_long[ !pleasure_long$ID %in% c(1,4,5,8,12,13,23,35,46,47), ]



pleasure <- pleasure_long_filtered %>%
  mutate(condition = case_when(
    ID %in% c(3,14,19,24,25,26,28,36,39,42,43,44,48,53,55) ~ "VR",
    ID %in% c(7,10,11,13,16,17,20,22,27,29,32,34,38,40,49,52) ~ "2D",
    ID %in% c(2,6,9,15,18,21,30,31,33,37,41,45,50,51,54) ~ "control"
  ))

pleasure_final <- pleasure[ !pleasure$ID %in% c(3510247), ]

view(pleasure)

table(pleasure_final$ID)

length(unique(pleasure_final$ID))

table(pleasure_final$condition)

pleasure_final$ID <- as.factor(pleasure_final$ID)

pleasure_final <- pleasure_final %>%
  rename(Pleasure = score)

pleasure_final$Pleasure <- as.numeric(sub("/.*", "", pleasure_final$Pleasure))


view(pleasure_final)


install.packages("writexl")

# Save as CSV
write.csv(pleasure_final, "pleasure.csv", row.names = FALSE)

# Save as Excel
library(writexl)
write_xlsx(pleasure_final, "pleasure.xlsx")