library(tidyverse)

# Zadanie 1
wesbrook_dataset <- read_csv("http://jolej.linuxpl.info/Wesbrook.csv",
                             col_types = "ificifffffffffffffinnnnniifnnnn")

glimpse(wesbrook_dataset)

wesbrook_dataset <- wesbrook_dataset %>%
  mutate(INDUPDT = mdy(INDUPDT))

glimpse(wesbrook_dataset)

# Zadanie 2
summary(wesbrook_dataset)


# Zadanie 3
# Checking for missing values
wesbrook_dataset <- subset(
  wesbrook_dataset,
  !is.na("MOV_DWEL") &
    !is.na("HH_1PER") &
    !is.na("HH_2PER"))

# Deleting columns we deemed unnecessary
wesbrook_dataset <- subset(wesbrook_dataset,
                           select = -c(ID, GRADYR1,
                                       FACULTY1, DEPT1,
                                       MAJOR1, FRSTYEAR, BIGBLOCK, EA, INDUPDT))
summary(wesbrook_dataset)