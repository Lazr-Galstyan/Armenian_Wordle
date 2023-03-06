library(utils)
library(readxl)
library(stringr)
library(dplyr)

# df <- read_excel("~/Wordle_ARM/Output_All_UPD_5l_w_imp.xlsx")
df <- read_excel("Output_All_UPD_5l_w_imp.xlsx")


cleaned_df <- df %>%
  mutate(count = str_count(name, "ու")) %>%
  filter((count == 2 & nchar(name) == 6) == FALSE) %>%
  mutate(name = str_replace(name, "ու", "ւ"))





words_common_5 <- cleaned_df[cleaned_df['Important'] == 1,]$name

words_all_5 <- cleaned_df$name


# Save the final data
write.csv(cleaned_df, "Output_All_UPD_F.csv", row.names=TRUE)

