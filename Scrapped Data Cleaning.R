library(utils)
library(readxl)

# Select the words with 5 letters
# Note: letter "ու" consists of two characters even though it is 1 letter
# Hence, make sure we do not include words that have 4 letters but 5 characters
df_all_upd <- read.csv("Output_All_UPD1.csv")

df_all_upd_5l <- df_all_upd %>%
  mutate(characters = ifelse(str_detect(name, "ու") == TRUE,nchar(name) - 1, nchar(name))) %>%
  filter(characters == 5) %>%
  distinct()




write.csv(df_all_upd_5l, "Output_All_UPD_5l.csv", row.names=TRUE)
