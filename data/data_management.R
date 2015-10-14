# Detta skript kan användas för att plocka ut variabler och göra urval etc från data som annars görs mha mallverktygen i INCA
# sedna sparas data ner för att kunna hanteras lite smidigare.



df <- read.csv2("~/Documents/huvud_hals/atal_blanketter/data/df.csv")

library(dplyr)

df <- df %>%
    filter(
        REGION_NAMN != "Region Demo",
        a_icd10_Värde != "00"
    )

save(df, file = "~/Documents/huvud_hals/atal_blanketter/data/df.rda")
