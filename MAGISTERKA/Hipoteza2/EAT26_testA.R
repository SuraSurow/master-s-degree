library(readxl)  
library(dplyr)   
library(writexl)  

dir.create("eat26_testA", showWarnings = FALSE)

ankieta <- read_excel("ankieta.xlsx")

nazwy_kolumn_ankieta <- colnames(ankieta)  
nazwy_kolumn_do_zakodowania <- nazwy_kolumn_ankieta[9:(9 + 25)]



kodowanieEAT26_testA <- kodowanieEAT26_testA %>%
  mutate(`26.Próbowanie nowych potraw sprawia mi przyjemność._kodowane` = 
           case_when(
             `26.Próbowanie nowych potraw sprawia mi przyjemność.` >= 4 ~ 0,
             `26.Próbowanie nowych potraw sprawia mi przyjemność.` == 3 ~ 1,
             `26.Próbowanie nowych potraw sprawia mi przyjemność.` == 2 ~ 2,
             `26.Próbowanie nowych potraw sprawia mi przyjemność.` == 1 ~ 3
           )
  )

dane_po_kodowaniu_eat26_testA <- kodowanieEAT26_testA %>%
  select(ends_with("_kodowane"))

write_xlsx(dane_po_kodowaniu_eat26_testA, "eat26_testA/ankieta_kodowanieEAT26_testA.xlsx")

kodowanieEAT26_testA <- read_excel("eat26_testA/ankieta_kodowanieEAT26_testA.xlsx")

nazwy_kolumn_do_sumowania <- colnames(kodowanieEAT26_testA)[1:26]

kodowanieEAT26_testA <- kodowanieEAT26_testA %>%
  mutate(Suma_1_do_26 = rowSums(select(., all_of(nazwy_kolumn_do_sumowania)))) %>%
  mutate(Interpretacja = ifelse(Suma_1_do_26 > 20, 1, 0))

suma_interpretacja <- sum(kodowanieEAT26_testA$Interpretacja, na.rm = TRUE)

kodowanieEAT26_testA <- kodowanieEAT26_testA %>%
  add_row(Interpretacja = suma_interpretacja)

write_xlsx(kodowanieEAT26_testA, "eat26_testA/ankieta_kodowanieEAT26_testA.xlsx")

kodowanieEAT26_testA <- kodowanieEAT26_testA %>%
  mutate(ID = row_number())


kodowanieEAT26_testA %>%
  write_xlsx("eat26_testA/ankieta_kodowanieEAT26_testA.xlsx")
