library(readxl)  
library(dplyr)   
library(writexl)  

dir.create("eat26_testB", showWarnings = FALSE)

ankieta <- read_excel("ankieta.xlsx")

nazwy_kolumn_ankieta <- colnames(ankieta)  
nazwy_kolumn_do_zakodowania <- nazwy_kolumn_ankieta[36:41]


kodowanieEAT26_testB <- ankieta %>%
  mutate(
    `A. Objadać się czując, że możesz nie być w stanie przestać?._kodowane` = 
      case_when(
        ankieta$`A. Objadać się czując, że możesz nie być w stanie przestać?` == "(1) - nigdy" ~ 0,
        ankieta$`A. Objadać się czując, że możesz nie być w stanie przestać?` == "(2) - 1 raz w miesiącu lub rzadziej" ~ 0,
        ankieta$`A. Objadać się czując, że możesz nie być w stanie przestać?` == "(3) - 2 do 3 razy w miesiącu" ~ 1,
        ankieta$`A. Objadać się czując, że możesz nie być w stanie przestać?` == "(4)- 1 raz w tygodniu" ~ 1,
        ankieta$`A. Objadać się czując, że możesz nie być w stanie przestać?` == "(5) - 2 do 6 razy w tygodniu" ~ 1,
        ankieta$`A. Objadać się czując, że możesz nie być w stanie przestać?` == "(6) - 1 raz dziennie lub więcej" ~ 1
      )
  )

kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(
    `B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?._kodowane` = 
      case_when(
        ankieta$`B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?` == "(1) - nigdy" ~ 0,
        ankieta$`B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?` == "(2) - 1 raz w miesiącu lub rzadziej" ~ 0,
        ankieta$`B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?` == "(3) - 2 do 3 razy w miesiącu" ~ 1,
        ankieta$`B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?` == "(4)- 1 raz w tygodniu" ~ 1,
        ankieta$`B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?` == "(5) - 2 do 6 razy w tygodniu" ~ 1,
        ankieta$`B. Wymiotować, aby w ten sposób wpłynąć na swoją wagę lub sylwetkę?` == "(6) - 1 raz dziennie lub więcej" ~ 1
      )
  )



kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(
    `C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?._kodowane` = 
      case_when(
        ankieta$`C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?` == "(1) - nigdy" ~ 0,
        ankieta$`C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?` == "(2) - 1 raz w miesiącu lub rzadziej" ~ 1,
        ankieta$`C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?` == "(3) - 2 do 3 razy w miesiącu" ~ 1,
        ankieta$`C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?` == "(4)- 1 raz w tygodniu" ~ 1,
        ankieta$`C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?` == "(5) - 2 do 6 razy w tygodniu" ~ 1,
        ankieta$`C. Stosować środki przeczyszczające, odchudzające lub moczopędne, aby kontrolować swoją wagę lub sylwetkę?` == "(6) - 1 raz dziennie lub więcej" ~ 1
      )
  )

kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(
    `D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?._kodowane` = 
      case_when(
        ankieta$`D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?` == "(1) - nigdy" ~ 0,
        ankieta$`D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?` == "(2) - 1 raz w miesiącu lub rzadziej" ~ 0,
        ankieta$`D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?` == "(3) - 2 do 3 razy w miesiącu" ~ 0,
        ankieta$`D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?` == "(4)- 1 raz w tygodniu" ~ 0,
        ankieta$`D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?` == "(5) - 2 do 6 razy w tygodniu" ~ 0,
        ankieta$`D. Ćwiczyć dłużej niż 60 minut dziennie, aby schudnąć lub kontrolować wagę?` == "(6) - 1 raz dziennie lub więcej" ~ 1
      )
  )

kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(
    `E. Schudnąć 10kg lub więcej?._kodowane` = 
      case_when(
        ankieta$`E. Schudnąć 10kg lub więcej?` == "(1) - Tak" ~ 1,
        ankieta$`E. Schudnąć 10kg lub więcej?` == "(2) - Nie" ~ 0
      )
  )

kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(
    `F. Czy kiedykolwiek byłeś leczony z powodu zaburzeń odżywiania?._kodowane` = 
      case_when(
        ankieta$`F. Czy kiedykolwiek byłeś leczony z powodu zaburzeń odżywiania?` == "(1) - Tak" ~ 1,
        ankieta$`F. Czy kiedykolwiek byłeś leczony z powodu zaburzeń odżywiania?` == "(2) - Nie" ~ 0
      )
  )


kodowanieEAT26_testB %>%
  select(ends_with("_kodowane")) %>%
  write_xlsx("eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")


kodowanieEAT26_testB <- read_excel("eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")


nazwy_kolumn_do_sumowania <- colnames(kodowanieEAT26_testB)


kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(Suma_1_do_6 = rowSums(select(., all_of(nazwy_kolumn_do_sumowania)))) %>%
  mutate(Interpretacja = ifelse(Suma_1_do_6 >= 1, 1, 0))
  

suma_interpretacja <- sum(kodowanieEAT26_testB$Interpretacja, na.rm = TRUE)
  
 

kodowanieEAT26_testB %>%
  add_row(Suma_1_do_6 = NA, Interpretacja = suma_interpretacja) %>%
  write_xlsx("eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")

kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  mutate(ID = row_number())


kodowanieEAT26_testB %>%
  write_xlsx("eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")

#wynik który mówi nam o tym że ktoś jest w grupie ryzyka jest 1 badz wiecej dla sumy 6 pytań
# wyszlo 63 osoby na 130
# Załaduj dane z Excela
kodowanieEAT26_testB <- read_excel("eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")

# Oblicz sumę dla kolumny 'Interpretacja'
suma_interpretacja <- sum(kodowanieEAT26_testB$Interpretacja, na.rm = TRUE)

# Dodaj nowy wiersz z obliczoną sumą dla 'Interpretacja'
kodowanieEAT26_testB <- kodowanieEAT26_testB %>%
  add_row(Suma_1_do_6 = NA, Interpretacja = suma_interpretacja, ID = NA) # Dodaje wiersz podsumowujący

# Zapisz zmodyfikowaną tabelę do pliku Excel
write_xlsx(kodowanieEAT26_testB, "eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")
