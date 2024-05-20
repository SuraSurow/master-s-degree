library(readxl)  
library(dplyr)   
library(writexl)  

# Wczytanie danych z pliku Excel
ankieta <- read_excel("ankieta.xlsx")

# Kodowanie zmiennych
nazwy_kolumn_do_zakodowania <- ankieta %>%
  mutate(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` =
           case_when(
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0,
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1,
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2,
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3
           ),
         `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` =
           case_when(
             `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?` == 'Tak' ~ 1,
             `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?` == 'Nie' ~ 0
           ),
         `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane` =
           case_when(
             `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?` == "Nie myślałam nad tym" ~ 0,
             `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?` == "Tak" ~ 1,
             `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?` == "Nie" ~ 0
           )
  )

# Wyodrębnienie kolumn zakodowanych
dane_po_kodowaniu_hip1_grupy <- nazwy_kolumn_do_zakodowania %>%
  select(ends_with("_kodowane"))

grupa_0 <- filter (
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 0
)

# Filtracja danych dla grup, które używają TikToka
grupa_1 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 1
)

grupa_2 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 2
)

grupa_3 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 3
)



# Test Kruskala-Wallisa
wynik_testu <- kruskal.test(
  dane_po_kodowaniu_hip1_grupy$`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` ~ 
    dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`,
  data = dane_po_kodowaniu_hip1_grupy
)

# Wyświetlenie wyników testu
print(wynik_testu)

# Tworzenie ramki danych do wykresu
dane_wykres <- data.frame(
  Grupa = c("A. nie korzysta z aplikacji","B. mniej niż godzina", "C. godzina do trzech godzin", "D. powyzej trzech godzin"),
  Liczba_Osob = c(nrow(grupa_0),nrow(grupa_1), nrow(grupa_2), nrow(grupa_3)),
  Liczba_Osob_Tak = c(
    sum(grupa_0$`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` == 1),
    sum(grupa_1$`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` == 1),
    sum(grupa_2$`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` == 1),
    sum(grupa_3$`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` == 1)
  )
)

# Obliczenie procentowego udziału mniejszego słupka w stosunku do całkowitej liczby osób
Procentowy_Udzial <- (dane_wykres$Liczba_Osob_Tak / dane_wykres$Liczba_Osob) * 100

# Tworzenie wykresu słupkowego
wykres_slupek <- ggplot(dane_wykres, aes(x = Grupa)) +
  geom_bar(aes(y = Liczba_Osob, fill = "Wszystkie Osoby"), stat = "identity", width = 0.5) +
  geom_bar(aes(y = Liczba_Osob_Tak, fill = "Osoby, które odpowiedziały tak"), stat = "identity", width = 0.5) +
  geom_text(aes(label = Liczba_Osob, y = Liczba_Osob), vjust = -0.5, size = 3, color = "black") +
  geom_text(aes(label = Liczba_Osob_Tak, y = Liczba_Osob_Tak), vjust = -0.5, size = 3, color = "black") +
  geom_text(x = 4, y = max(dane_wykres$Liczba_Osob), 
            label = paste("p-value =", format(wynik_testu$p.value, scientific = FALSE, digits = 2)), 
            vjust = 1.5, size = 3, color = "black") +
  scale_fill_manual(values = c("Wszystkie Osoby" = "lightblue", "Osoby, które odpowiedziały tak" = "orange")) +
  geom_text(aes(label = paste0(round(Procentowy_Udzial,2), "%"), y = Liczba_Osob_Tak-10), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Porównanie liczby osób, które porównują swój wygląd z influencerami w zależności od czasu spędzanego na TikToku",
       x = "Grupa",
       y = "Liczba osób",
       fill = "Legenda") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

print(wykres_slupek)



########################################################











####### DRUGA WYKRES













################################################

library(readxl)  
library(dplyr)   
library(writexl)  
library(ggplot2)

# Wczytanie danych z pliku Excel
ankieta <- read_excel("ankieta.xlsx")

# Kodowanie zmiennych
nazwy_kolumn_do_zakodowania <- ankieta %>%
  mutate(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` =
           case_when(
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0,
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1,
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2,
             `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3
           ),
         `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` =
           case_when(
             `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?` == 'Tak' ~ 1,
             `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?` == 'Nie' ~ 0
           ),
         `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane` =
           case_when(
             `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?` == "Nie myślałam nad tym" ~ 0,
             `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?` == "Tak" ~ 1,
             `19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?` == "Nie" ~ 0
           )
  )

# Wyodrębnienie kolumn zakodowanych
dane_po_kodowaniu_hip1_grupy <- nazwy_kolumn_do_zakodowania %>%
  select(ends_with("_kodowane"))

grupa_0 <- filter (
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 0
)

# Filtracja danych dla grup, które używają TikToka
grupa_1 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 1
)

grupa_2 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 2
)

grupa_3 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 3
)

# Test Kruskala-Wallisa
wynik_testu <- kruskal.test(
  dane_po_kodowaniu_hip1_grupy$`19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane`~ 
    dane_po_kodowaniu_hip1_grupy$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`,
  data = dane_po_kodowaniu_hip1_grupy
)

# Wyświetlenie wyników testu
print(wynik_testu)

# Tworzenie ramki danych do wykresu dla pytania 19
dane_wykres_19 <- data.frame(
  Grupa = c("A. nie korzysta z aplikacji","B. mniej niż godzina", "C. godzina do trzech godzin", "D. powyzej trzech godzin"),
  Liczba_Osob = c(nrow(grupa_0),nrow(grupa_1), nrow(grupa_2), nrow(grupa_3)),
  Liczba_Osob_Tak = c(
    sum(grupa_0$`19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane` == 1),
    sum(grupa_1$`19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane` == 1),
    sum(grupa_2$`19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane` == 1),
    sum(grupa_3$`19. Myślisz, że byłabyś bardziej zadowolona z własnego ciała, gdybyś wyglądała jak osoba, którą podziwiasz na TikToku ?_kodowane` == 1)
  )
)

Procentowy_Udzial_19 <- (dane_wykres_19$Liczba_Osob_Tak / dane_wykres_19$Liczba_Osob) * 100

wykres_slupek_19 <- ggplot(dane_wykres_19, aes(x = Grupa)) +
  geom_bar(aes(y = Liczba_Osob, fill = "Wszystkie Osoby"), stat = "identity", width = 0.5) +
  geom_bar(aes(y = Liczba_Osob_Tak, fill = "Osoby, które odpowiedziały tak"), stat = "identity", width = 0.5) +
  geom_text(aes(label = Liczba_Osob, y = Liczba_Osob), vjust = -0.5, size = 3, color = "black") +
  geom_text(aes(label = Liczba_Osob_Tak, y = Liczba_Osob_Tak), vjust = -0.5, size = 3, color = "black") +
  geom_text(x = 4, y = max(dane_wykres_19$Liczba_Osob),
            label = paste("p-value =", format(wynik_testu$p.value, scientific = FALSE, digits = 2)),
            vjust = 1.5, size = 3, color = "black") +
  scale_fill_manual(values = c("Wszystkie Osoby" = "lightblue", "Osoby, które odpowiedziały tak" = "orange")) +
  geom_text(aes(label = paste0(round(Procentowy_Udzial_19,2), "%"), y = Liczba_Osob_Tak-10), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Porównanie liczby osób, które chciałyby wyglądać jak osoby, które podziwiają w zależności od czasu spędzanego na TikToku",
       x = "Grupa",
       y = "Liczba osób",
       fill = "Legenda") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

print(wykres_slupek_19)
        




