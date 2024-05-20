
library(readxl)  
library(dplyr)   


library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(lawstat)
library(PMCMRplus)
library(writexl)  
#'''
#''' Młode
#' 
#' 
#' kobiety większość czasu na TikToku spędzają na przeglądaniu 
#' treści związanych z odżywianiem i wyglądem zewnętrznym, które mają szkodliwy 
#' wpływ na ich zdrowie psychiczne oraz wybory żywieniowe.#'''


dir.create("Hipoteza3/Kodowanie", showWarnings = FALSE)

ankieta <- read_excel("../ankieta.xlsx")

nazwy_kolumn_do_zakodowania <- ankieta %>%
  mutate( `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane`=
            case_when(
              `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Nie oglądam tego typu treści'~ 0 ,
              `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Raz w tygodniu' ~ 1,
              `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Codziennie'~ 3,
              `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Kilka razy w tygodniu' ~ 2
            ),
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` = 
      case_when(
        `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0 ,
        `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1 ,
        `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2 ,
        `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3 ,
        
      )
         
  )




uzytkownicyTiktoka <- nazwy_kolumn_do_zakodowania %>%
  filter( `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`!= 0)

dane_po_kodowaniu_hip1_grupy <- uzytkownicyTiktoka %>%
  select(ends_with("_kodowane"))

grupa_0 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` == 0

grupa_1 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` == 1
)

grupa_2 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` == 2
)
#powyzej 3 godzin
grupa_3 <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` == 3
)

srednia1 <- mean(grupa_0$)



t_samoocena <- t.test(
  grupa_Ogladaczy$`5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?_kodowane`,
  grupa_NieOgladaczy$`5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?_kodowane`,
  alternative = "two.sided"
)

print(t_samoocena)

# Obliczenie liczby osób z grupy, które mają złą samoocenę
liczba_osob_zla_samoocena <- sum(grupa_Ogladaczy$`5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?_kodowane` == 1)
procent_zla_samoocena <- liczba_osob_zla_samoocena / nrow(grupa_Ogladaczy) * 100
liczba_osob_zla_samoocena_nieogl <- sum(grupa_NieOgladaczy$`5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?_kodowane` == 1)

# Tworzenie ramki danych do wykresu
dane_wykres <- data.frame(
  Grupa = c("Grupa Oglądających", "Grupa Nie Oglądających"),
  Liczba_Osob = c(nrow(grupa_Ogladaczy), nrow(grupa_NieOgladaczy)),
  Zla_Samoocena = c(liczba_osob_zla_samoocena, liczba_osob_zla_samoocena_nieogl),
  Procent_Zla_Samoocena = c(liczba_osob_zla_samoocena / nrow(grupa_Ogladaczy) * 100, liczba_osob_zla_samoocena_nieogl / nrow(grupa_NieOgladaczy) * 100)
)

wykres_slupek <- ggplot(dane_wykres, aes(x = Grupa)) +
  geom_bar(aes(y = Liczba_Osob, fill = "Wszystkie Osoby"), stat = "identity", width = 0.5) +
  geom_bar(aes(y = Zla_Samoocena, fill = "Osoby które odczuły wpływ porównywania się na samoocene"), stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Wszystkie Osoby" = "lightblue", "Osoby które odczuły wpływ porównywania się na samoocene" = "orange")) +
  geom_text(aes(label = Liczba_Osob, y = Liczba_Osob), vjust = -0.5, size = 3, color = "black") +
  geom_text(aes(label = Zla_Samoocena, y = Zla_Samoocena), vjust = -0.5, size = 3, color = "black") +
  geom_text(aes(label = paste(round(Procent_Zla_Samoocena, 2), "%"), y = Zla_Samoocena - 10), vjust = -0.5, size = 3, color = "black") +
  geom_text(x = 1.5, y = max(dane_wykres$Liczba_Osob), label = paste("p-value =", format(t_samoocena$p.value, digits = 4)), vjust = 1.5, size = 3, color = "black") +
  labs(title = "Odsetek osób, które odczuły wpływ porównywania się do influencerów na swoją samoocenę,\n w zestawieniu z ogólną liczbą osób w badanych grupach.",
       x = "Grupa",
       y = "Liczba osób",
       fill = "Legenda") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))  # Poprawiona linijka z legendą

print(wykres_slupek)

print(grupa_NieOgladaczy,n=40)