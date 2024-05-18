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
#1) Treści na TikToku  prowadzą do niezadowolenia z kształtu, 
#wyglądu ciała oraz obniżenia samooceny'''


dir.create("Hipoteza1/Kodowanie", showWarnings = FALSE)

ankieta <- read_excel("./ankieta.xlsx")

nazwy_kolumn_do_zakodowania <- ankieta %>%
  mutate(`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane`=
           case_when(
             `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Nie oglądam tego typu treści'~ 0 ,
             `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Raz w tygodniu' ~ 0,
             `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Codziennie'~ 1,
             `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` =='Kilka razy w tygodniu' ~ 1
           ),
         `21.  Czy cierpisz z powodu niskiej samooceny i/lub zaburzeń emocjonalnych ?_kodowane`=
           case_when(
             `21.  Czy cierpisz z powodu niskiej samooceny i/lub zaburzeń emocjonalnych ?` == 'Tak' ~ 1,
             `21.  Czy cierpisz z powodu niskiej samooceny i/lub zaburzeń emocjonalnych ?` == 'Nie wiem' ~ 0 ,
             `21.  Czy cierpisz z powodu niskiej samooceny i/lub zaburzeń emocjonalnych ?` == 'Nie' ~0
           ),
         `18. Czy zadowala Cię wygląd własnego ciała ?_kodowane`=
           case_when(
             `18. Czy zadowala Cię wygląd własnego ciała ?` == 'Nie, nie jestem zadowolona' ~ 0,
             `18. Czy zadowala Cię wygląd własnego ciała ?` == 'Tak, jestem zadowolona' ~ 1,
             `18. Czy zadowala Cię wygląd własnego ciała ?` == 'Gdybym schudła byłabym zadowolona' ~ 0,
             `18. Czy zadowala Cię wygląd własnego ciała ?` == 'Gdybym przytyła byłabym zadowolona' ~ 0
             
           ),
         `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane`=
           case_when(
             `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?` == 'Tak' ~ 1,
             `4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?` == 'Nie' ~ 0
           ),
         `5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?_kodowane` =
           case_when(
             `5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?`== 'Tak, moja samoocena uległa pogorszeniu' ~ 1,
             `5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?`== 'Nie myślałam nad tym' ~ 0,
             `5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?`== 'Nie, nie wpłynęło to na moją samoocenę' ~ 0,
           ),
         `interptacja_pytania_4_i_5_kodowane` = ifelse(`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` == 1, 
                                                       `5. Jeżeli tak to czy wpłynęło to w jakiś sposób na twoją samoocenę ?_kodowane`, 
                                                       "none")
         
  )


# Filtracja na podstawie pytania 4
osoby_porownujace_wyglad <- nazwy_kolumn_do_zakodowania %>%
  filter(`4. Czy zdarza Ci się porównywać swój wygląd z wyglądem influencerów z TikToka?_kodowane` == 1)

# Dalsza analiza na podstawie pytania 5
dane_po_kodowaniu_hip1_grupy <- osoby_porownujace_wyglad %>%
  select(ends_with("_kodowane"))

grupa_Ogladaczy <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` == 1
)

grupa_NieOgladaczy <- filter(
  dane_po_kodowaniu_hip1_grupy,
  dane_po_kodowaniu_hip1_grupy$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` == 0
)


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
    geom_bar(aes(y = Zla_Samoocena, fill = "Osoby niezadowolone ze swojego wyglądu ciała"), stat = "identity", width = 0.5) +
    scale_fill_manual(values = c("Wszystkie Osoby" = "lightblue", "Osoby niezadowolone ze swojego wyglądu ciała" = "orange")) +
    geom_text(aes(label = Liczba_Osob, y = Liczba_Osob), vjust = -0.5, size = 3, color = "black") +
    geom_text(aes(label = Zla_Samoocena, y = Zla_Samoocena), vjust = -0.5, size = 3, color = "black") +
    geom_text(aes(label = paste(round(Procent_Zla_Samoocena, 2), "%"), y = Zla_Samoocena - 10), vjust = -0.5, size = 3, color = "black") +
    geom_text(x = 1.5, y = max(dane_wykres$Liczba_Osob), label = paste("p-value =", format(t_samoocena$p.value, digits = 4)), vjust = 1.5, size = 3, color = "black") +
    labs(title = "Liczba osób niezadowolonych z wyglądu swojego ciała w porównaniu z ogólną liczbą osób w grupach",
         x = "Grupa",
         y = "Liczba osób",
         fill = "Legenda") +
    theme_classic() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))  # Poprawiona linijka z legendą
  
  print(wykres_slupek)
  
  print(grupa_NieOgladaczy,n=40)
  