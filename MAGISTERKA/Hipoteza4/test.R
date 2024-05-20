













####################################################################

                                #pytanie 12

####################################################################
# Biblioteki
library(readxl)
library(dplyr)

# Wczytanie danych z pliku Excel
ścieżka_do_pliku <- "ankieta.xlsx"
ankieta <- read_excel(ścieżka_do_pliku)

# Zakodowanie odpowiedzi
dane_kodowane <- ankieta %>%
  mutate(
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` = case_when(
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3
    ),
    `12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?_kodowane` = case_when(
      `12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?`== "Tak zauważyłam pozytywne zmiany w moim odżywieniu" ~ 1,
      `12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?`== "Tak, zauważyłam negatywne zmiany w moim odżywianiu" ~ -1,
      `12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?`== "Nie , nie zauważyłam zmian w moim odżywieniu" ~ 0
    )
  )

# Podzielenie danych na grupy
grupa_0 <- dane_kodowane %>%
  filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 0) %>%
  pull(`12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?_kodowane`)

grupa_1 <- dane_kodowane %>%
  filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 1) %>%
  pull(`12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?_kodowane`)

grupa_2 <- dane_kodowane %>%
  filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 2) %>%
  pull(`12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?_kodowane`)

grupa_3 <- dane_kodowane %>%
  filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 3) %>%
  pull(`12. Czy w wyniku korzystania z TikToka zauważyłaś jakieś zmiany w swoim odżywianiu?_kodowane`)

# Przeprowadzenie testu Kruskala-Wallisa
kruskal_test <- kruskal.test(list( grupa_1, grupa_2, grupa_3))

# Wyświetlenie wyników testu
print(kruskal_test)

# Wykres słupkowy
grupy <- c("Grupa 1", "Grupa 2", "Grupa 3")
srednie <- c(mean(grupa_1, na.rm = TRUE), mean(grupa_2, na.rm = TRUE), mean(grupa_3, na.rm = TRUE))

df <- data.frame(grupy, srednie)

# Wykres słupkowy ze skalą uwzględniającą wartości ujemne
# Wykres słupkowy ze skalą uwzględniającą wartości ujemne i etykietami na słupkach
ggplot(df, aes(x = grupy, y = srednie, fill = factor(ifelse(srednie > 0, "Pozytywny wpływ", "Negatywny wpływ")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(srednie, 2)), vjust = -0.5) +  # Dodanie etykiet
  geom_text( label = paste("p =", round(kruskal_test$p.value, 3)),x=3,y=0.05, vjust = -1) +  # Dodanie p-value
  scale_fill_manual(name = "Wpływ", values = c("Pozytywny wpływ" = "green", "Negatywny wpływ" = "red")) +  # Ustawienie kolorów
  labs(title = "Średnie wartości zmian w odżywianiu w poszczególnych grupach", x = "Grupy", y = "Średnia wartość zmiany w odżywianiu")

####################################################################


                              #pytanie 17

#####################################################################
# Biblioteki
library(readxl)
library(dplyr)
library(ggplot2)

# Wczytanie danych z pliku Excel
ścieżka_do_pliku <- "ankieta.xlsx"
ankieta <- read_excel(ścieżka_do_pliku)

# Zakodowanie odpowiedzi
dane_kodowane <- ankieta %>%
  mutate(
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` = case_when(
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3
    ),
    `17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?_kodowane` = case_when(
      `17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?` == "Tak" ~ 1,
      `17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?` == "Nie" ~ 0,
      `17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?` == "Nie przypominam sobie" ~ 0
    )
  )

# Podzielenie danych na grupy
liczba_osob <- c(#pomijam 1 grupe nieogladajacy
  dane_kodowane %>% filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 1) %>% nrow(),
  dane_kodowane %>% filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 2) %>% nrow(),
  dane_kodowane %>% filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 3) %>% nrow()
)

liczba_tak <- c(
  sum(dane_kodowane %>% filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 1) %>% pull(`17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?_kodowane`)),
  sum(dane_kodowane %>% filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 2) %>% pull(`17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?_kodowane`)),
  sum(dane_kodowane %>% filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 3) %>% pull(`17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?_kodowane`))
)

# Procent odpowiedzi "Tak" w każdej grupie
procent_tak <- liczba_tak / liczba_osob * 100

# Tworzenie ramki danych
df <- data.frame(
  grupy = c( "Godzina i mniej", "Od godziny do trzech", "Powyżej trzech godzin"),
  liczba_osob = liczba_osob,
  liczba_tak = liczba_tak,
  procent_tak = procent_tak
)

# Funkcja do obliczenia testu k-sample permutation
k_sample_permutation_test <- function(x, g) {
  # Liczba grup
  k <- length(unique(g))
  
  # Liczba obserwacji
  n <- length(x)
  
  # Obliczenie statystyki testowej
  group_means <- tapply(x, g, mean)
  grand_mean <- mean(x)
  obs_stat <- sum((group_means - grand_mean)^2)
  
  # Przeprowadzenie permutacji
  num_perm <- 1000
  perm_stats <- numeric(num_perm)
  for (i in 1:num_perm) {
    perm_group <- sample(g)
    perm_group_means <- tapply(x, perm_group, mean)
    perm_stat <- sum((perm_group_means - grand_mean)^2)
    perm_stats[i] <- perm_stat
  }
  
  # Obliczenie p-value
  p_value <- sum(perm_stats >= obs_stat) / num_perm
  
  return(p_value)
}

# Wykonanie testu k-sample permutation
perm_test_p_value <- k_sample_permutation_test(
  x = dane_kodowane$`17. Czy przez treści z TikToka kiedykolwiek ograniczyłaś ilość przyjmowanych kalorii co wpłynęło na wystąpienie u Ciebie niedostatecznej masy ciała?_kodowane`, g = dane_kodowane$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`
)

#Wyświetlenie wartości p
print(perm_test_p_value)

#Wykres słupkowy z uwzględnieniem wartości p-value
# Wykres słupkowy z uwzględnieniem wartości p-value
df <- df %>%
  mutate(procent_tak = liczba_tak / liczba_osob * 100)

# Wykres słupkowy z uwzględnieniem wartości p-value i procentowych odpowiedzi 'Tak'
ggplot(df, aes(x = grupy)) +
  geom_bar(aes(y = liczba_osob, fill = "Liczba osób"), stat = "identity", width = 0.5) +
  geom_text(aes(label = liczba_osob, y = liczba_osob), vjust = -0.5, size = 3) +
  geom_bar(aes(y = liczba_tak, fill = "Odpowiedzi 'Tak'"), stat = "identity", width = 0.5) +
  geom_text(aes(label = liczba_tak, y = liczba_tak), vjust = 1, size = 3) +
  geom_text(aes(label = paste("p-value =", round(perm_test_p_value, 3))), x = 3, y = max(df$liczba_osob) * 0.8, size = 4, color = "black") +
  geom_text(aes(label = paste0(round(procent_tak, 1), "%"), y = liczba_tak), vjust = -0.5, size = 3, color = "black") +
  labs(
    title = "Liczba osób i odpowiedzi 'Tak' na pytanie dotyczące ograniczania ilości kalorii związane z TikTokiem",
    x = "Grupy",
    y = "Liczba osób / Liczba odpowiedzi 'Tak'",
    caption = "Procent odpowiedzi 'Tak'"
  ) +
  scale_fill_manual(values = c("Liczba osób" = "skyblue", "Odpowiedzi 'Tak'" = "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())

  

###########################################################

###########################################################





























































library(readxl)
library(dplyr)

# Wczytanie danych z pliku Excel
ścieżka_do_pliku <- "ankieta.xlsx"
dane_excel <- read_excel(ścieżka_do_pliku)

# Podział odpowiedzi na poszczególne elementy
podzielone_odpowiedzi <- lapply(dane_excel$`15. Które z poniższych zachowań zaobserwowałaś u siebie pod wpływem treści przeglądanych na TikToku?`, function(x) trimws(strsplit(x, ",")[[1]]))

# Dodanie kodowania odpowiedzi
dane_excel <- dane_excel %>%
  mutate(
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` = case_when(
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3
    )
  )

# Grupowanie danych według czasu poświęcanego na przeglądanie TikToka i obliczenie statystyk
wyniki_grupowane <- dane_excel %>%
  group_by(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`) %>%
  summarize(
    Liczba_osob = n(),
    Średnia_odpowiedzi = mean(unlist(podzielone_odpowiedzi), na.rm = TRUE)
    # Tutaj możesz dodać inne statystyki, których chcesz użyć
  )

# Wyświetlenie wyników
print(wyniki_grupowane)




