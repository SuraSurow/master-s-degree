library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Wczytanie danych
ankieta <- read_excel("../ankieta.xlsx")

# Kodowanie danych
nazwy_kolumn_do_zakodowania <- ankieta %>%
  mutate(
    `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane` = case_when(
      `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` == 'Nie oglądam tego typu treści' ~ 0,
      `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` == 'Raz w tygodniu' ~ 1,
      `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` == 'Kilka razy w tygodniu' ~ 2,
      `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?` == 'Codziennie' ~ 3
    ),
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` = case_when(
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'w ogóle nie używam tej aplikacji' ~ 0,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'godzinę i mniej' ~ 1,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'od godziny do trzech godzin' ~ 2,
      `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?` == 'powyżej trzech godzin' ~ 3
    )
  ) %>%
  filter(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` != 0)  # Usunięcie grupy, która nie używa aplikacji

# Test chi-kwadrat
test_chikwadrat <- chisq.test(table(
  nazwy_kolumn_do_zakodowania$`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane`,
  nazwy_kolumn_do_zakodowania$`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`
))

# Wyświetlenie wyników testu
print(test_chikwadrat)

# Obliczenie liczby i procentów dla etykiet
dane_podsumowanie <- nazwy_kolumn_do_zakodowania %>%
  group_by(`2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane`, `3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane`) %>%
  summarise(Liczba = n()) %>%
  mutate(Procent = Liczba / sum(Liczba) * 100)

# Dodanie etykiet do wykresu
ggplot(data = nazwy_kolumn_do_zakodowania, aes(x = factor(
  case_when(
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 1 ~ 'godzinę i mniej',
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 2 ~ 'od godziny do trzech godzin',
    `2. Ile czasu dziennie poświęcasz na przeglądanie TikToka?_kodowane` == 3 ~ 'powyżej trzech godzin'
  )), 
  fill = factor(`3. Jak często przeglądasz treści na TikToku dotyczące odżywiania i wyglądu ?._kodowane`))) +
  geom_bar(position = "fill") +
  geom_text(data = dane_podsumowanie, aes(label = paste(Liczba, "\n", round(Procent, 1), "%"), y = Procent / 100 + 0.02), size = 3, color = "black", position = position_fill(vjust = 0.5)) +
  geom_text(aes(x = 2.5, y = 1.05, label = paste("p-value =", format(test_chikwadrat$p.value, digits = 4))), vjust = -1, size = 3, color = "black") +
  labs(x = "Czas spędzany na TikToku ",
       y = "Proporcja",
       fill = "Częstość przeglądania treści ",
       title = "Częstość przeglądania treści na TikToku dotyczących odżywiania i wyglądu vs Czas spędzany na TikToku") +
  theme_minimal()


