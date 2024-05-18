library(readxl)
library(dplyr)
library(ggplot2)

# Wczytanie danych
EAT26_testA <- read_excel("eat26_testA/ankieta_kodowanieEAT26_testA.xlsx")
EAT26_testB <- read_excel("eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")
ankieta <- read_excel("ankieta.xlsx")

# Podsumowanie liczby unikalnych wartości w EAT26_testB
count <- EAT26_testA %>%
  group_by(Suma_1_do_26) %>%
  summarize(count = n()) %>%
  mutate(kategoria = if_else(Suma_1_do_26 >= 20, "Pozytywny Wynik", "Negatywny Wynik"))

# Tworzymy wykres słupkowy z liczbą nad każdym słupkiem
ggplot(data = count, aes(x = Suma_1_do_26, y = count, fill = kategoria)) +
  geom_bar(stat = "identity") +  # Dodajemy słupki
  geom_text(aes(label = count), vjust = -0.5) +  # Dodajemy liczby nad słupkami, vjust kontroluje pozycję pionową
  scale_fill_manual(values = c("Negatywny Wynik" = "black", "Pozytywny Wynik" = "red"),  # Ustawiamy kolory
                    name = "Kategoria",  # Nazwa dla legendy
                    labels = c("Negatywny Wynik", "Pozytywny Wynik")) +  # Etykiety legendy
  labs(x = "Wynik", y = "Liczba osób") +  # Etykiety osi
  theme_minimal() +  # Minimalny styl
  ggtitle("Wykres liczebności wyników EAT26-A")  # Tytuł wykresu


