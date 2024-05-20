library(readxl)
library(ggplot2)
library(dplyr)

ścieżka_do_pliku <- "Dane-i-hipotezy.xlsx"
dane_excel <- read_excel(ścieżka_do_pliku)

dane_ankiety_nafld <- dane_excel %>%
  filter(`10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?` == "Tak")

liczba_osob_nafld <- nrow(dane_ankiety_nafld)

pytanie <- dane_ankiety_nafld$`19. Jakie parametry uległy poprawie po stosowaniu suplementacji z sylimaryną (ostropestem plamistym)?`

podzielone_odpowiedzi <- lapply(pytanie, function(x) trimws(strsplit(x, ",")[[1]]))

glowne_odpowiedzi <- c(
  "Poprawa pracy przewodu pokarmowego",
  "Zmniejszenie dyskomfortu po posiłku",
  "Zmniejszenie lub całkowity brak bólu brzucha",
  "Brak ciągłego zmęczenia i osłabienia"
)

inne_odpowiedzi <- c(
  "Nie stosuje", "Nie biorę", "Nie brałam", "null", "biorę z polecenia", "Nie suplementuje"
)

dane_wykresow <- data.frame(
  Odpowiedz = character(),
  Liczba_osob = numeric()
)

for (i in 1:length(podzielone_odpowiedzi)) {
  odpowiedzi <- podzielone_odpowiedzi[[i]]
  for (odpowiedz in odpowiedzi) {
    if (odpowiedz %in% glowne_odpowiedzi) {
      dane_wykresow <- rbind(dane_wykresow, data.frame(Odpowiedz = odpowiedz, Liczba_osob = 1))
    } else if (odpowiedz %in% inne_odpowiedzi) {
      next
    } else {
      dane_wykresow <- rbind(dane_wykresow, data.frame(Odpowiedz = "Inne", Liczba_osob = 1))
    }
  }
}

dane_sum <- aggregate(Liczba_osob ~ Odpowiedz, data = dane_wykresow, sum)

# Dodanie danych o liczbie osób zdiagnozowanych z NAFLD
dane_sum <- rbind(data.frame(Odpowiedz = "NAFLD", Liczba_osob = liczba_osob_nafld), dane_sum)

wykres <- ggplot(dane_sum, aes(x = Odpowiedz, y = Liczba_osob, fill = Odpowiedz)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Liczba_osob), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Liczba osób dla różnych odpowiedzi") +
  theme_minimal() +
  theme(legend.position = "none", panel.background = element_rect(fill = "white"))

print(wykres)

nazwa_pliku <- "wykres_odpowiedzi.png"
ggsave(filename = nazwa_pliku, plot = wykres, width = 19, height = 6, dpi = 300)



