












































library(readxl)
library(ggplot2)
library(dplyr)

# Wczytywanie danych z pliku Excel
ścieżka_do_pliku <- "./Dane-i-hipotezy.xlsx"
dane_excel <- read_excel(ścieżka_do_pliku)

# Funkcja do kodowania wartości
koduj_wartosci <- function(df) {
  df %>%
    mutate_all(as.character) %>%
    filter_all(all_vars(. != "null")) %>%
    mutate_all(function(x) {
      case_when(
        x == "Nigdy  lub prawie nigdy" ~ 1,
        x == "Raz w miesiącu lub rzadziej" ~ 2,
        x == "Kilka razy w miesiącu" ~ 3,
        x == "Kilka razy w tygodniu" ~ 4,
        x == "Codziennie" ~ 5,
        x == "Kilka razy dziennie" ~ 6,
        TRUE ~ NA_integer_
      )
    })
}

# Filtracja danych dla dwóch grup
wykrz <- dane_excel %>%
  filter(
    `5. Wykształcenie` == "wyższe" &
      `6. Czy choruje Pan_i na cukrzycę typu 2?` == "Tak" &
      `10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?` == "Tak"
  ) %>%
  select(starts_with("Jak często w ciągu ostatnich")) %>%
  koduj_wartosci()

nieWykrz <- dane_excel %>%
  filter(
    !(`5. Wykształcenie` == "wyższe" &
        `6. Czy choruje Pan_i na cukrzycę typu 2?` == "Tak" &
        `10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?` == "Tak")
  ) %>%
  select(starts_with("Jak często w ciągu ostatnich")) %>%
  koduj_wartosci()

# Tworzenie tablicy danych dla obu grup
tworz_tablice_danych <- function(df) {
  data.frame(
    Slodycze_i_przekaski = rowSums(df[, 1:7], na.rm = TRUE),
    Produkty_mleczne_i_jaja = rowSums(df[, 8:13], na.rm = TRUE),
    Produkty_zbozowe = rowSums(df[, 14:18], na.rm = TRUE),
    Tluszcze = rowSums(df[, 19:24], na.rm = TRUE),
    Owoce = rowSums(df[, 25:35], na.rm = TRUE),
    Warzywa_i_ziarna = rowSums(df[, 36:47], na.rm = TRUE),
    Produkty_miesne_i_ryby = rowSums(df[, 48:53], na.rm = TRUE),
    Napoje = rowSums(df[, 54:60], na.rm = TRUE)
  )
}

tablica_danych_nieWykrz <- tworz_tablice_danych(nieWykrz)
tablica_danych_wykrz <- tworz_tablice_danych(wykrz)

# Definicja kategorii i liczby pytań w każdej kategorii
kategorie <- c("A. Slodycze i przekaski", "B. Produkty mleczne i jaja", "C. Produkty zbozowe", 
               "D. Tluszcze", "E. Owoce", "F. Warzywa i ziarna", "G. Produkty miesne i ryby", "H. Napoje")

mapowanie_kategorii <- list(
  "A. Slodycze i przekaski" = list(kolumna = "Slodycze_i_przekaski", liczba_pytan = 7),
  "B. Produkty mleczne i jaja" = list(kolumna = "Produkty_mleczne_i_jaja", liczba_pytan = 6),
  "C. Produkty zbozowe" = list(kolumna = "Produkty_zbozowe", liczba_pytan = 5),
  "D. Tluszcze" = list(kolumna = "Tluszcze", liczba_pytan = 6),
  "E. Owoce" = list(kolumna = "Owoce", liczba_pytan = 11),
  "F. Warzywa i ziarna" = list(kolumna = "Warzywa_i_ziarna", liczba_pytan = 12),
  "G. Produkty miesne i ryby" = list(kolumna = "Produkty_miesne_i_ryby", liczba_pytan = 6),
  "H. Napoje" = list(kolumna = "Napoje", liczba_pytan = 7)
)

# Iteracja przez kategorie i generowanie wykresów
for (kategoria in kategorie) {
  kolumna <- mapowanie_kategorii[[kategoria]]$kolumna
  liczba_pytan <- mapowanie_kategorii[[kategoria]]$liczba_pytan
  
  suma_nieWykrz <- tablica_danych_nieWykrz[[kolumna]]
  suma_wykrz <- tablica_danych_wykrz[[kolumna]]
  maksymalna_mozliwa_ilosc_punktow <- liczba_pytan * 6
  
  dane_do_wykresu_nieWykrz <- data.frame(
    Kategoria = c("Liczba osób", "Średnia ilość punktów", "Maksymalna ilość punktów"),
    Wartość = c(length(suma_nieWykrz), mean(suma_nieWykrz, na.rm = TRUE), maksymalna_mozliwa_ilosc_punktow),
    Grupa = "Brak Wykształcenie Wyższego"
  )
  
  dane_do_wykresu_wykrz <- data.frame(
    Kategoria = c("Liczba osób", "Średnia ilość punktów", "Maksymalna ilość punktów"),
    Wartość = c(length(suma_wykrz), mean(suma_wykrz, na.rm = TRUE), maksymalna_mozliwa_ilosc_punktow),
    Grupa = "Wykształcenie Wyższe"
  )
  
  dane_do_wykresu <- bind_rows(dane_do_wykresu_nieWykrz, dane_do_wykresu_wykrz)
  
  # Obliczanie testu t dla niezależnych prób
  #test_t <- ks.test(suma_nieWykrz, suma_wykrz)#tutaj 2 z 8 jest istotna
  test_t <- kruskal.test(list(suma_nieWykrz, suma_wykrz))#kruskal wallis
  
  
  
  
  p_value <- test_t$p.value
  #
  wykres <- ggplot(data = dane_do_wykresu, aes(x = Kategoria, y = Wartość, fill = Grupa)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black") +
    geom_text(aes(label = round(Wartość, 2)), vjust = -0.5, position = position_dodge(0.9), size = 3) +
    labs(title = paste(kategoria, "- Test t p-value:", round(p_value, 4))) +
    scale_fill_manual(values = c("lightblue", "yellow")) +
    theme_minimal(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(color = "black", hjust = 0.5)
    ) +
    ylim(0, max(dane_do_wykresu$Wartość, na.rm = TRUE) * 1.1)
  
  ggsave(filename = paste0(kategoria, ".png"), plot = wykres, width = 12, height = 6, dpi = 300)
}








  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



