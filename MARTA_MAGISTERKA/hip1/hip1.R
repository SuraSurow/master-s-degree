library(readxl)  
library(dplyr)   
library(writexl)  
library(ggpubr)

#1)    Czas stosowania suplementu 
#zawierającego silimarynę ma wpływ 
#na stopień obniżenia glikemii u osób 
#chorujących na cukrzycę typu 2 i NAFLD.

dir.create("Hipoteza1/Kodowanie", showWarnings = FALSE)

an <- read_excel("M.-Bednarczyk_dane.xlsx")

kodowanieHipoteza1 <- an %>%
  mutate(
    `6. Czy choruje Pan_i na cukrzycę typu 2?_kodowane` = case_when(
      `6. Czy choruje Pan_i na cukrzycę typu 2?` == 'Tak' ~ 1,
      `6. Czy choruje Pan_i na cukrzycę typu 2?` == 'Nie (w tym momencie można zakończyć' ~ 0,
      `6. Czy choruje Pan_i na cukrzycę typu 2?` == 'Nie' ~ 0
    ),
    `10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?_kodowane` = case_when(
      `10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?` == 'Tak' ~ 1,
      `10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?` == 'Nie' ~ 0
    ),
    `13. Czy przyjmuje Pan_i suplement, który w swoim składzie zawiera sylimarynę (ostropest plamisty)?_kodowane` = case_when(
      `13. Czy przyjmuje Pan_i suplement, który w swoim składzie zawiera sylimarynę (ostropest plamisty)?` == 'Tak' ~ 1,
      `13. Czy przyjmuje Pan_i suplement, który w swoim składzie zawiera sylimarynę (ostropest plamisty)?` == 'Nie' ~ 0
    ),
    `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?_kodowane` = case_when(
      `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?` == '1-2 miesiące' ~ 1,
      `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?` == '3-5 miesięcy' ~ 2,
      `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?` == '6-10 miesięcy' ~ 3,
      `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?` == 'Rok' ~ 4,
      `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?` == 'Więcej niż rok' ~ 5
    )
  )

po_kodowaniuHipoteza1 <- kodowanieHipoteza1 %>%
  select(ends_with("_kodowane"))

po_kodowaniuHipoteza1 <- mutate(po_kodowaniuHipoteza1,
                                `16. Jaki średni wynik glukozy na czczo odnotowuje Pan_i po suplementacji sylimaryną (ostropestem plamistym)?` 
                                = an$`16. Jaki średni wynik glukozy na czczo odnotowuje Pan_i po suplementacji sylimaryną (ostropestem plamistym)?`,
                                `17. Jaki średni wynik glukozy na czczo odnotowywał_a Pan_i przed zastosowaniem suplementacji sylimaryną (ostropestem plamistym)?` 
                                = an$`17. Jaki średni wynik glukozy na czczo odnotowywał_a Pan_i przed zastosowaniem suplementacji sylimaryną (ostropestem plamistym)?`
)

#grupa osob posiadajacych NAFLD oraz cykrzyce typu 2
grupa_badawcza <- filter(
  po_kodowaniuHipoteza1,
  `6. Czy choruje Pan_i na cukrzycę typu 2?_kodowane` == 1 
  & `10. Czy zdiagnozowano u Pana_i NAFLD (niealkoholową stłuszczeniową chorobę wątroby)?_kodowane` == 1
  & `13. Czy przyjmuje Pan_i suplement, który w swoim składzie zawiera sylimarynę (ostropest plamisty)?_kodowane`==1
  )



# Stworzenie grup na podstawie zadanych kryteriów
grupa1 <- filter(grupa_badawcza, `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?_kodowane` %in% c(1, 2))
grupa2 <- filter(grupa_badawcza, `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?_kodowane` %in% c(3, 4))
grupa3 <- filter(grupa_badawcza, `14. Jak długo przyjmuje Pan_i suplement z sylimaryną (ostropestem plamistym)?_kodowane` %in% c(5))

# Obliczenie średnich dla każdej grupy przed i po
srednia1przed <- mean(grupa1$`17. Jaki średni wynik glukozy na czczo odnotowywał_a Pan_i przed zastosowaniem suplementacji sylimaryną (ostropestem plamistym)?`)
srednia1po <- mean(grupa1$`16. Jaki średni wynik glukozy na czczo odnotowuje Pan_i po suplementacji sylimaryną (ostropestem plamistym)?`)

srednia2przed <- mean(grupa2$`17. Jaki średni wynik glukozy na czczo odnotowywał_a Pan_i przed zastosowaniem suplementacji sylimaryną (ostropestem plamistym)?`)
srednia2po <- mean(grupa2$`16. Jaki średni wynik glukozy na czczo odnotowuje Pan_i po suplementacji sylimaryną (ostropestem plamistym)?`)

srednia3przed <- mean(grupa3$`17. Jaki średni wynik glukozy na czczo odnotowywał_a Pan_i przed zastosowaniem suplementacji sylimaryną (ostropestem plamistym)?`)
srednia3po <- mean(grupa3$`16. Jaki średni wynik glukozy na czczo odnotowuje Pan_i po suplementacji sylimaryną (ostropestem plamistym)?`)

# Przygotowanie danych do analizy
przed <- c(srednia1przed, srednia2przed, srednia3przed)
po <- c(srednia1po, srednia2po, srednia3po)
grupy <- rep(1:3, each = 1)

# Stworzenie ramki danych
dane <- data.frame(Przed = przed, Po = po, Grupy = factor(grupy))

# Wykonanie testu Kruskala-Wallisa
wynik_kw <- kruskal.test(Po ~ Grupy, data = dane)

# Sprawdzenie wyników testu Kruskala-Wallisa
print(wynik_kw)
library(ggplot2)
library(ggplot2)

# Dane dla wykresu
dane <- data.frame(
  Grupa = c("1-6 msc", "6-12 msc", "Ponad Rok"),
  Przed = c(srednia1przed, srednia2przed, srednia3przed),
  Po = c(srednia1po, srednia2po, srednia3po),
  Liczba_Osob = c(nrow(grupa1), nrow(grupa2), nrow(grupa3))
)
dane$Przed <- round(dane$Przed, 2)
dane$Po <- round(dane$Po, 2)

# Różnica między przed a po
dane$Roznica <- dane$Po - dane$Przed

# Wykres słupkowy
wykres <- ggplot(dane, aes(x = Grupa)) +
  geom_bar(aes(y = Przed, fill = "Przed"), stat = "identity", position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = Po, fill = "Po"), stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(y = -2, label = paste("- Test Kruskal-Wallis p-value:", round(wynik_kw$p.value, 2))), x = 2) +
  geom_text(aes(y = Przed, label = paste(Przed)), position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  geom_text(aes(y = Po, label = paste(Po)), position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  geom_text(aes(y = Po, label = paste(as.character(Liczba_Osob), " osób")), 
            position = position_dodge(width = 0.9), vjust = +5.) +
  labs(title = "Wyniki przed i po suplementacji",
       x = "Grupa",
       y = "Wartość",
       fill = "Legenda") +
  theme_classic() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Przed" = "lightblue", "Po" = "yellow"))

# Wyświetlenie wykresu
print(wykres)

  

# Wyświetlenie wykresu
print(wykres)

