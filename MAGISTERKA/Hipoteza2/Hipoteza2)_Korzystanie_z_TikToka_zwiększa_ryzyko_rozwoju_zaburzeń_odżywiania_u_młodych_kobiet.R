
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(lawstat)
library(PMCMRplus)


EAT26_testA <- read_excel("eat26_testA/ankieta_kodowanieEAT26_testA.xlsx")
EAT26_testB <- read_excel("Hipoteza2/eat26_testB/ankieta_kodowanieEAT26_testB.xlsx")
ankieta <- read_excel("ankieta.xlsx")



wplyw <- ankieta %>%
  transmute(
    ID = row_number(),
    Czy_TikTok_wpływał = case_when(
      ankieta$`Czy którekolwiek z danych wyżej zachowań zostało wywołane lub podtrzymane pod wpływem treści oglądanych na TikToku?...35` %in% c("Tak", "Może") ~ 1,
      ankieta$`Czy którekolwiek z danych wyżej zachowań zostało wywołane lub podtrzymane pod wpływem treści oglądanych na TikToku?...35` %in% c("Nie", "Nie wiem") ~ 0,
      TRUE ~ NA_integer_
    )
  )


wyniki <- wplyw %>%
  left_join(select(EAT26_testB, ID, Interpretacja), by = "ID") %>%
  rename(InterpretacjaTestB = Interpretacja)


grupa_wpływ <- wyniki %>%
  filter(Czy_TikTok_wpływał == 1) %>%
  pull(InterpretacjaTestB)

grupa_brak_wpływu <- wyniki %>%
  filter(Czy_TikTok_wpływał == 0) %>%
  pull(InterpretacjaTestB)

#test_result <-wilcox.test(grupa_wpływ, grupa_brak_wpływu, exact = FALSE) wyszlo 0.055
#test_result <- brunner.munzel.test(grupa_wpływ, grupa_brak_wpływu)  wyszlo 0.085

test_result <- vanWaerdenTest(list(grupa_wpływ,grupa_brak_wpływu))
dane_wpływ <- wyniki %>%
  group_by(Czy_TikTok_wpływał) %>%
  summarise(
    Total = n(),
    With_Interpretacja = sum(InterpretacjaTestB == 1, na.rm = TRUE)
  )%>%
  mutate(Percent_Risk =With_Interpretacja / Total * 100 )


# Wykres
ggplot(dane_wpływ, aes(x = factor(Czy_TikTok_wpływał, levels = c(0, 1)), y = Total, fill = "Wszystkie osoby")) +
  geom_bar(stat = "identity", alpha = 0.7, color = "black") +
  geom_bar(aes(y = With_Interpretacja, fill = "Osoby w grupie ryzyka"), stat = "identity", alpha = 0.7, color = "black") +
  scale_fill_manual(values = c("Wszystkie osoby" = "lightblue", "Osoby w grupie ryzyka" = "orange")) +
  labs(
    title = "Wpływ TikToka na ryzyko zaburzeń odżywiania EAT26-B",
    x = "Wpływ TikToka",
    y = "Liczba osób",
    fill = "Legenda"
  ) +
  # Dodanie wartości liczbowych na szczycie każdego słupka
  geom_text(aes(x = factor(Czy_TikTok_wpływał), y = Total, label = Total), vjust = -0.5, color = "black") +
  geom_text(aes(x = factor(Czy_TikTok_wpływał), y = With_Interpretacja, label = With_Interpretacja), vjust = -0.5, color = "orange") +
  geom_text(aes(x = factor(Czy_TikTok_wpływał), y = With_Interpretacja + (Total - With_Interpretacja) / 2, label = sprintf("%0.1f%%", Percent_Risk)), color = "orange", size = 3) +
  geom_signif(
    comparisons = list(c("0", "1")),
    annotations = paste("p =", format(test_result$p.value, digits = 3)),
    textsize = 3,
    y_position = max(dane_wpływ$Total) + 5
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Umieszczenie legendy na dole
    plot.title = element_text(hjust = 0.5)  # Centrowanie tytułu
  )
