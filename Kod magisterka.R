# Załadowanie wymaganych pakietów

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(DescTools)
library(ca) 
library(factoextra)
library(plotly)
library(tidyr)
# Wczytanie danych
dane <- read_excel("C:/Users/Monisia/Desktop/Analiza wpływu stresu i cech osobowościowych na wybór_ organizacji zawodowej.xlsx")
dane <- as.data.frame(dane)
dane <- dane[, -(1:5)]
###############PRZYPISANIE KOLORÓW ORGANIZACJI NA PODSTAWIE ODPOWIEDZI##########################
# Pytanie 1: Podejście do stresu
# Przypisanie koloru do nowej kolumny
dane <- dane %>%
  mutate(kolor_pyt1 = case_when(
    grepl("bardzo źle", .[[1]], ignore.case = TRUE) ~ "czerwona",
    grepl("raczej źle", .[[1]], ignore.case = TRUE) ~ "bursztynowa",
    grepl("średnio", .[[1]], ignore.case = TRUE) ~ "pomarańczowa",
    grepl("raczej dobrze", .[[1]], ignore.case = TRUE) ~ "zielona",
    grepl("bardzo dobrze", .[[1]], ignore.case = TRUE) ~ "turkusowa",
    TRUE ~ NA_character_
  ))

# Pytanie 2: Struktura organizacji
dane <- dane %>%
  mutate(kolor_pyt2 = case_when(
    grepl("sztywna", .[[2]], ignore.case = TRUE) ~ "czerwona",
    grepl("elastyczna", .[[2]], ignore.case = TRUE) ~ "turkusowa",
    TRUE ~ NA_character_
  ))

colnames(dane)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)] <- c(
  "pyt3_kontrola",
  "pyt3_lojalnosc",
  "pyt3_innowacyjnosc",
  "pyt3_kooperatywnosc",
  "pyt3_autonomiczność",
  "pyt4_bezpieczenstwo_w_hierarchii",
  "pyt4_stabilnosc",
  "pyt4_efektywnosc",
  "pyt4_szacunek",
  "pyt4_samozarzadzanie"
)
# # Pytanie 3: Aspekty ważne w pracy + Pytanie 4: Wartości ważne w pracy
# dane <- dane %>%
#   mutate(
#     # Pytanie 3 – aspekty
#     kolor_pyt3_kontrola = case_when(
#       pyt3_kontrola == 1 ~ "turkusowa",
#       pyt3_kontrola == 2 ~ "zielona",
#       pyt3_kontrola == 3 ~ "pomarańczowa",
#       pyt3_kontrola == 4 ~ "bursztynowa",
#       pyt3_kontrola == 5 ~ "czerwona"
#     ),
#     kolor_pyt3_lojalnosc = case_when(
#       pyt3_lojalnosc == 1 ~ "turkusowa",
#       pyt3_lojalnosc == 2 ~ "zielona",
#       pyt3_lojalnosc == 3 ~ "pomarańczowa",
#       pyt3_lojalnosc == 4 ~ "bursztynowa",
#       pyt3_lojalnosc == 5 ~ "czerwona"
#     ),
#     kolor_pyt3_innowacyjnosc = case_when(
#       pyt3_innowacyjnosc == 1 ~ "czerwona",
#       pyt3_innowacyjnosc == 2 ~ "bursztynowa",
#       pyt3_innowacyjnosc == 3 ~ "pomarańczowa",
#       pyt3_innowacyjnosc == 4 ~ "zielona",
#       pyt3_innowacyjnosc == 5 ~ "turkusowa"
#     ),
#     kolor_pyt3_kooperatywnosc = case_when(
#       pyt3_kooperatywnosc == 1 ~ "czerwona",
#       pyt3_kooperatywnosc == 2 ~ "bursztynowa",
#       pyt3_kooperatywnosc == 3 ~ "pomarańczowa",
#       pyt3_kooperatywnosc == 4 ~ "zielona",
#       pyt3_kooperatywnosc == 5 ~ "turkusowa"
#     ),
#     kolor_pyt3_autonomiczność = case_when(
#       pyt3_autonomiczność == 1 ~ "czerwona",
#       pyt3_autonomiczność == 2 ~ "bursztynowa",
#       pyt3_autonomiczność == 3 ~ "pomarańczowa",
#       pyt3_autonomiczność == 4 ~ "zielona",
#       pyt3_autonomiczność == 5 ~ "turkusowa"
#     ),
#     
#     # Pytanie 4 – wartości
#     kolor_pyt4_bezpieczenstwo_w_hierarchii = case_when(
#       pyt4_bezpieczenstwo_w_hierarchii == 1 ~ "turkusowa",
#       pyt4_bezpieczenstwo_w_hierarchii == 2 ~ "zielona",
#       pyt4_bezpieczenstwo_w_hierarchii == 3 ~ "pomarańczowa",
#       pyt4_bezpieczenstwo_w_hierarchii == 4 ~ "bursztynowa",
#       pyt4_bezpieczenstwo_w_hierarchii == 5 ~ "czerwona"
#     ),
#     kolor_pyt4_stabilnosc = case_when(
#       pyt4_stabilnosc == 1 ~ "turkusowa",
#       pyt4_stabilnosc == 2 ~ "zielona",
#       pyt4_stabilnosc == 3 ~ "pomarańczowa",
#       pyt4_stabilnosc == 4 ~ "bursztynowa",
#       pyt4_stabilnosc == 5 ~ "czerwona"
#     ),
#     kolor_pyt4_efektywnosc = case_when(
#       pyt4_efektywnosc == 1 ~ "czerwona",
#       pyt4_efektywnosc == 2 ~ "bursztynowa",
#       pyt4_efektywnosc == 3 ~ "pomarańczowa",
#       pyt4_efektywnosc == 4 ~ "zielona",
#       pyt4_efektywnosc == 5 ~ "turkusowa"
#     ),
#     kolor_pyt4_szacunek = case_when(
#       pyt4_szacunek == 1 ~ "czerwona",
#       pyt4_szacunek == 2 ~ "bursztynowa",
#       pyt4_szacunek == 3 ~ "pomarańczowa",
#       pyt4_szacunek == 4 ~ "zielona",
#       pyt4_szacunek == 5 ~ "turkusowa"
#     ),
#     kolor_pyt4_samozarzadzanie = case_when(
#       pyt4_samozarzadzanie == 1 ~ "czerwona",
#       pyt4_samozarzadzanie == 2 ~ "bursztynowa",
#       pyt4_samozarzadzanie == 3 ~ "pomarańczowa",
#       pyt4_samozarzadzanie == 4 ~ "zielona",
#       pyt4_samozarzadzanie == 5 ~ "turkusowa"
#     )
#   )
---------------------------------------------------------------
#Inny sposób:
# Indeksy kolumn dla pytania 3 (Aspekty ważne w pracy)
kolumny_pyt3_idx <- 3:7  # Cechy: Wysokie poczucie kontroli, Lojalność, Innowacyjność, Kooperatywność, Autonomiczność
# Przypisanie nazw kolorów do tych cech
kolory_pyt3 <- c(
  "czerwona",     # Wysokie poczucie kontroli
  "bursztynowa",  # Lojalność
  "pomarańczowa", # Innowacyjność
  "zielona",      # Kooperatywność
  "turkusowa"     # Autonomiczność
)
# Indeksy kolumn dla pytania 4 (Wartości ważne w pracy)
kolumny_pyt4_idx <- 8:12  # Wartości: Poczucie bezpieczeństwa w hierarchii, Stabilność, Efektywność, Wzajemny szacunek, Samozarządzanie
# Przypisanie nazw kolorów do tych wartości
kolory_pyt4 <- c(
  "czerwona",     # Poczucie bezpieczeństwa w hierarchii
  "bursztynowa",  # Stabilność
  "pomarańczowa", # Efektywność
  "zielona",      # Wzajemny szacunek
  "turkusowa"     # Samozarządzanie
)  
# Najpierw sprawdźmy strukturę danych
str(dane[, kolumny_pyt3_idx])
str(dane[, kolumny_pyt4_idx])

# Konwersja danych znaków na liczby - WAŻNE, gdyż dane są w formacie tekstu
dane <- dane %>%
  mutate(across(all_of(c(kolumny_pyt3_idx, kolumny_pyt4_idx)), ~as.numeric(as.character(.))))

# Sprawdź, czy konwersja przebiegła pomyślnie
str(dane[, kolumny_pyt3_idx])
str(dane[, kolumny_pyt4_idx])

# Sprawdź, czy są jakieś NA lub wartości problematyczne
summary(dane[, kolumny_pyt3_idx])
summary(dane[, kolumny_pyt4_idx])

# ROZWIĄZANIE 1: Użyj bardziej bezpośredniego podejścia do dodania kolumn
# Dodaj kolumnę dla pytania 3
dane$pyt3_dominujacy_kolor <- NA_character_  # Inicjalizuj kolumnę z NA

# W pętli przetwórz każdy wiersz
for (i in 1:nrow(dane)) {
  # Pobierz wartości dla wiersza i
  oceny_pyt3 <- as.numeric(unlist(dane[i, kolumny_pyt3_idx]))
  
  # Znajdź indeks maksymalnej wartości
  max_idx <- which.max(oceny_pyt3)
  
  # Jeśli znaleziono maksimum, przypisz kolor
  if (length(max_idx) > 0 && !is.na(max_idx)) {
    dane$pyt3_dominujacy_kolor[i] <- kolory_pyt3[max_idx]
  }
}

# Dodaj kolumnę dla pytania 4
dane$pyt4_dominujacy_kolor <- NA_character_  # Inicjalizuj kolumnę z NA

# W pętli przetwórz każdy wiersz
for (i in 1:nrow(dane)) {
  # Pobierz wartości dla wiersza i
  oceny_pyt4 <- as.numeric(unlist(dane[i, kolumny_pyt4_idx]))
  
  # Znajdź indeks maksymalnej wartości
  max_idx <- which.max(oceny_pyt4)
  
  # Jeśli znaleziono maksimum, przypisz kolor
  if (length(max_idx) > 0 && !is.na(max_idx)) {
    dane$pyt4_dominujacy_kolor[i] <- kolory_pyt4[max_idx]
  }
}

# Sprawdź, czy kolumny zostały dodane
names(dane)

# Podgląd nowych kolumn
head(dane[, c("pyt3_dominujacy_kolor", "pyt4_dominujacy_kolor")])

# Podsumowanie dominujących kolorów
table(dane$pyt3_dominujacy_kolor, useNA = "ifany")
table(dane$pyt4_dominujacy_kolor, useNA = "ifany")

#Pytanie 5 Stwierdzenia:
# Przypisanie kolorów do stwierdzeń 13–32
kolory_stwierdzen <- c(
  "bursztynowa", "turkusowa", "zielona", "czerwona", "bursztynowa", 
  "pomarańczowa", "turkusowa", "zielona", "czerwona", "zielona", 
  "pomarańczowa", "pomarańczowa", "turkusowa", "pomarańczowa", "bursztynowa", 
  "czerwona", "turkusowa", "zielona", "bursztynowa", "czerwona"
)

# Pobranie nazw kolumn z danych (przyjmujemy, że są to kolumny 1–20)
nazwy_stwierdzen <- names(dane)[13:32]

# Łączenie kolumny z odpowiadającym kolorem
stwierdzenia_kolory <- data.frame(
  kolumna = nazwy_stwierdzen,
  kolor = kolory_stwierdzen,
  stringsAsFactors = FALSE
)

# Obliczanie dominującego koloru dla każdego respondenta
dane <- dane %>%
  rowwise() %>%
  mutate(pyt5_stwierdzenia = {
    punkty <- as.numeric(c_across(all_of(stwierdzenia_kolory$kolumna)))
    punkty[is.na(punkty)] <- 0  # jeśli są NA, traktuj jako 0
    suma_kolorow <- tapply(punkty, stwierdzenia_kolory$kolor, sum)
    
    if (all(suma_kolorow == 0)) {
      NA_character_  # brak ocen
    } else {
      names(which.max(suma_kolorow))  # zwraca kolor z najwyższą sumą
    }
  }) %>%
  ungroup()

#Pytanie 6:Poziom stresu
dane <- dane %>%
  mutate(pyt6_stres_kolor = case_when(
    .[[33]]== "Zdecydowanie tak" ~ "turkusowa",
    .[[33]] == "Raczej tak" ~ "zielona",
    .[[33]] == "Nie mam zdania" ~ "pomarańczowa",
    .[[33]] == "Raczej nie" ~ "bursztynowa",
    .[[33]] == "Zdecydowanie nie" ~ "czerwona",
    TRUE ~ NA_character_
  ))

# Pytanie 7: Wybór firmy
library(stringr)
dane <- dane %>%
  mutate(
    pyt7_firma = case_when(
      str_detect(.[[34]], "Firma A") ~ "pomarańczowa",
      str_detect(.[[34]], "Firma B") ~ "turkusowa", 
      str_detect(.[[34]], "Firma C") ~ "czerwona",
      str_detect(.[[34]], "Firma D") ~ "zielona",
      str_detect(.[[34]], "Firma E") ~ "bursztynowa",
      TRUE ~ NA_character_
    )
  )

# Sprawdź czy teraz działa
table(dane$pyt7_firma, useNA = "ifany")


# Pytanie 8 Hierarchia
# Indeksy kolumn (zgodnie z  opisem)
kolumny_cechy_idx <- 35:39

# Przypisanie nazw kolorów według indeksów
cechy_kolory <- c(
  "czerwona",     # Jasna struktura i hierarchia
  "turkusowa",    # Swoboda działania i autonomia
  "pomarańczowa", # Kreatywność i innowacyjność
  "zielona",      # Wartości i zaangażowanie
  "bursztynowa"   # Proceduralność i lojalność
)

# Nazwy kolumn odpowiadające tym indeksom
kolumny_cechy <- names(dane)[kolumny_cechy_idx]

# Przypisanie kolorów do nazw kolumn (mapa nazw → kolor)
cechy_kolory_map <- setNames(cechy_kolory, kolumny_cechy)

# Oblicz dominujący kolor
dane <- dane %>%
  rowwise() %>%
  mutate(pyt8_cechy_hierarchia = {
    oceny <- c_across(all_of(kolumny_cechy))
    suma_kolorow <- tapply(as.numeric(oceny), cechy_kolory_map, sum)
    names(which.max(suma_kolorow))
  }) %>%
  ungroup()

#Pytanie 9 Zasady hiererchii
# Indeksy kolumn dla pytania 9 (Pytanie 9 - hierarchie organizacyjne)
kolumny_hierarchia_idx <- 40:44  # Zgodnie z Twoimi wskazówkami (możesz dostosować, jeśli są inne)

# Przypisanie nazw kolorów do tych cech
hierarchia_kolory <- c(
  "czerwona",     # W organizacji dominuje autokratyczna struktura
  "bursztynowa",  # Sztywne zasady i struktura, większa rzetelność
  "pomarańczowa", # Hierarchia z naciskiem na odpowiedzialność i innowacyjność
  "zielona",      # Większa autonomia, zaufanie do pracowników
  "turkusowa"     # Brak hierarchii, pełna autonomia
)

# Nazwy kolumn odpowiadające tym indeksom
kolumny_hierarchia <- names(dane)[kolumny_hierarchia_idx]

# Przypisanie kolorów do nazw kolumn (mapa nazw → kolor)
hierarchia_kolory_map <- setNames(hierarchia_kolory, kolumny_hierarchia)

# Oblicz dominujący kolor hierarchii organizacyjnej
dane <- dane %>%
  rowwise() %>%
  mutate(pyt9_hierarchia = {
    oceny <- c_across(all_of(kolumny_hierarchia))
    suma_kolorow <- tapply(as.numeric(oceny), hierarchia_kolory_map, sum)
    names(which.max(suma_kolorow))
  }) %>%
  ungroup()

###########################################ANALIZA + WIZUALIZACJE###############
#analiza wierszowa
dominujacy_kolor_z_logika <- function(kolory, pyt7_kolor) {
  kolory <- na.omit(kolory)
  if (length(kolory) == 0) return(NA_character_)
  
  # Zlicz ile razy występuje każdy kolor
  liczby <- table(kolory)
  max_val <- max(liczby)
  top_kolory <- names(liczby[liczby == max_val])
  
  # 1) Jeśli jeden dominuje – bierzemy go
  if (length(top_kolory) == 1) return(top_kolory)
  
  # 2) Mamy remis – rozstrzygamy wg odległości od koloru z Pytania 7
  priorytet <- c("czerwona", "bursztynowa", "pomarańczowa", "zielona", "turkusowa")
  
  # Sprawdź, czy kolor z pyt7 jest w priorytecie
  idx7 <- match(pyt7_kolor, priorytet)
  if (!is.na(idx7)) {
    # Oblicz odległość każdego top_koloru od idx7
    odleglosci <- abs(match(top_kolory, priorytet) - idx7)
    # Wybierz ten z minimalną odległości
    return(top_kolory[which.min(odleglosci)])
  }
  
  # 3) Jeśli nie ma pyt7 lub nie rozstrzyga – fallback: pierwszy w priorytecie
  for (kol in priorytet) {
    if (kol %in% top_kolory) return(kol)
  }
  
  return(NA_character_)
}

# Teraz aplikujemy to do danych (zakładam, że pytanie 7 jest w kolumnie "pyt7_firma")
dane <- dane %>%
  rowwise() %>%
  mutate(
    kolor_sredni = dominujacy_kolor_z_logika(
      c_across(48:56),
      .data[["pyt7_firma"]]
    )
  ) %>%
  ungroup()
table(dane$kolor_sredni)

## analiza kolumnowa

# Zakres kolumn do analizy
kolumny_kolorowe <- dane[, 48:56]

# Funkcja pomocnicza: dominujący kolor w kolumnie
dominujacy_kolor_kolumna <- function(kolumna) {
  kolumna <- na.omit(kolumna)
  if (length(kolumna) == 0) return(NA_character_)
  
  liczby <- table(kolumna)
  max_val <- max(liczby)
  top_kolory <- names(liczby[liczby == max_val])
  
  # Jeśli remis – wybierz kolor najwyższy w hierarchii
  priorytet <- c("czerwona", "bursztynowa", "pomarańczowa", "zielona", "turkusowa")
  for (kolor in priorytet) {
    if (kolor %in% top_kolory) return(kolor)
  }
  return(NA_character_)
}

# Zastosowanie funkcji do każdej kolumny
dominujace_kolory_po_kolumnach <- sapply(kolumny_kolorowe, dominujacy_kolor_kolumna)

# Wyświetlenie wyników
dominujace_kolory_po_kolumnach
wynik_df <- data.frame(
  Pytanie = names(dominujace_kolory_po_kolumnach),
  Dominujacy_kolor = dominujace_kolory_po_kolumnach,
  row.names = NULL
)

print(wynik_df)

###################ANALIZA PYTAŃ##########################
library(gridExtra)
library(scales)
library(corrplot)
library(ggiraph)
library(viridis)
library(RColorBrewer)
# Definicja kolorów dla wizualizacji
kolory_organizacji <- c(
  "czerwona" = "tomato1",
  "bursztynowa" = "tan3",
  "pomarańczowa" = "orange",
  "zielona" = "darkolivegreen2",
  "turkusowa" = "cadetblue1"
)

############1. ROZKŁAD DOMINUJĄCYCH KOLORÓW#########

# Funkcja do tworzenia wykresu kołowego dla rozkładu dominujących kolorów
utworz_wykres_kolowy <- function(dane, kolumna, tytul) {
  # Oblicz częstości
  freq_table <- table(dane[[kolumna]])
  freq_df <- as.data.frame(freq_table)
  colnames(freq_df) <- c("Kolor", "Liczba")
  
  # Dodaj procenty
  freq_df$Procent <- freq_df$Liczba / sum(freq_df$Liczba) * 100
  freq_df$Label <- paste0(freq_df$Kolor, "\n", round(freq_df$Procent, 1), "%")
  
  # Sortuj według kolorów (hierarchia kolorów)
  kolejnosc_kolorow <- c("czerwona", "bursztynowa", "pomarańczowa", "zielona", "turkusowa")
  freq_df$Kolor <- factor(freq_df$Kolor, levels = kolejnosc_kolorow)
  freq_df <- freq_df[order(freq_df$Kolor), ]
  
  # Utwórz wykres kołowy
  wykres <- ggplot(freq_df, aes(x = "", y = Liczba, fill = Kolor)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = kolory_organizacji) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
    ) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
    labs(title = tytul)
  
  return(wykres)
}

# Tworzenie wykresów kołowych dla różnych pytań
str(dane$pyt7_firma)
str(dane$kolor_sredni)
str(dane$kolor_pyt1)
str(dane$kolor_pyt2)
str(dane$pyt3_dominujacy_kolor)
str(dane$pyt4_dominujacy_kolor)
p1 <- utworz_wykres_kolowy(dane, "pyt7_firma", "Rozkład firm docelowo wybranych przez studentów według kolorów organizacji")
p2 <- utworz_wykres_kolowy(dane, "kolor_sredni", "Ogólny rozkład dominujących kolorów organizacji wśród repsondentów")
p3 <- utworz_wykres_kolowy(dane, "pyt3_dominujacy_kolor", "Dominujące kolory - cechy charakteru")
p4 <- utworz_wykres_kolowy(dane, "pyt4_dominujacy_kolor", "Dominujące kolory - wartości w pracy")
p5 <- utworz_wykres_kolowy(dane, "kolor_pyt1", "Dominujące kolory - stres w pracy")
p6 <- utworz_wykres_kolowy(dane, "kolor_pyt2", "Dominujące kolory - skrajność struktury")
print(p5)
print(p6)
print(p3)
print(p4)
print(p1)
print(p2)


# Łączymy wykresy w siatkę
grid.arrange(p5, p6, p3, p4, p1, p2, ncol = 2)
# Rozkład dla pytania 1 (stres)
table(dane$kolor_pyt1)
prop.table(table(dane$kolor_pyt1)) * 100  # w procentach

# Rozkład dla pytania 2 (skarjność struktury)
table(dane$kolor_pyt2)
prop.table(table(dane$kolor_pyt2)) * 100  # w procentach

# Rozkład dla cech charakteru (pyt3)
table(dane$pyt3_dominujacy_kolor)
prop.table(table(dane$pyt3_dominujacy_kolor)) * 100

# Rozkład dla wartości w pracy (pyt4)
table(dane$pyt4_dominujacy_kolor)
prop.table(table(dane$pyt4_dominujacy_kolor)) * 100

# Rozkład dla pytania 7 (preferowane firmy)
table(dane$pyt7_firma)
prop.table(table(dane$pyt7_firma)) * 100  # w procentach

# Rozkład dla średniego koloru respondentów
table(dane$kolor_sredni)
prop.table(table(dane$kolor_sredni)) * 100

#############PODZIAŁ NA FILARY################

# Funkcja do wyliczenia dominującego koloru z wybranych kolumn
dominujacy_kolor_z_filaru <- function(df, kolumny) {
  apply(df[kolumny], 1, function(x) {
    tb <- table(x)
    names(tb)[which.max(tb)]
  })
}
# Dodajemy nowe kolumny z dominującym kolorem dla każdego obszaru
dane$filar_cechy <- dominujacy_kolor_z_filaru(dane, c("pyt3_dominujacy_kolor", "pyt4_dominujacy_kolor", "pyt5_stwierdzenia"))
dane$filar_hierarchia <- dominujacy_kolor_z_filaru(dane, c("kolor_pyt2","pyt9_hierarchia", "pyt8_cechy_hierarchia"))
dane$filar_stres <- dominujacy_kolor_z_filaru(dane, c("kolor_pyt1", "pyt6_stres_kolor"))
#dla wykresów tablice krzyzowe, pierwsza jak byla pomaranczowa w jednej ffilarze, czy była pomaranczowa na drugiej, pozniej na podstawie t=na tej postawie bedzie budowana tablica korelacji miedzy filarem jednym a drugim a trzecim, dklaa porządkowa, wspolczynnik gamma 
#na głównej rpzekątnej inromacja bylaby relan, główna przekątna stailnoś c studenta, ani nie jestczerwona a pozniej jestpomaranczowa, pozniej badamyile prcentjest zzgodna
p_cechy <- utworz_wykres_kolowy(dane, "filar_cechy", "Dominujące kolory: Cechy organizacji")
p_hierarchia <- utworz_wykres_kolowy(dane, "filar_hierarchia", "Dominujące kolory: Hierarchia")
p_stres <- utworz_wykres_kolowy(dane, "filar_stres", "Dominujące kolory: Stres")

# Lista kolumn do analizy
kolumny_filarowe <- c("filar_cechy", "filar_hierarchia", "filar_stres")

kolory_po_filarach <- sapply(kolumny_filarowe, function(kol) {
  names(sort(table(dane[[kol]]), decreasing = TRUE))[1]
})

kolory_filar_df <- data.frame(
  Filar = names(kolory_po_filarach),
  Kolor = kolory_po_filarach,
  row.names = NULL
)

print(kolory_filar_df)


utworz_wykres_filaru <- function(dane, kolumna, tytul) {
  dane_podsumowanie <- dane %>%
    count(Kolor = .data[[kolumna]]) %>%
    mutate(Procent = n / sum(n) * 100,
           Etykieta = paste0(round(Procent, 1), "%"))
  
  ggplot(dane_podsumowanie, aes(x = "", y = n, fill = Kolor)) +
    geom_col(width = 5, color = "black") +
    geom_text(aes(label = Etykieta),
              position = position_stack(vjust = 0.5),
              size = 5,
              color = "black") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = kolory_organizacji, drop = FALSE) +
    labs(title = tytul, x = NULL, y = NULL, fill = "Kolor") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

wykres_cechy <- utworz_wykres_filaru(dane, "filar_cechy", "Filar: Cechy organizacji")
wykres_hierarchia <- utworz_wykres_filaru(dane, "filar_hierarchia", "Filar: Hierarchia")
wykres_stres <- utworz_wykres_filaru(dane, "filar_stres", "Filar: Stres")

# Wyświetl jeden z wykresów, np.:
print(wykres_cechy)
print(wykres_hierarchia)
print(wykres_stres)
# Sprawdź rozkłady dla każdego filaru
table(dane$filar_cechy)
prop.table(table(dane$filar_cechy)) * 100

table(dane$filar_hierarchia) 
prop.table(table(dane$filar_hierarchia)) * 100

table(dane$filar_stres)
prop.table(table(dane$filar_stres)) * 100

library(patchwork)
wykres_cechy + wykres_hierarchia + wykres_stres +
  plot_layout(ncol = 3) +
  plot_annotation(title = "Dominujące kolory w trzech filarach organizacji")

kolumny_filarowe <- c("filar_cechy", "filar_hierarchia", "filar_stres")

kolory_po_filarach <- sapply(kolumny_filarowe, function(kol) {
  names(sort(table(dane[[kol]]), decreasing = TRUE))[1]
})

kolory_filar_df <- data.frame(
  Filar = names(kolory_po_filarach),
  Dominujacy_Kolor = kolory_po_filarach,
  Kod_Heks = kolory_organizacji[kolory_po_filarach],
  row.names = NULL
)

print(kolory_filar_df)

##################ANALIZA STABILNOŚCI###########################
print("\nANALIZA STABILNOŚCI STUDENTÓW (ZGODNOŚĆ KOLORÓW)")
print("============================================")

stabilność_cechy_hierarchia <- analiza_stabilności(
  dane, "filar_cechy", "filar_hierarchia", 
  "Stałość: Cechy vs Hierarchia"
)

stabilność_cechy_stres <- analiza_stabilności(
  dane, "filar_cechy", "filar_stres", 
  "Stałość: Cechy vs Stres"
)

stabilność_hierarchia_stres <- analiza_stabilności(
  dane, "filar_hierarchia", "filar_stres", 
  "Stałość: Hierarchia vs Stres"
)

# Wizualizacja macierzy korelacji
library(corrplot)

# Wykres macierzy korelacji
corrplot(macierz_gamma, 
         method = "color", 
         type = "full",
         order = "original",
         tl.cex = 0.8,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.8,
         title = "Macierz współczynników gamma między filarami",
         mar = c(0,0,2,0))

# Podsumowanie wyników
print("\nPODSUMOWANIE WYNIKÓW")
print("===================")
cat("Współczynniki gamma (siła związku):\n")
cat("- Cechy ↔ Hierarchia:", round(macierz_gamma[1,2], 3), "\n")
cat("- Cechy ↔ Stres:", round(macierz_gamma[1,3], 3), "\n")
cat("- Hierarchia ↔ Stres:", round(macierz_gamma[2,3], 3), "\n\n")

cat("Procenty zgodności (zgodnosc):\n")
cat("- Cechy ↔ Hierarchia:", round(stabilność_cechy_hierarchia, 1), "%\n")
cat("- Cechy ↔ Stres:", round(stabilność_cechy_stres, 1), "%\n")
cat("- Hierarchia ↔ Stres:", round(stabilność_hierarchia_stres, 1), "%\n")

# Interpretacja współczynników gamma
interpretuj_gamma <- function(gamma) {
  abs_gamma <- abs(gamma)
  if (abs_gamma < 0.1) return("bardzo słaby")
  if (abs_gamma < 0.3) return("słaby")
  if (abs_gamma < 0.5) return("umiarkowany")
  if (abs_gamma < 0.7) return("silny")
  return("bardzo silny")
}

print("\nINTERPRETACJA WSPÓŁCZYNNIKÓW GAMMA:")
print("==================================")
cat("- Cechy ↔ Hierarchia:", interpretuj_gamma(macierz_gamma[1,2]), "związek\n")
cat("- Cechy ↔ Stres:", interpretuj_gamma(macierz_gamma[1,3]), "związek\n")
cat("- Hierarchia ↔ Stres:", interpretuj_gamma(macierz_gamma[2,3]), "związek\n")


# WYKRESY STABILNOŚCI ODPOWIEDZI STUDENTÓW
# ========================================


# Przygotowanie danych do wykresów
library(ggplot2)
library(gridExtra)
library(reshape2)

# Dane do wykresów
dane_stabilnosc <- data.frame(
  Para_filarow = c("Cechy ↔ Hierarchia", "Cechy ↔ Stres", "Hierarchia ↔ Stres"),
  Procent_zgodnosci = c(28.2, 18.7, 19.1),
  Wspolczynnik_gamma = c(0.224, -0.135, 0.134),
  Liczba_zgodnych = c(59, 39, 40),
  Calkowita_liczba = c(209, 209, 209)
)
# WYKRES 1: Słupkowy - Procenty zgodności
wykres_procenty <- ggplot(dane_stabilnosc, aes(x = Para_filarow, y = Procent_zgodnosci)) +
  geom_col(fill = c("#3498db", "#e74c3c", "#f39c12"), alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(Procent_zgodnosci, "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Stabilność odpowiedzi studentów",
       subtitle = "Procent zgodności kolorów między filarami",
       x = "Pary filarów",
       y = "Procent zgodności (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 35)

# WYKRES 2: Punktowy - Współczynniki gamma
wykres_gamma <- ggplot(dane_stabilnosc, aes(x = Para_filarow, y = Wspolczynnik_gamma)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_point(size = 4, color = c("#3498db", "#e74c3c", "#f39c12")) +
  geom_text(aes(label = round(Wspolczynnik_gamma, 3)), 
            vjust = -1.2, size = 4, fontface = "bold") +
  labs(title = "Współczynniki gamma między filarami",
       subtitle = "Siła i kierunek związku (γ)",
       x = "Pary filarów",
       y = "Współczynnik gamma (γ)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  ylim(-0.25, 0.35)

# WYKRES 3: Kombinowany - Oba wskaźniki
dane_kombinowane <- melt(dane_stabilnosc[,c("Para_filarow", "Procent_zgodnosci", "Wspolczynnik_gamma")], 
                         id.vars = "Para_filarow")

# Normalizacja danych do tego samego zakresu (0-1)
dane_kombinowane$value_norm <- ifelse(dane_kombinowane$variable == "Procent_zgodnosci",
                                      dane_kombinowane$value / 100,
                                      (dane_kombinowane$value + 1) / 2)

wykres_kombinowany <- ggplot(dane_kombinowane, aes(x = Para_filarow, y = value_norm, 
                                                   fill = variable)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  scale_fill_manual(values = c("#2ecc71", "#9b59b6"),
                    labels = c("Procent zgodności", "Współczynnik gamma (norm.)")) +
  labs(title = "Porównanie wskaźników stabilności",
       subtitle = "Procent zgodności vs współczynnik gamma (znormalizowany)",
       x = "Pary filarów",
       y = "Wartość znormalizowana (0-1)",
       fill = "Wskaźnik") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# WYKRES 4: Heatmapa stabilności
macierz_stabilnosc <- matrix(c(100, 28.2, 18.7,
                               28.2, 100, 19.1,
                               18.7, 19.1, 100), 
                             nrow = 3, ncol = 3,
                             dimnames = list(c("Cechy", "Hierarchia", "Stres"),
                                             c("Cechy", "Hierarchia", "Stres")))

# Przekształcenie macierzy do formatu długiego
heatmap_data <- melt(macierz_stabilnosc)
names(heatmap_data) <- c("Filar1", "Filar2", "Zgodnosc")

wykres_heatmap <- ggplot(heatmap_data, aes(x = Filar1, y = Filar2, fill = Zgodnosc)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(Zgodnosc == 100, "100%", paste0(Zgodnosc, "%"))), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#e74c3c", mid = "#f39c12", high = "#27ae60",
                       midpoint = 50, name = "Zgodność\n(%)") +
  labs(title = "Mapa cieplna stabilności odpowiedzi",
       subtitle = "Procent zgodności kolorów między filarami",
       x = "Filar", y = "Filar") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  ) +
  coord_equal()

# WYKRES 5: Wykres kołowy - rozkład zgodności
dane_kolowy <- data.frame(
  Kategoria = c("Zgodne odpowiedzi", "Niezgodne odpowiedzi"),
  Srednia_zgodnosc = c(mean(dane_stabilnosc$Procent_zgodnosci), 
                       100 - mean(dane_stabilnosc$Procent_zgodnosci))
)

wykres_kolowy <- ggplot(dane_kolowy, aes(x = "", y = Srednia_zgodnosc, fill = Kategoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#e74c3c","#27ae60")) +
  labs(title = "Średni rozkład stabilności odpowiedzi",
       subtitle = paste0("Średnia zgodność: ", round(mean(dane_stabilnosc$Procent_zgodnosci), 1), "%")) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = paste0(round(Srednia_zgodnosc, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold")

# Wyświetlenie wszystkich wykresów
print("WYKRESY STABILNOŚCI ODPOWIEDZI STUDENTÓW")
print("=======================================")
print("")

# Pojedyncze wykresy
print(wykres_procenty)
print(wykres_gamma)
print(wykres_kombinowany)
print(wykres_heatmap)
print(wykres_kolowy)

# Panel z wieloma wykresami
panel_wykresow <- grid.arrange(
  wykres_procenty, wykres_gamma,
  wykres_kombinowany, wykres_heatmap,
  ncol = 2, nrow = 2,
  top = "Panel analiz stabilności odpowiedzi studentów"
)

# Dodatkowe statystyki opisowe
print("\nDODAT KOWE STATYSTYKI:")
print("=====================")
cat("Średnia zgodność:", round(mean(dane_stabilnosc$Procent_zgodnosci), 2), "%\n")
cat("Odchylenie standardowe zgodności:", round(sd(dane_stabilnosc$Procent_zgodnosci), 2), "%\n")
cat("Mediana zgodności:", round(median(dane_stabilnosc$Procent_zgodnosci), 2), "%\n")
cat("Zakres zgodności:", round(min(dane_stabilnosc$Procent_zgodnosci), 1), "% -", 
    round(max(dane_stabilnosc$Procent_zgodnosci), 1), "%\n")

cat("\nŚredni współczynnik gamma:", round(mean(abs(dane_stabilnosc$Wspolczynnik_gamma)), 3), "\n")
cat("Najsilniejszy związek:", round(max(abs(dane_stabilnosc$Wspolczynnik_gamma)), 3), 
    "(", dane_stabilnosc$Para_filarow[which.max(abs(dane_stabilnosc$Wspolczynnik_gamma))], ")\n")

# Interpretacja wyników
print("\nINTERPRETACJA WYNIKÓW:")
print("=====================")
cat("• Najwyższa stabilność: Cechy ↔ Hierarchia (28.2%)\n")
cat("• Najniższa stabilność: Cechy ↔ Stres (18.7%)\n")
cat("• Ogólna stabilność jest niska (średnio", round(mean(dane_stabilnosc$Procent_zgodnosci), 1), "%)\n")
cat("• Wszystkie współczynniki gamma są słabe (|γ| < 0.3)\n")
cat("• Sugeruje to różnorodność w odpowiedziach studentów\n")


# Przygotowanie danych porządkowych
dane <- dane %>%
  mutate(
    filar_cechy_ord = as.numeric(konwertuj_na_porzadkowe(filar_cechy)),
    filar_hierarchia_ord = as.numeric(konwertuj_na_porzadkowe(filar_hierarchia)),
    filar_stres_ord = as.numeric(konwertuj_na_porzadkowe(filar_stres))
  ) %>%
  filter(!is.na(filar_cechy) & !is.na(filar_hierarchia) & !is.na(filar_stres))

# Definiowanie hierarchii kolorów
hierarchia_kolorow <- c("czerwona", "bursztynowa", "pomarańczowa", "zielona", "turkusowa")

# Funkcja do konwersji kolorów na wartości porządkowe
konwertuj_na_porzadkowe <- function(kolory) {
  factor(kolory, levels = hierarchia_kolorow, ordered = TRUE)
}


# Dodane: testy istotności, przedziały ufności, kontrola błędów

# Obliczenie gamma + test istotności dla każdej pary
gamma_cechy_hierarchia <- oblicz_gamma_z_testem(dane$filar_cechy_ord, dane$filar_hierarchia_ord)
gamma_cechy_stres     <- oblicz_gamma_z_testem(dane$filar_cechy_ord, dane$filar_stres_ord)
gamma_hierarchia_stres <- oblicz_gamma_z_testem(dane$filar_hierarchia_ord, dane$filar_stres_ord)
print("\nTESTY ISTOTNOŚCI I PRZEDZIAŁY UFNOŚCI")
print("=====================================")

cat("- Cechy ↔ Hierarchia:\n")
cat("  gamma =", round(gamma_cechy_hierarchia$gamma, 3), 
    ", p =", round(gamma_cechy_hierarchia$p_value, 4),
    ", 95% CI [", round(gamma_cechy_hierarchia$ci_lower, 3), ",", round(gamma_cechy_hierarchia$ci_upper, 3), "]\n")

cat("- Cechy ↔ Stres:\n")
cat("  gamma =", round(gamma_cechy_stres$gamma, 3), 
    ", p =", round(gamma_cechy_stres$p_value, 4),
    ", 95% CI [", round(gamma_cechy_stres$ci_lower, 3), ",", round(gamma_cechy_stres$ci_upper, 3), "]\n")

cat("- Hierarchia ↔ Stres:\n")
cat("  gamma =", round(gamma_hierarchia_stres$gamma, 3), 
    ", p =", round(gamma_hierarchia_stres$p_value, 4),
    ", 95% CI [", round(gamma_hierarchia_stres$ci_lower, 3), ",", round(gamma_hierarchia_stres$ci_upper, 3), "]\n")

#####Analiza satbilności studentów między pytaniem 7 a dominuącym kolorem w profilu wierszowym#############################
# Hierarchia kolorów 
hierarchia_kolorow <- c("czerwona", "bursztynowa", "pomarańczowa", "zielona", "turkusowa")

# Funkcja do analizy zgodności kolorów
analiza_zgodnosci_kontrola <- function(dane, kolumna_sredni, kolumna_kontrola, nazwa_analizy) {
  
  # Filtrowanie danych (usunięcie NA)
  dane_filtrowane <- dane %>%
    filter(!is.na(!!sym(kolumna_sredni)) & !is.na(!!sym(kolumna_kontrola)))
  
  # Obliczenie zgodności
  zgodne <- sum(dane_filtrowane[[kolumna_sredni]] == dane_filtrowane[[kolumna_kontrola]])
  calkowite <- nrow(dane_filtrowane)
  procent_zgodnosci <- (zgodne / calkowite) * 100
  
  # Wyświetlenie wyników
  cat("\n", nazwa_analizy, ":\n")
  cat("- Liczba zgodnych odpowiedzi:", zgodne, "\n")
  cat("- Całkowita liczba odpowiedzi:", calkowite, "\n")
  cat("- Procent zgodności:", round(procent_zgodnosci, 2), "%\n")
  
  return(procent_zgodnosci)
}

# Funkcja do obliczania gamma z testami
oblicz_gamma_z_testem_kontrola <- function(x, y) {
  # Konwersja na rangi
  x_rang <- as.numeric(factor(x, levels = hierarchia_kolorow, ordered = TRUE))
  y_rang <- as.numeric(factor(y, levels = hierarchia_kolorow, ordered = TRUE))
  
  # Usunięcie par z NA
  kompletne <- complete.cases(x_rang, y_rang)
  x_rang <- x_rang[kompletne]
  y_rang <- y_rang[kompletne]
  
  # Obliczenie tabeli kontyngencji
  tabela <- table(x_rang, y_rang)
  
  # Obliczenie gamma
  n <- sum(tabela)
  P <- 0  # Pary zgodne
  Q <- 0  # Pary niezgodne
  
  for(i in 1:(nrow(tabela)-1)) {
    for(j in 1:(ncol(tabela)-1)) {
      for(k in (i+1):nrow(tabela)) {
        for(l in (j+1):ncol(tabela)) {
          P <- P + tabela[i,j] * tabela[k,l]
        }
      }
    }
  }
  
  for(i in 1:(nrow(tabela)-1)) {
    for(j in 2:ncol(tabela)) {
      for(k in (i+1):nrow(tabela)) {
        for(l in 1:(j-1)) {
          Q <- Q + tabela[i,j] * tabela[k,l]
        }
      }
    }
  }
  
  gamma <- ifelse(P + Q == 0, 0, (P - Q) / (P + Q))
  
  # Błąd standardowy gamma
  if(P + Q > 0) {
    se_gamma <- sqrt(4 * P * Q * (P + Q - (P - Q)^2 / (P + Q))) / (P + Q)^2
    z_stat <- gamma / se_gamma
    p_value <- 2 * (1 - pnorm(abs(z_stat)))
    ci_lower <- gamma - 1.96 * se_gamma
    ci_upper <- gamma + 1.96 * se_gamma
  } else {
    se_gamma <- 0
    z_stat <- 0
    p_value <- 1
    ci_lower <- 0
    ci_upper <- 0
  }
  
  return(list(
    gamma = gamma,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n = n,
    P = P,
    Q = Q
  ))
}

# Analiza zgodności głównej
zgodnosc_sredni_kontrola <- analiza_zgodnosci_kontrola(
  dane, "kolor_sredni", "pyt7_firma", 
  "Zgodność: Kolor średni vs Pytanie kontrolne"
)

# Analiza korelacji rangowej (gamma)
dane_kompletne <- dane %>%
  filter(!is.na(kolor_sredni) & !is.na(pyt7_firma))

gamma_wynik <- oblicz_gamma_z_testem_kontrola(dane_kompletne$kolor_sredni, dane_kompletne$pyt7_firma)

print("\nANALIZA KORELACJI RANGOWEJ (GAMMA)")
print("==================================")
cat("Współczynnik gamma:", round(gamma_wynik$gamma, 3), "\n")
cat("Wartość p:", round(gamma_wynik$p_value, 4), "\n")
cat("95% CI: [", round(gamma_wynik$ci_lower, 3), ",", round(gamma_wynik$ci_upper, 3), "]\n")
cat("Liczba par:", gamma_wynik$n, "\n")
cat("Pary zgodne (P):", gamma_wynik$P, "\n")
cat("Pary niezgodne (Q):", gamma_wynik$Q, "\n")

# Interpretacja gamma
interpretuj_gamma <- function(gamma) {
  abs_gamma <- abs(gamma)
  if (abs_gamma < 0.1) return("bardzo słaby")
  if (abs_gamma < 0.3) return("słaby")
  if (abs_gamma < 0.5) return("umiarkowany")
  if (abs_gamma < 0.7) return("silny")
  return("bardzo silny")
}

cat("Interpretacja:", interpretuj_gamma(gamma_wynik$gamma), "związek\n")

# Test istotności
if(gamma_wynik$p_value < 0.05) {
  cat("Związek jest statystycznie istotny (p < 0.05)\n")
} else {
  cat("Związek NIE jest statystycznie istotny (p >= 0.05)\n")
}

# Wizualizacja wyników
library(ggplot2)

# Obliczenie liczby zgodnych i niezgodnych
zgodne_liczba <- sum(dane$kolor_sredni == dane$pyt7_firma, na.rm = TRUE)
niezgodne_liczba <- sum(dane$kolor_sredni != dane$pyt7_firma, na.rm = TRUE)

# Dane do wykresu
dane_wykres <- data.frame(
  Kategoria = c("Zgodne odpowiedzi", "Niezgodne odpowiedzi"),
  Liczba = c(zgodne_liczba, niezgodne_liczba),
  Procent = c(zgodnosc_sredni_kontrola, 100 - zgodnosc_sredni_kontrola)
)

# Wykres słupkowy
wykres_zgodnosci <- ggplot(dane_wykres, aes(x = Kategoria, y = Procent, fill = Kategoria)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Procent, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#27ae60", "#e74c3c")) +
  labs(title = "Zgodność między kolorem średnim a pytaniem kontrolnym",
       subtitle = "Analiza spójności odpowiedzi respondentów",
       x = "Kategoria odpowiedzi",
       y = "Procent (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(dane_wykres$Procent) * 1.1)

print(wykres_zgodnosci)

# Tabela kontyngencji
tabela_kontyngencji <- table(dane$kolor_sredni, dane$pyt7_firma, useNA = "ifany")

print("\nTABELA KONTYNGENCJI")
print("==================")
print(tabela_kontyngencji)

# Proporcje wierszowe
print("\nPROPORCJE WIERSZOWE (%)")
print("======================")
proporcje <- prop.table(tabela_kontyngencji, 1) * 100
print(round(proporcje, 1))

# Mapa cieplna zgodności
library(reshape2)
heatmap_data <- melt(tabela_kontyngencji)
names(heatmap_data) <- c("Kolor_sredni", "Pyt7_firma", "Liczba")

wykres_heatmap <- ggplot(heatmap_data, aes(x = Kolor_sredni, y = Pyt7_firma, fill = Liczba)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Liczba), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#e74c3c", high = "#27ae60", name = "Liczba\nrespond.") +
  labs(title = "Mapa cieplna: Kolor dominujący vs Pytanie kontrolne",
       subtitle = "Liczba respondentów w każdej kombinacji kolorów",
       x = "Kolor dominujący", y = "Pytanie kontrolne (pyt7_firma)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()

print(wykres_heatmap)

# TABELA PODSUMOWUJĄCA WG KOLORÓW FIRMY
# ====================================

# Obliczanie statystyk dla każdego koloru firmy
tabela_kolorow <- dane %>%
  filter(!is.na(pyt7_firma) & !is.na(kolor_sredni)) %>%
  group_by(pyt7_firma) %>%
  summarise(
    Liczba_studentow = n(),
    Zgodnosc_procent = round(mean(kolor_sredni == pyt7_firma) * 100, 1),
    .groups = 'drop'
  )

# Obliczanie średniej stabilności (jako przykład - można dostosować)
# Zakładam że chcesz stabilność jako odwrotność rozproszenia odpowiedzi
srednia_stabilnosc <- dane %>%
  filter(!is.na(pyt7_firma) & !is.na(kolor_sredni)) %>%
  group_by(pyt7_firma) %>%
  summarise(
    # Stabilność jako zgodność podzielona przez 100 (żeby mieć wartości 0-1)
    Srednia_stabilnosc = round(mean(kolor_sredni == pyt7_firma), 3),
    .groups = 'drop'
  )

# Łączenie tabel
tabela_finalna <- merge(tabela_kolorow, srednia_stabilnosc, by = "pyt7_firma")

# Sortowanie według kolejności hierarchii kolorów
tabela_finalna$pyt7_firma <- factor(tabela_finalna$pyt7_firma, 
                                    levels = hierarchia_kolorow, 
                                    ordered = TRUE)
tabela_finalna <- tabela_finalna[order(tabela_finalna$pyt7_firma),]

# Zmiana nazw kolumn
names(tabela_finalna) <- c("Kolory_firmy", "Liczba_studentow", "Zgodnosc_cechy_procent", "Srednia_stabilnosc")

# Wyświetlenie tabeli
print("\nTABELA PODSUMOWUJĄCA WG KOLORÓW FIRMY")
print("====================================")
print(tabela_finalna)

# Sformatowana tabela do łatwego kopiowania
print("\nSFORMATOWANA TABELA (do kopiowania):")
print("===================================")
cat("Kolory firmy\t\tLiczba studentów\tŚrednia stabilność\tZgodność cechy (%)\n")
cat("--------------------------------------------------------------------------\n")
for(i in 1:nrow(tabela_finalna)) {
  cat(sprintf("%-12s\t\t%d\t\t\t%.3f\t\t\t%.1f\n", 
              tabela_finalna$Kolory_firmy[i],
              tabela_finalna$Liczba_studentow[i],
              tabela_finalna$Srednia_stabilnosc[i],
              tabela_finalna$Zgodnosc_cechy_procent[i]))
}

# Dodatkowe statystyki
print("\nDODAT KOWE STATYSTYKI:")
print("=====================")
cat("Najwyższa zgodność:", max(tabela_finalna$Zgodnosc_cechy_procent), "% (", 
    tabela_finalna$Kolory_firmy[which.max(tabela_finalna$Zgodnosc_cechy_procent)], ")\n")
cat("Najniższa zgodność:", min(tabela_finalna$Zgodnosc_cechy_procent), "% (", 
    tabela_finalna$Kolory_firmy[which.min(tabela_finalna$Zgodnosc_cechy_procent)], ")\n")
cat("Średnia zgodność:", round(mean(tabela_finalna$Zgodnosc_cechy_procent), 1), "%\n")
cat("Najwyższa stabilność:", max(tabela_finalna$Srednia_stabilnosc), 
    "(", tabela_finalna$Kolory_firmy[which.max(tabela_finalna$Srednia_stabilnosc)], ")\n")

# Podsumowanie końcowe
print("\nPODSUMOWANIE WYNIKÓW")
print("===================")
cat("Procent zgodności kolorów:", round(zgodnosc_sredni_kontrola, 2), "%\n")
cat("Liczba zgodnych odpowiedzi:", zgodne_liczba, "z", zgodne_liczba + niezgodne_liczba, "\n")
cat("Korelacja rangowa (gamma):", round(gamma_wynik$gamma, 3), "\n")
cat("Siła związku:", interpretuj_gamma(gamma_wynik$gamma), "\n")
cat("Istotność statystyczna:", ifelse(gamma_wynik$p_value < 0.05, "TAK", "NIE"), "\n")

print("\nINTERPRETACJA:")
print("=============")
if(zgodnosc_sredni_kontrola > 50) {
  cat("• Wysoka zgodność - respondenci są spójni w swoich odpowiedziach\n")
} else if(zgodnosc_sredni_kontrola > 30) {
  cat("• Umiarkowana zgodność - część respondentów jest spójna\n")
} else {
  cat("• Niska zgodność - respondenci nie są spójni w swoich odpowiedziach\n")
}

if(abs(gamma_wynik$gamma) > 0.3) {
  cat("• Istnieje zauważalny związek między kolorami średnimi a wyborem firmy\n")
} else {
  cat("• Słaby związek między kolorami średnimi a wyborem firmy\n")
#####################################KORELACJE MIĘDZY PYTANIAMI#############################3


# Funkcja interpretacji siły korelacji
interpretuj_spearmana <- function(r) {
  r_abs <- abs(r)
  if (r_abs < 0.1) return("bardzo słaby")
  if (r_abs < 0.3) return("słaby") 
  if (r_abs < 0.5) return("umiarkowany")
  if (r_abs < 0.7) return("silny")
  return("bardzo silny")
}# PRZEKSZTAŁCENIE KOLORÓW NA RANGI PORZĄDKOWE
# ============================================

print("PRZEKSZTAŁCANIE KOLORÓW NA RANGI PORZĄDKOWE")
print("==========================================")

# 1. DEFINICJA HIERARCHII KOLORÓW (ZGODNIE Z TWOIMI DANYMI)
mapa_kolorow <- c(
  "czerwona" = 1,      # najniższa wartość
  "bursztynowa" = 2, 
  "pomarańczowa" = 3,
  "zielona" = 4,
  "turkusowa" = 5      # najwyższa wartość
)

# Dodatkowe mapowanie dla możliwych wariantów (z końcówkami męskimi)
mapa_kolorow_alternatywne <- c(
  "czerwony" = 1,
  "bursztynowy" = 2, 
  "pomarańczowy" = 3,
  "zielony" = 4,
  "turkusowy" = 5
)

print("HIERARCHIA KOLORÓW (DOKŁADNIE WEDŁUG TWOICH DANYCH):")
print("==================================================")
cat("czerwona → 1 (najniższa wartość)\n")
cat("bursztynowa → 2\n") 
cat("pomarańczowa → 3\n")
cat("zielona → 4\n")
cat("turkusowa → 5 (najwyższa wartość)\n")
print("")

# Sprawdzenie, jakie dokładnie kolory są w danych
print("SPRAWDZENIE KOLORÓW W TWOICH DANYCH:")
print("====================================")
cat("Unikalne wartości w pyt7_firma:\n")
print(sort(unique(dane$pyt7_firma)))
print("")

# Sprawdź też filary jeśli istnieją
if("filar_cechy" %in% names(dane)) {
  cat("Unikalne wartości w filar_cechy:\n")
  print(sort(unique(dane$filar_cechy)))
  print("")
}
if("filar_hierarchia" %in% names(dane)) {
  cat("Unikalne wartości w filar_hierarchia:\n")
  print(sort(unique(dane$filar_hierarchia)))
  print("")
}
if("filar_stres" %in% names(dane)) {
  cat("Unikalne wartości w filar_stres:\n")
  print(sort(unique(dane$filar_stres)))
  print("")
}

# 2. SPRAWDZENIE ORYGINALNYCH DANYCH
print("ORYGINALNE DANE PYTANIE 7:")
print("==========================")
cat("Typ kolumny pyt7_firma:", class(dane$pyt7_firma), "\n")
cat("Liczba obserwacji:", length(dane$pyt7_firma), "\n")
print("")

# Rozkład oryginalnych kolorów
print("ROZKŁAD KOLORÓW (ORYGINAŁ):")
print("============================")
tabela_orig <- table(dane$pyt7_firma, useNA = "ifany")
print(tabela_orig)
print("")

# 3. FUNKCJA KONWERSJI KOLORÓW NA RANGI (DOSTOSOWANA DO TWOICH DANYCH)
konwertuj_kolory <- function(kolumna_kolorow) {
  if(is.character(kolumna_kolorow)) {
    wynik <- numeric(length(kolumna_kolorow))
    
    for(i in 1:length(kolumna_kolorow)) {
      # Normalizuj tekst: małe litery, usuń spacje i polskie znaki
      kolor <- tolower(trimws(kolumna_kolorow[i]))
      
      # Sprawdź różne możliwe warianty
      if(kolor %in% c("czerwona", "czerwony")) {
        wynik[i] <- 1
      } else if(kolor %in% c("bursztynowa", "bursztynowy")) {
        wynik[i] <- 2
      } else if(kolor %in% c("pomarańczowa", "pomarańczowy", "pomaranczowa", "pomaranczowy")) {
        wynik[i] <- 3
      } else if(kolor %in% c("zielona", "zielony")) {
        wynik[i] <- 4
      } else if(kolor %in% c("turkusowa", "turkusowy")) {
        wynik[i] <- 5
      } else if(kolor == "" || is.na(kolor)) {
        wynik[i] <- NA
      } else {
        wynik[i] <- NA
        warning(paste("Nieznany kolor:", kolumna_kolorow[i], "- przypisano NA"))
      }
    }
    return(wynik)
  } else if(is.numeric(kolumna_kolorow)) {
    # Jeśli już są numeryczne, sprawdź czy w zakresie 1-5
    wynik <- as.numeric(kolumna_kolorow)
    wynik[wynik < 1 | wynik > 5] <- NA
    return(wynik)
  } else {
    warning("Nieobsługiwany typ danych")
    return(rep(NA, length(kolumna_kolorow)))
  }
}

# 4. PRZEKSZTAŁCENIE DANYCH
print("PRZEKSZTAŁCANIE DANYCH:")
print("=======================")

# Konwersja Pytania 7
dane$pyt7_firma_rang <- konwertuj_kolory(dane$pyt7_firma)

# Sprawdzenie, które kolumny filarów faktycznie istnieją w danych
filary_kolumny <- c("filar_cechy", "filar_hierarchia", "filar_stres")
istniejace_filary <- filary_kolumny[filary_kolumny %in% names(dane)]

cat("Dostępne kolumny filarów:", paste(istniejace_filary, collapse = ", "), "\n")

# Konwersja tylko istniejących kolumn
for(filar in istniejace_filary) {
  nowa_nazwa <- paste0(filar, "_rang")
  dane[[nowa_nazwa]] <- konwertuj_kolory(dane[[filar]])
  cat("Przekształcono:", filar, "→", nowa_nazwa, "\n")
}

# 5. SPRAWDZENIE WYNIKÓW KONWERSJI
print("WYNIKI KONWERSJI:")
print("=================")

# Porównanie przed i po
print("PYTANIE 7 - PRZED I PO KONWERSJI:")
porownanie <- data.frame(
  Oryginal = dane$pyt7_firma[1:min(10, nrow(dane))],
  Rang = dane$pyt7_firma_rang[1:min(10, nrow(dane))]
)
print(porownanie)
print("")

# Rozkład po konwersji
print("ROZKŁAD RANG PYTANIE 7:")
print("========================")
tabela_rang <- table(dane$pyt7_firma_rang, useNA = "ifany")
prop_rang <- prop.table(tabela_rang) * 100

for(i in 1:length(tabela_rang)) {
  if(!is.na(names(tabela_rang)[i])) {
    ranga <- as.numeric(names(tabela_rang)[i])
    if(ranga %in% mapa_kolorow) {
      kolor <- names(mapa_kolorow)[mapa_kolorow == ranga]
      cat("Ranga", ranga, "(", kolor, "):", tabela_rang[i], 
          "obs. (", round(prop_rang[i], 1), "%)\n")
    }
  }
}

# Statystyki braków danych
cat("\nBraki danych po konwersji:", sum(is.na(dane$pyt7_firma_rang)), "\n")
cat("Procent braków:", round(mean(is.na(dane$pyt7_firma_rang)) * 100, 1), "%\n")
print("")

# 6. PRZYGOTOWANIE DANYCH DO KORELACJI SPEARMANA
# Dynamiczne tworzenie ramki danych tylko z istniejącymi kolumnami
dane_korelacji <- data.frame(
  Pytanie_7 = dane$pyt7_firma_rang
)

# Dodaj filary, które faktycznie istnieją
if("filar_cechy_rang" %in% names(dane)) {
  dane_korelacji$Filar_Cechy <- dane$filar_cechy_rang
}
if("filar_hierarchia_rang" %in% names(dane)) {
  dane_korelacji$Filar_Hierarchia <- dane$filar_hierarchia_rang
}
if("filar_stres_rang" %in% names(dane)) {
  dane_korelacji$Filar_Stres <- dane$filar_stres_rang
}

cat("Kolumny w ramce do analizy:", paste(names(dane_korelacji), collapse = ", "), "\n")

print("PODSUMOWANIE DANYCH DO ANALIZY:")
print("===============================")
print(summary(dane_korelacji))
print("")

# Sprawdzenie braków w każdej zmiennej
braki <- sapply(dane_korelacji, function(x) sum(is.na(x)))
cat("Braki danych w każdej zmiennej:\n")
print(braki)
print("")

# 7. TERAZ MOŻNA ZASTOSOWAĆ KORELACJĘ SPEARMANA
print("DANE GOTOWE DO ANALIZY SPEARMANA!")
print("=================================")
cat("✓ Wszystkie zmienne są teraz porządkowe (rangi 1-5)\n")
cat("✓ Można użyć korelacji Spearmana\n")
cat("✓ Hierarchia kolorów zachowana: czerwony(1) < bursztynowy(2) < pomarańczowy(3) < zielony(4) < turkusowy(5)\n")
print("")

# Przykład prostej korelacji
if(!all(is.na(dane_korelacji$Pytanie_7)) && !all(is.na(dane_korelacji$Filar_Cechy))) {
  library(Hmisc)
  przyklad_korelacji <- rcorr(dane_korelacji$Pytanie_7, dane_korelacji$Filar_Cechy, type = "spearman")
  cat("PRZYKŁAD - Korelacja Pytanie 7 vs Filar Cechy:\n")
  cat("ρ =", round(przyklad_korelacji$r[1,2], 3), "\n")
  cat("p =", round(przyklad_korelacji$P[1,2], 4), "\n")
}

# 8. KOMPLETNA ANALIZA KORELACJI SPEARMANA
print("ANALIZA KORELACJI SPEARMANA")
print("===========================")

# Usuń braki danych
dane_korelacji_clean <- na.omit(dane_korelacji)
cat("Obserwacje po usunięciu braków:", nrow(dane_korelacji_clean), "z", nrow(dane_korelacji), "\n")
print("")

if(nrow(dane_korelacji_clean) > 10) {  # Minimum obserwacji do analizy
  
  # Oblicz macierz korelacji
  library(corrplot)
  library(Hmisc)
  
  korelacja_matrix <- cor(dane_korelacji_clean, method = "spearman", use = "complete.obs")
  wyniki_testow <- rcorr(as.matrix(dane_korelacji_clean), type = "spearman")
  
  print("MACIERZ KORELACJI SPEARMANA:")
  print("============================")
  print(round(korelacja_matrix, 4))
  print("")
  
  # Dynamicznie twórz korelacje tylko dla istniejących filarów
  nazwy_filarow <- names(dane_korelacji_clean)[names(dane_korelacji_clean) != "Pytanie_7"]
  
  if(length(nazwy_filarow) > 0) {
    korelacje_pyt7 <- data.frame(
      Filar = gsub("Filar_", "", nazwy_filarow),
      Korelacja = sapply(nazwy_filarow, function(x) korelacja_matrix["Pytanie_7", x]),
      p_value = sapply(nazwy_filarow, function(x) wyniki_testow$P["Pytanie_7", x])
    )
    
    korelacje_pyt7$Istotnosc <- ifelse(korelacje_pyt7$p_value < 0.05, "TAK", "NIE")
    
    print("KORELACJE PYTANIE 7 vs FILARY:")
    print("===============================")
    print(korelacje_pyt7)
    print("")
    
    # Interpretacja zgodna z hierarchią kolorów
    print("INTERPRETACJA (czerwona=1 ← → turkusowa=5):")
    print("============================================")
    for(i in 1:nrow(korelacje_pyt7)) {
      r <- korelacje_pyt7$Korelacja[i]
      filar <- korelacje_pyt7$Filar[i]
      
      if(r > 0) {
        kierunek_opis <- "Dodatnia: im wyższy kolor w Pytaniu 7, tym wyższy w filarze"
      } else {
        kierunek_opis <- "Ujemna: im wyższy kolor w Pytaniu 7, tym niższy w filarze"
      }
      
      istotnosc <- ifelse(korelacje_pyt7$p_value[i] < 0.05, "ISTOTNY", "nieistotny")
      
      cat("Pytanie 7 ↔", filar, ":", round(r, 3), "\n")
      cat("  →", kierunek_opis, "\n")
      cat("  →", interpretuj_spearmana(r), "związek,", istotnosc, "\n")
      cat("  → p-wartość:", round(korelacje_pyt7$p_value[i], 4), "\n\n")
    }
    
    # Wizualizacja z lepszymi opisami
    library(ggplot2)
    
    wykres_korelacji <- ggplot(korelacje_pyt7, aes(x = Filar, y = Korelacja)) +
      geom_col(aes(fill = ifelse(Korelacja > 0, "Dodatnia", "Ujemna")), 
               alpha = 0.8, width = 0.6) +
      geom_text(aes(label = paste0(round(Korelacja, 3), 
                                   ifelse(p_value < 0.05, "*", ""))), 
                vjust = ifelse(korelacje_pyt7$Korelacja > 0, -0.5, 1.2), 
                size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Dodatnia" = "#27ae60", "Ujemna" = "#e74c3c")) +
      labs(title = "Korelacje Spearmana: Pytanie 7 vs Filary",
           subtitle = "Hierarchia: czerwona(1) → bursztynowa(2) → pomarańczowa(3) → zielona(4) → turkusowa(5)\n* = p < 0.05",
           x = "Filary badania",
           y = "Współczynnik korelacji Spearmana (ρ)",
           fill = "Kierunek korelacji") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.position = "bottom"
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
    
    print(wykres_korelacji)
    
  } else {
    cat("BŁĄD: Brak dostępnych filarów do analizy\n")
  }
  
} else {
  cat("BŁĄD: Za mało obserwacji do analizy (", nrow(dane_korelacji_clean), ")\n")
  cat("Sprawdź konwersję kolorów i braki danych.\n")
}

#####################################ANALIZA KOLORÓW W STOSUNKU DO DEMOGRAFII#####################################

# Dodaj na początku skryptu (jeśli jeszcze nie ma):
library(stringr)  # dla funkcji str_detect()
str(dane)
head(dane)

# Dostosuj nazwy kolumn do rzeczywistych nazw w danych
dane <- dane %>%
  rename(
    płeć   = `Płeć`,
    wiek = `Proszę podaj swój wiek`,
    rok_studiów = `Jestem na`
  )
kolumny_kolorow <- c("kolor_pyt1", "kolor_pyt2", "pyt3_dominujacy_kolor", "pyt4_dominujacy_kolor", 
                     "pyt5_stwierdzenia", "pyt6_stres_kolor", "pyt7_firma", "pyt8_cechy_hierarchia", "pyt9_hierarchia")

lepsze_nazwy <- c("Odporność na stres", "Struktura organizacji", "Cechy charakteru w pracy", "Wartości przedsiębiorstwa", 
                  "Stwierdzenia", "Rezygnacja przy stresie", "Wybór firmy", "Cechy hierarchii", "Przykłady hierarchii")
analiza_wedlug_plci <- function(dane) {
  dane_long <- dane %>%
    select(płeć, all_of(kolumny_kolorow)) %>%
    pivot_longer(
      cols = all_of(kolumny_kolorow),
      names_to = "Pytanie",
      values_to = "Kolor"
    )
  
  dane_long$Pytanie <- factor(dane_long$Pytanie, 
                              levels = kolumny_kolorow,
                              labels = lepsze_nazwy)
  
  udzialy <- dane_long %>%
    group_by(płeć, Pytanie, Kolor) %>%
    summarise(liczba = n(), .groups = "drop") %>%
    group_by(płeć, Pytanie) %>%
    mutate(procent = liczba / sum(liczba)) %>%
    ungroup()
  
  ggplot(udzialy, aes(x = Pytanie, y = procent, fill = Kolor)) +
    geom_col(position = "fill") +
    geom_text(aes(label = scales::percent(procent, accuracy = 0.1)),
              position = position_fill(vjust = 0.5),
              color = "black",  # zmieniono na biały dla lepszej widoczności
              size = 3,
              fontface = "bold") +
    facet_wrap(~płeć) +
    scale_fill_manual(values = kolory_organizacji) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    ) +
    labs(
      title = "Rozkład kolorów organizacji według płci",
      y = "Proporcja",
      x = NULL
    )
}

# Analiza kolorów według wieku z etykietami procentowymi
analiza_wedlug_wieku <- function(dane) {
  dane <- dane %>%
    mutate(wiek = as.numeric(wiek)) %>%
    filter(!is.na(wiek)) %>%
    mutate(kategoria_wieku = cut(wiek, 
                                 breaks = c(18, 22, 25, 30, 100),
                                 labels = c("18-22", "23-25", "26-30", "31+")))
  
  dane_long <- dane %>%
    select(kategoria_wieku, kolor_sredni) %>%
    filter(!is.na(kategoria_wieku) & !is.na(kolor_sredni))
  
  udzialy <- dane_long %>%
    group_by(kategoria_wieku, kolor_sredni) %>%
    summarise(liczba = n(), .groups = "drop") %>%
    group_by(kategoria_wieku) %>%
    mutate(procent = liczba / sum(liczba)) %>%
    ungroup()
  
  ggplot(udzialy, aes(x = kategoria_wieku, y = procent, fill = kolor_sredni)) +
    geom_col(position = "fill") +
    geom_text(aes(label = scales::percent(procent, accuracy = 0.1)),
              position = position_fill(vjust = 0.5),
              color = "black",
              size = 3,
              fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = kolory_organizacji) +
    theme_minimal() +
    labs(
      title = "Rozkład dominujących kolorów organizacji według wieku",
      y = "Procent",
      x = "Kategoria wieku",
      fill = "Kolor organizacji"
    )
}

# Analiza kolorów według roku studiów z etykietami procentowymi
analiza_wedlug_roku <- function(dane) {
  dane <- dane %>%
    mutate(rok_studiów = case_when(
      str_detect(rok_studiów, "1\\. roku studiów licencjackich") ~ "1 lic.",
      str_detect(rok_studiów, "2\\. roku studiów licencjackich") ~ "2 lic.",
      str_detect(rok_studiów, "3\\. roku studiów licencjackich") ~ "3 lic.",
      str_detect(rok_studiów, "1\\. roku studiów magisterskich") ~ "1 mgr",
      str_detect(rok_studiów, "2\\. roku studiów magisterskich") ~ "2 mgr",
      TRUE ~ NA_character_
    ))
  
  dane_long <- dane %>%
    select(rok_studiów, kolor_sredni) %>%
    filter(!is.na(rok_studiów) & !is.na(kolor_sredni))
  
  udzialy <- dane_long %>%
    group_by(rok_studiów, kolor_sredni) %>%
    summarise(liczba = n(), .groups = "drop") %>%
    group_by(rok_studiów) %>%
    mutate(procent = liczba / sum(liczba)) %>%
    ungroup()
  
  ggplot(udzialy, aes(x = factor(rok_studiów, levels = c("1 lic.", "2 lic.", "3 lic.", "1 mgr", "2 mgr")), 
                      y = procent, fill = kolor_sredni)) +
    geom_col(position = "fill") +
    geom_text(aes(label = scales::percent(procent, accuracy = 0.1)),
              position = position_fill(vjust = 0.5),
              color = "black",
              size = 3,
              fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = kolory_organizacji) +
    theme_minimal() +
    labs(
      title = "Rozkład dominujących kolorów organizacji według roku studiów",
      y = "Proporcja",
      x = "Rok studiów",
      fill = "Kolor organizacji"
    )
}

# Uruchomienie analizy demograficznej
p_plec <- analiza_wedlug_plci(dane)
p_wiek <- analiza_wedlug_wieku(dane)
p_rok <- analiza_wedlug_roku(dane)

print(p_plec)
print(p_wiek)
print(p_rok)

# Poprawione wywołanie grid.arrange (dodano brakujący nawias)
grid.arrange(p_plec, p_wiek, p_rok, ncol = 1)

# PODSTAWOWE STATYSTYKI DEMOGRAFICZNE
print("=== PODSTAWOWE STATYSTYKI ===")
print("Rozkład płci:")
table(dane$płeć)
prop.table(table(dane$płeć)) * 100

print("Rozkład wieku:")
dane_wiek_clean <- dane %>% mutate(wiek = as.numeric(wiek)) %>% filter(!is.na(wiek))
print(paste("Średni wiek:", round(mean(dane_wiek_clean$wiek, na.rm = TRUE), 1)))
print(paste("Wiek min-max:", min(dane_wiek_clean$wiek, na.rm = TRUE), "-", max(dane_wiek_clean$wiek, na.rm = TRUE)))

# PROSTE TABELE KRZYŻOWE Z PROCENTAMI
print("\n=== KOLORY WEDŁUG PŁCI (w procentach) ===")
tabela_plec_kolor <- table(dane$płeć, dane$kolor_sredni)
print("Liczby:")
print(tabela_plec_kolor)
print("Procenty w rzędach (dla każdej płci osobno):")
print(round(prop.table(tabela_plec_kolor, margin = 1) * 100, 1))

print("\n=== KOLORY WEDŁUG WIEKU ===")
dane_z_wiekiem <- dane %>%
  mutate(wiek = as.numeric(wiek)) %>%
  filter(!is.na(wiek)) %>%
  mutate(kategoria_wieku = cut(wiek, breaks = c(18, 22, 25, 30, 100), 
                               labels = c("18-22", "23-25", "26-30", "31+")))

tabela_wiek_kolor <- table(dane_z_wiekiem$kategoria_wieku, dane_z_wiekiem$kolor_sredni)
print("Liczby:")
print(tabela_wiek_kolor)
print("Procenty w rzędach (dla każdej grupy wiekowej osobno):")
print(round(prop.table(tabela_wiek_kolor, margin = 1) * 100, 1))

print("\n=== KOLORY WEDŁUG ROKU STUDIÓW ===")
dane_z_rokiem <- dane %>%
  mutate(rok_studiów = case_when(
    str_detect(rok_studiów, "1\\. roku studiów licencjackich") ~ "1 lic.",
    str_detect(rok_studiów, "2\\. roku studiów licencjackich") ~ "2 lic.", 
    str_detect(rok_studiów, "3\\. roku studiów licencjackich") ~ "3 lic.",
    str_detect(rok_studiów, "1\\. roku studiów magisterskich") ~ "1 mgr",
    str_detect(rok_studiów, "2\\. roku studiów magisterskich") ~ "2 mgr",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(rok_studiów))

tabela_rok_kolor <- table(dane_z_rokiem$rok_studiów, dane_z_rokiem$kolor_sredni)
print("Liczby:")
print(tabela_rok_kolor)
print("Procenty w rzędach (dla każdego roku osobno):")
print(round(prop.table(tabela_rok_kolor, margin = 1) * 100, 1))

# SZYBKIE PODSUMOWANIE
print("\n=== SZYBKIE PODSUMOWANIE ===")
print(paste("Kobiet:", sum(dane$płeć == "Kobieta", na.rm = TRUE), 
            "(" , round(mean(dane$płeć == "Kobieta", na.rm = TRUE) * 100, 1), "%)"))
print(paste("Mężczyzn:", sum(dane$płeć == "Mężczyzna", na.rm = TRUE), 
            "(" , round(mean(dane$płeć == "Mężczyzna", na.rm = TRUE) * 100, 1), "%)"))

print(paste("Najczęstszy kolor ogółem:", names(sort(table(dane$kolor_sredni), decreasing = TRUE))[1]))
print(paste("Najczęstszy kolor u kobiet:", names(sort(table(dane$kolor_sredni[dane$płeć == "Kobieta"]), decreasing = TRUE))[1]))
print(paste("Najczęstszy kolor u mężczyzn:", names(sort(table(dane$kolor_sredni[dane$płeć == "Mężczyzna"]), decreasing = TRUE))[1]))
# PODSUMOWANIE OGÓLNE
dane_long <- dane %>%
  select(płeć, all_of(kolumny_kolorow)) %>%
  pivot_longer(
    cols = all_of(kolumny_kolorow),
    names_to = "Pytanie",
    values_to = "Kolor"
  )

dane_long$Pytanie <- factor(dane_long$Pytanie, 
                            levels = kolumny_kolorow,
                            labels = lepsze_nazwy)

udzialy <- dane_long %>%
  group_by(płeć, Pytanie, Kolor) %>%
  summarise(liczba = n(), .groups = "drop") %>%
  group_by(płeć, Pytanie) %>%
  mutate(procent = round(liczba / sum(liczba) * 100, 1)) %>%
  ungroup()

# WYDRUKOWANIE DANYCH
print(udzialy, n = Inf)



###########################################ANALIZA STABILNOŚCI STUDENTÓW filary +pyt7##############

# Punkt odniesienia: pyt7_firma (wybór firmy)
# Porównanie z filarami: filar_cechy, filar_hierarchia, filar_stres

# Kolumny do analizy stabilności
punkt_odniesienia <- "pyt7_firma"
filary_do_porownania <- c("filar_cechy", "filar_hierarchia", "filar_stres")

# Funkcja do obliczenia stabilności dla każdego studenta
oblicz_stabilnosc <- function(dane, punkt_odniesienia, filary) {
  stabilnosc <- numeric(nrow(dane))
  
  for (i in 1:nrow(dane)) {
    # Kolor odniesienia (z pytania o firmę)
    kolor_odniesienia <- dane[[punkt_odniesienia]][i]
    
    # Sprawdź czy kolor odniesienia nie jest NA
    if (is.na(kolor_odniesienia)) {
      stabilnosc[i] <- NA
      next
    }
    
    # Zlicz zgodności w filarach
    zgodnosci <- 0
    liczba_filarow <- 0
    
    for (filar in filary) {
      kolor_filaru <- dane[[filar]][i]
      
      # Sprawdź tylko filary, które nie są NA
      if (!is.na(kolor_filaru)) {
        liczba_filarow <- liczba_filarow + 1
        if (kolor_filaru == kolor_odniesienia) {
          zgodnosci <- zgodnosci + 1
        }
      }
    }
    
    # Oblicz stabilność (zgodności / liczba dostępnych filarów)
    if (liczba_filarow > 0) {
      stabilnosc[i] <- zgodnosci / liczba_filarow
    } else {
      stabilnosc[i] <- NA
    }
  }
  
  return(stabilnosc)
}

# Oblicz stabilność dla każdego studenta
dane$stabilnosc <- oblicz_stabilnosc(dane, punkt_odniesienia, filary_do_porownania)

# Podstawowe statystyki stabilności
cat("=== ANALIZA STABILNOŚCI STUDENTÓW ===\n")
cat("Punkt odniesienia:", punkt_odniesienia, "\n")
cat("Porównywane filary:", paste(filary_do_porownania, collapse = ", "), "\n\n")

# Statystyki opisowe
stabilnosc_stats <- summary(dane$stabilnosc)
print(stabilnosc_stats)

cat("\nŚrednia stabilność:", round(mean(dane$stabilnosc, na.rm = TRUE), 3), "\n")
cat("Mediana stabilności:", round(median(dane$stabilnosc, na.rm = TRUE), 3), "\n")
cat("Odchylenie standardowe:", round(sd(dane$stabilnosc, na.rm = TRUE), 3), "\n")

# Rozkład poziomów stabilności
cat("\n=== ROZKŁAD POZIOMÓW STABILNOŚCI ===\n")
stabilnosc_kategorie <- cut(dane$stabilnosc, 
                            breaks = c(-0.1, 0, 0.33, 0.66, 1.0),
                            labels = c("Brak (0)", "Niska (0-0.33)", "Średnia (0.34-0.66)", "Wysoka (0.67-1.0)"),
                            include.lowest = TRUE)

table_stabilnosc <- table(stabilnosc_kategorie, useNA = "ifany")
prop_stabilnosc <- prop.table(table_stabilnosc) * 100

print(table_stabilnosc)
cat("\nProcentowy rozkład:\n")
print(round(prop_stabilnosc, 1))

# Szczegółowa analiza zgodności
cat("\n=== SZCZEGÓŁOWA ANALIZA ZGODNOŚCI ===\n")

# Funkcja do analizy zgodności dla każdego filaru osobno
analiza_zgodnosci_filar <- function(dane, punkt_odniesienia, filar) {
  zgodnosci <- sum(dane[[punkt_odniesienia]] == dane[[filar]], na.rm = TRUE)
  dostepne <- sum(!is.na(dane[[punkt_odniesienia]]) & !is.na(dane[[filar]]))
  procent <- ifelse(dostepne > 0, (zgodnosci / dostepne) * 100, 0)
  
  return(data.frame(
    Filar = filar,
    Zgodnosci = zgodnosci,
    Dostepne_pary = dostepne,
    Procent_zgodnosci = round(procent, 1)
  ))
}

# Analiza dla każdego filaru
wyniki_filarow <- do.call(rbind, lapply(filary_do_porownania, 
                                        function(x) analiza_zgodnosci_filar(dane, punkt_odniesienia, x)))

print(wyniki_filarow)

# Analiza stabilności według koloru odniesienia
cat("\n=== STABILNOŚĆ WEDŁUG KOLORU FIRMY ===\n")
stabilnosc_wg_koloru <- dane %>%
  group_by(!!sym(punkt_odniesienia)) %>%
  summarise(
    Liczba_studentow = n(),
    Srednia_stabilnosc = round(mean(stabilnosc, na.rm = TRUE), 3),
    Mediana_stabilnosc = round(median(stabilnosc, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(Srednia_stabilnosc))

print(stabilnosc_wg_koloru)

# ANALIZA STABILNOŚCI WEDŁUG FILARÓW - PROFILE WIERSZOWE
cat("\n=== ANALIZA STABILNOŚCI WEDŁUG FILARÓW (PROFILE WIERSZOWE) ===\n")

# Funkcja do tworzenia szczegółowej tabeli zgodności dla każdego studenta
utworz_tabele_stabilnosci <- function(dane, punkt_odniesienia, filary) {
  # Utwórz ramkę danych z kolorami odniesienia i filarów
  tabela_det <- dane %>%
    select(all_of(c(punkt_odniesienia, filary)), stabilnosc) %>%
    mutate(ID_studenta = row_number()) %>%
    select(ID_studenta, everything())
  
  # Dodaj kolumny zgodności dla każdego filaru
  for (filar in filary) {
    nazwa_zgodnosci <- paste0("zgodnosc_", gsub("filar_", "", filar))
    tabela_det[[nazwa_zgodnosci]] <- ifelse(
      tabela_det[[punkt_odniesienia]] == tabela_det[[filar]], 
      "TAK", "NIE"
    )
    tabela_det[[nazwa_zgodnosci]][is.na(tabela_det[[punkt_odniesienia]]) | 
                                    is.na(tabela_det[[filar]])] <- "BRAK_DANYCH"
  }
  
  return(tabela_det)
}

# Utwórz szczegółową tabelę
tabela_stabilnosci <- utworz_tabele_stabilnosci(dane, punkt_odniesienia, filary_do_porownania)

# Pokaż przykład pierwszych 10 studentów
cat("Przykład szczegółowej analizy dla pierwszych 10 studentów:\n")
print(head(tabela_stabilnosci, 10))

# ANALIZA PROFILÓW WIERSZOWYCH - główne tabele
cat("\n=== PROFILE WIERSZOWE STABILNOŚCI ===\n")

# 1. Tabela krzyżowa: Kolor firmy vs Poziom stabilności
poziomy_stabilnosci <- cut(dane$stabilnosc, 
                           breaks = c(-0.1, 0, 0.33, 0.66, 1.0),
                           labels = c("Brak", "Niska", "Średnia", "Wysoka"))

tabela_krzyzowa <- table(dane[[punkt_odniesienia]], poziomy_stabilnosci, useNA = "ifany")
cat("Tabela krzyżowa: Kolor firmy × Poziom stabilności\n")
print(tabela_krzyzowa)

# Profile wierszowe (procenty w wierszach)
profile_wierszowe <- prop.table(tabela_krzyzowa, 1) * 100
cat("\nProfile wierszowe (% w wierszach):\n")
print(round(profile_wierszowe, 1))

# 2. Szczegółowa analiza zgodności dla każdego koloru firmy
cat("\n=== SZCZEGÓŁOWA ANALIZA DLA KAŻDEGO KOLORU FIRMY ===\n")

analiza_szczegolowa <- dane %>%
  filter(!is.na(!!sym(punkt_odniesienia))) %>%
  group_by(kolor_firmy = !!sym(punkt_odniesienia)) %>%
  summarise(
    liczba_studentow = n(),
    srednia_stabilnosc = round(mean(stabilnosc, na.rm = TRUE), 3),
    # Zgodności w poszczególnych filarach
    zgodnosc_cechy = round(mean(filar_cechy == kolor_firmy, na.rm = TRUE) * 100, 1),
    zgodnosc_hierarchia = round(mean(filar_hierarchia == kolor_firmy, na.rm = TRUE) * 100, 1),
    zgodnosc_stres = round(mean(filar_stres == kolor_firmy, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(srednia_stabilnosc))

print(analiza_szczegolowa)

# 3. Macierz zgodności między filarami
cat("\n=== MACIERZ ZGODNOŚCI MIĘDZY PUNKTEM ODNIESIENIA A FILARAMI ===\n")

for (filar in filary_do_porownania) {
  cat(paste("\nZgodność:", punkt_odniesienia, "vs", filar, "\n"))
  tabela_zgodnosci <- table(dane[[punkt_odniesienia]], dane[[filar]], useNA = "ifany")
  print(tabela_zgodnosci)
  
  # Profile wierszowe dla tej tabeli
  profile <- prop.table(tabela_zgodnosci, 1) * 100
  cat("Profile wierszowe (%):\n")
  print(round(profile, 1))
  cat("\n", paste(rep("-", 50), collapse = ""), "\n")
}

# Wykres rozkładu stabilności (poprawiony)
library(ggplot2)

wykres_stabilnosc <- ggplot(dane, aes(x = stabilnosc)) +
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  geom_vline(xintercept = mean(dane$stabilnosc, na.rm = TRUE), 
             color = "red", linetype = "dashed") +
  labs(title = "Rozkład stabilności studentów",
       x = "Poziom stabilności (0-1)",
       y = "Liczba studentów") +
  theme_minimal()

print(wykres_stabilnosc)


# Dodatkowa tabela: macierz zgodności dla każdego studenta
cat("\n=== PRZYKŁADY INDYWIDUALNYCH ANALIZ ===\n")
przyklad_analizy <- dane %>%
  select(all_of(c(punkt_odniesienia, filary_do_porownania)), stabilnosc) %>%
  head(10)

print(przyklad_analizy)

# ANALIZA STABILNOŚCI WEDŁUG KOLORÓW
# Sprawdzamy, który kolor ma najlepszą stabilność

library(ggplot2)
library(dplyr)

# 1. TABELA: Średnia stabilność według koloru firmy
stabilnosc_wg_koloru <- dane %>%
  filter(!is.na(pyt7_firma) & !is.na(stabilnosc)) %>%
  group_by(Kolor_firmy = pyt7_firma) %>%
  summarise(
    Liczba_studentow = n(),
    Srednia_stabilnosc = round(mean(stabilnosc), 3),
    Mediana_stabilnosc = round(median(stabilnosc), 3),
    Min_stabilnosc = round(min(stabilnosc), 3),
    Max_stabilnosc = round(max(stabilnosc), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(Srednia_stabilnosc))

cat("=== STABILNOŚĆ WEDŁUG KOLORU FIRMY ===\n")
print(stabilnosc_wg_koloru)

# 2. WYKRES SŁUPKOWY: Średnia stabilność według koloru
wykres_stabilnosc_kolory <- ggplot(stabilnosc_wg_koloru, 
                                   aes(x = reorder(Kolor_firmy, -Srednia_stabilnosc), 
                                       y = Srednia_stabilnosc, 
                                       fill = Kolor_firmy)) +
  geom_col(color = "black", alpha = 0.8) +
  geom_text(aes(label = paste0(Srednia_stabilnosc, "\n(n=", Liczba_studentow, ")")), 
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(
    "czerwona" = "red",
    "pomarańczowa" = "orange", 
    "zielona" = "green",
    "turkusowa" = "turquoise",
    "bursztynowa" = "gold"
  )) +
  labs(title = "Średnia stabilność według koloru wybranej firmy",
       subtitle = "Liczba w nawiasach = liczba studentów",
       x = "Kolor firmy",
       y = "Średnia stabilność (0-1)",
       fill = "Kolor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)

print(wykres_stabilnosc_kolory)

# 3. SZCZEGÓŁOWA ANALIZA: Zgodność w każdym filarze według koloru
cat("\n=== ZGODNOŚĆ W POSZCZEGÓLNYCH FILARACH WEDŁUG KOLORU ===\n")

zgodnosc_szczegolowa <- dane %>%
  filter(!is.na(pyt7_firma)) %>%
  group_by(Kolor_firmy = pyt7_firma) %>%
  summarise(
    Liczba_studentow = n(),
    # Procent zgodności w każdym filarze
    Zgodnosc_cechy = round(mean(filar_cechy == pyt7_firma, na.rm = TRUE) * 100, 1),
    Zgodnosc_hierarchia = round(mean(filar_hierarchia == pyt7_firma, na.rm = TRUE) * 100, 1),
    Zgodnosc_stres = round(mean(filar_stres == pyt7_firma, na.rm = TRUE) * 100, 1),
    # Średnia stabilność
    Srednia_stabilnosc = round(mean(stabilnosc, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(Srednia_stabilnosc))

print(zgodnosc_szczegolowa)

# 4. WYKRES: Zgodność w filarach według kolorów (wykres radarowy/heatmap)
library(tidyr)

# Przekształć dane do formatu długiego
dane_dlugie <- zgodnosc_szczegolowa %>%
  select(Kolor_firmy, Zgodnosc_cechy, Zgodnosc_hierarchia, Zgodnosc_stres) %>%
  pivot_longer(cols = starts_with("Zgodnosc_"), 
               names_to = "Filar", 
               values_to = "Procent_zgodnosci") %>%
  mutate(Filar = case_when(
    Filar == "Zgodnosc_cechy" ~ "Cechy",
    Filar == "Zgodnosc_hierarchia" ~ "Hierarchia", 
    Filar == "Zgodnosc_stres" ~ "Stres"
  ))

# Wykres heatmap zgodności
wykres_heatmap <- ggplot(dane_dlugie, aes(x = Filar, y = Kolor_firmy, fill = Procent_zgodnosci)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = paste0(Procent_zgodnosci, "%")), color = "black", size = 4) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                       midpoint = 50, name = "% zgodności") +
  labs(title = "Mapa zgodności: Kolory firm vs Filary",
       subtitle = "Im bardziej zielone, tym wyższa zgodność",
       x = "Filar", 
       y = "Kolor firmy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_heatmap)

# 5. WYKRES PUNKTOWY: Stabilność vs Liczba studentów
wykres_stabilnosc_vs_liczba <- ggplot(stabilnosc_wg_koloru, 
                                      aes(x = Liczba_studentow, y = Srednia_stabilnosc, 
                                          color = Kolor_firmy, size = Liczba_studentow)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = Kolor_firmy), vjust = -1, size = 3) +
  scale_color_manual(values = c(
    "czerwona" = "red",
    "pomarańczowa" = "orange", 
    "zielona" = "green",
    "turkusowa" = "turquoise",
    "bursztynowa" = "gold"
  )) +
  labs(title = "Stabilność vs Popularność kolorów",
       x = "Liczba studentów wybierających kolor",
       y = "Średnia stabilność",
       color = "Kolor firmy",
       size = "Liczba studentów") +
  theme_minimal()

print(wykres_stabilnosc_vs_liczba)

# 6. RANKING KOLORÓW
cat("\n=== RANKING KOLORÓW POD WZGLĘDEM STABILNOŚCI ===\n")
ranking <- stabilnosc_wg_koloru %>%
  mutate(Pozycja = row_number()) %>%
  select(Pozycja, Kolor_firmy, Srednia_stabilnosc, Liczba_studentow)

print(ranking)


#############ANALIZA PAM######################
# Pakiety
library(cluster)
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)

# 1. Przygotowanie danych
dane_do_klas <- dane %>%
  select(pyt7_firma, filar_cechy, filar_hierarchia, filar_stres) %>%
  filter(complete.cases(.)) %>%
  mutate(across(c(pyt7_firma, filar_cechy, filar_hierarchia, filar_stres), as.factor))

cat("Liczba obserwacji:", nrow(dane_do_klas), "\n")


# 2. Obliczanie macierzy odległości Gowera
odleglosci_gower <- daisy(dane_do_klas, metric = "gower")

# WIZUALIZACJA MACIERZY ODLEGŁOŚCI GOWERA
# Konwersja do macierzy
macierz_gower <- as.matrix(odleglosci_gower)

##metoda łokcia####
# Funkcja do obliczania WSS dla PAM z gotową macierzą Gowera
calculate_wss_pam_gower <- function(distance_matrix, k) {
  if (k == 1) {
    # Dla k=1: średnia wszystkich odległości do centrum
    macierz <- as.matrix(distance_matrix)
    return(sum(macierz) / (2 * nrow(macierz)))
  } else {
    pam_result <- pam(distance_matrix, k = k)
    # WSS to suma objective function PAM
    return(sum(pam_result$objective))
  }
}

# Główna funkcja metody łokcia
metoda_lokcia <- function(distance_matrix, max_k = 8) {
  wss_values <- numeric(max_k)
  
  cat("Obliczanie WSS dla k = 1 do", max_k, ":\n")
  
  for (k in 1:max_k) {
    cat("k =", k, "... ")
    wss_values[k] <- calculate_wss_pam_gower(distance_matrix, k)
    cat("WSS =", round(wss_values[k], 4), "\n")
  }
  
  # Oblicz procentową redukcję WSS
  reduction <- c(NA, -diff(wss_values) / wss_values[-length(wss_values)] * 100)
  
  results <- data.frame(
    k = 1:max_k,
    wss = wss_values,
    reduction_percent = reduction
  )
  
  return(results)
}

# Uruchom analizę łokcia
elbow_results <- metoda_lokcia(odleglosci_gower, max_k = 8)

# Wyświetl tabelę wyników
cat("\nWyniki metody łokcia:\n")
print(elbow_results)

# Stwórz wykresy
p1 <- ggplot(elbow_results, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = round(wss, 3)), vjust = -0.5, size = 3) +
  labs(
    title = "Metoda łokcia - WSS vs liczba klastrów",
    subtitle = "PAM clustering z odległością Gowera",
    x = "Liczba klastrów (k)",
    y = "Within Sum of Squares (WSS)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  ) +
  scale_x_continuous(breaks = elbow_results$k)

p2 <- ggplot(elbow_results[-1,], aes(x = k, y = reduction_percent)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkred", size = 3) +
  geom_text(aes(label = paste0(round(reduction_percent, 1), "%")), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Procentowa redukcja WSS",
    x = "Liczba klastrów (k)",
    y = "Redukcja WSS (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_x_continuous(breaks = elbow_results$k[-1])

# Wyświetl wykresy
print(p1)
print(p2)
######Analiza porównawcza##############33
#funkcja weryfikuj_klasy_gower (bez zmian)
weryfikuj_klasy_gower <- function(k) {
  pam_temp <- pam(odleglosci_gower, k = k)
  klasy <- pam_temp$clustering
  macierz <- as.matrix(odleglosci_gower)
  
  odl_wewnatrz <- c()
  odl_miedzy <- c()
  
  for(i in 1:nrow(macierz)) {
    for(j in (i+1):ncol(macierz)) {
      if(j <= nrow(macierz)) {
        if(klasy[i] == klasy[j]) {
          odl_wewnatrz <- c(odl_wewnatrz, macierz[i,j])
        } else {
          odl_miedzy <- c(odl_miedzy, macierz[i,j])
        }
      }
    }
  }
  
  cat("WERYFIKACJA", k, "KLAS:\n")
  cat("Odległości WEWNĄTRZ klas:\n")
  cat("  Średnia:", round(mean(odl_wewnatrz), 3), "\n")
  cat("  Mediana:", round(median(odl_wewnatrz), 3), "\n")
  cat("  Min:", round(min(odl_wewnatrz), 3), "\n")
  cat("  Max:", round(max(odl_wewnatrz), 3), "\n\n")
  
  cat("Odległości MIĘDZY klasami:\n") 
  cat("  Średnia:", round(mean(odl_miedzy), 3), "\n")
  cat("  Mediana:", round(median(odl_miedzy), 3), "\n")
  cat("  Min:", round(min(odl_miedzy), 3), "\n")
  cat("  Max:", round(max(odl_miedzy), 3), "\n\n")
  
  ratio <- mean(odl_miedzy) / mean(odl_wewnatrz)
  cat("WSKAŹNIK SEPARACJI:", round(ratio, 3), "\n")
  cat("(Im wyższy, tym lepsze klasy - powinien być > 1.2)\n\n")
  
  medoidy <- pam_temp$medoids
  cat("Odległości między medoidami (centrami klas):\n")
  for(i in 1:(length(medoidy)-1)) {
    for(j in (i+1):length(medoidy)) {
      odl_medoidy <- macierz[medoidy[i], medoidy[j]]
      cat("  Klasa", i, "vs Klasa", j, ":", round(odl_medoidy, 3), "\n")
    }
  }
  
  cat("\nŚrednie odległości od medoidów w klasach:\n")
  for(i in 1:k) {
    obs_w_klasie <- which(klasy == i)
    medoid <- medoidy[i]
    odl_od_medoidu <- macierz[obs_w_klasie, medoid]
    cat("  Klasa", i, ":", round(mean(odl_od_medoidu), 3), 
        "(rozrzut:", round(sd(odl_od_medoidu), 3), ")\n")
  }
  
  return(list(
    wewnatrz = odl_wewnatrz,
    miedzy = odl_miedzy, 
    ratio = ratio,
    wss = elbow_results$wss[k] # Dodaj WSS z analizy łokcia
  ))
}

# Uruchom analizy dla różnych k
wyniki_2 <- weryfikuj_klasy_gower(2)
wyniki_3 <- weryfikuj_klasy_gower(3) 
wyniki_4 <- weryfikuj_klasy_gower(4)
wyniki_5 <- weryfikuj_klasy_gower(5)
wyniki_6 <-weryfikuj_klasy_gower(6)

#####Podsumowanie######
summary_df <- data.frame(
  k = 2:6,
  WSS = c(wyniki_2$wss, wyniki_3$wss, wyniki_4$wss, wyniki_5$wss, wyniki_6$wss),
  WSS_redukcja = elbow_results$reduction_percent[2:6],
  Separacja = c(wyniki_2$ratio, wyniki_3$ratio, wyniki_4$ratio, wyniki_5$ratio, wyniki_6$wss),
  Separacja_OK = c(wyniki_2$ratio, wyniki_3$ratio, wyniki_4$ratio, wyniki_5$ratio, wyniki_6$wss) > 1.2
)

print(summary_df)

####Rekomendacja####
# Najlepsza separacja
best_separation <- which.max(summary_df$Separacja)
cat("Najlepsza separacja klas:", summary_df$k[best_separation], "klas (wskaźnik:", 
    round(summary_df$Separacja[best_separation], 3), ")\n")

# Największa redukcja WSS
best_wss_reduction <- which.max(summary_df$WSS_redukcja[-1]) + 1 # pomijamy NA
cat("Największa redukcja WSS:", summary_df$k[best_wss_reduction], "klas (redukcja:", 
    round(summary_df$WSS_redukcja[best_wss_reduction], 1), "%)\n")

# Kombinowana rekomendacja
cat("\nKOMBINOWANA REKOMENDACJA:\n")
if(summary_df$k[best_separation] == summary_df$k[best_wss_reduction]) {
  cat("🎯 JEDNOZNACZNA REKOMENDACJA:", summary_df$k[best_separation], "klas\n")
  cat("   (zgodność metody łokcia i wskaźnika separacji)\n")
} else {
  cat("📊 ANALIZA WYKAZUJE:\n")
  cat("   - Metoda łokcia wskazuje:", summary_df$k[best_wss_reduction], "klas\n")
  cat("   - Wskaźnik separacji wskazuje:", summary_df$k[best_separation], "klas\n")
  cat("   - Rekomendacja: rozważ oba warianty w kontekście interpretacji biznesowej\n")
}

# Wykres porównawczy
p3 <- ggplot(summary_df, aes(x = factor(k))) +
  geom_col(aes(y = WSS, fill = "WSS"), alpha = 0.7, position = "dodge") +
  geom_line(aes(y = Separacja * max(WSS), group = 1, color = "Separacja"), size = 1.2) +
  geom_point(aes(y = Separacja * max(WSS), color = "Separacja"), size = 3) +
  scale_y_continuous(
    name = "WSS",
    sec.axis = sec_axis(~ . / max(summary_df$WSS), name = "Wskaźnik Separacji")
  ) +
  scale_fill_manual(values = c("WSS" = "lightblue")) +
  scale_color_manual(values = c("Separacja" = "red")) +
  labs(title = "Porównanie WSS i Wskaźnika Separacji", x = "Liczba klas") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)


#####Wybieranie liczby klas#####
WYBRANA_LICZBA <- 3

pam_final <- pam(odleglosci_gower, k = WYBRANA_LICZBA)
dane_do_klas$klasa <- pam_final$clustering

summary_klas <- dane_do_klas %>%
  group_by(klasa) %>%
  summarise(
    liczba_osob = n(),
    procent = round(n()/nrow(dane_do_klas)*100, 1),
    najcz_firma = names(sort(table(pyt7_firma), TRUE))[1],
    najcz_cechy = names(sort(table(filar_cechy), TRUE))[1],
    najcz_hierarchia = names(sort(table(filar_hierarchia), TRUE))[1],
    najcz_stres = names(sort(table(filar_stres), TRUE))[1],
    .groups = "drop"
  )

cat("FINALNE KLASY (", WYBRANA_LICZBA, "):\n")
print(summary_klas)

# 3. Wybór liczby klas – ustaw ręcznie, np. 3
liczba_klas <- 3

# 4. Grupowanie metodą PAM
pam_model <- pam(odleglosci_gower, k = liczba_klas)
dane_do_klas$klasa <- pam_model$clustering

# 5. Podsumowanie klas
summary_klas <- dane_do_klas %>%
  group_by(klasa) %>%
  summarise(
    liczba_osob = n(),
    najcz_firma = names(sort(table(pyt7_firma), TRUE))[1],
    najcz_cechy = names(sort(table(filar_cechy), TRUE))[1],
    najcz_hierarchia = names(sort(table(filar_hierarchia), TRUE))[1],
    najcz_stres = names(sort(table(filar_stres), TRUE))[1],
    .groups = "drop"
  )

# 6. Tabela kontyngencji: Klasa vs Kolor firmy
tabela <- table(dane_do_klas$klasa, dane_do_klas$pyt7_firma)
cat("Tabela klas vs kolor firmy:\n")
print(tabela)
cat("Procentowy udział kolorów w klasach:\n")
print(round(prop.table(tabela, 1) * 100, 1))

# 7. Wykres pudełkowy stabilności w klasach
ggplot(dane_do_klas, aes(x = factor(klasa), y = dane$stabilnosc, fill = factor(klasa))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Rozkład stabilności w klasach", x = "Klasa", y = "Stabilność") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Klasa"))

# 8. Dodatkowa wizualizacja: Macierz odległości z podziałem na klasy
# Sortowanie macierzy według przynależności do klas
indeksy_sortowane <- order(dane_do_klas$klasa)
macierz_sortowana <- macierz_gower[indeksy_sortowane, indeksy_sortowane]

# Jeśli za dużo obserwacji, pokazujemy tylko część
if(nrow(macierz_sortowana) > 100) {
  macierz_sortowana <- macierz_sortowana[1:100, 1:100]
}

macierz_sort_long <- melt(macierz_sortowana)
names(macierz_sort_long) <- c("Obs1", "Obs2", "Odleglosc")

ggplot(macierz_sort_long, aes(x = Obs1, y = Obs2, fill = Odleglosc)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0.5, name = "Odległość\nGowera") +
  labs(title = "Macierz odległości Gowera (posortowana według klas)",
       x = "Obserwacja", y = "Obserwacja") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Informacje o medoidach (centrach klas)
cat("\nMedoidy (centra klas):\n")
for(i in 1:liczba_klas) {
  medoid_idx <- pam_model$medoids[i]
  cat("Klasa", i, "- medoid (obserwacja nr", medoid_idx, "):\n")
  print(dane_do_klas[medoid_idx, ])
  cat("\n")
}




# FUNKCJA WERYFIKACJI KLAS MACIERZĄ GOWERA
weryfikuj_klasy_gower <- function(k) {
  # PAM dla k klas
  pam_temp <- pam(odleglosci_gower, k = k)
  klasy <- pam_temp$clustering
  
  # Macierz odległości
  macierz <- as.matrix(odleglosci_gower)
  
  # 1. ODLEGŁOŚCI WEWNĄTRZ vs MIĘDZY KLASAMI
  odl_wewnatrz <- c()
  odl_miedzy <- c()
  
  for(i in 1:nrow(macierz)) {
    for(j in (i+1):ncol(macierz)) {
      if(j <= nrow(macierz)) {
        if(klasy[i] == klasy[j]) {
          # Ta sama klasa
          odl_wewnatrz <- c(odl_wewnatrz, macierz[i,j])
        } else {
          # Różne klasy  
          odl_miedzy <- c(odl_miedzy, macierz[i,j])
        }
      }
    }
  }
  
  # 2. STATYSTYKI
  cat("WERYFIKACJA", k, "KLAS:\n")
  
  cat("Odległości WEWNĄTRZ klas:\n")
  cat("  Średnia:", round(mean(odl_wewnatrz), 3), "\n")
  cat("  Mediana:", round(median(odl_wewnatrz), 3), "\n")
  cat("  Min:", round(min(odl_wewnatrz), 3), "\n")
  cat("  Max:", round(max(odl_wewnatrz), 3), "\n\n")
  
  cat("Odległości MIĘDZY klasami:\n") 
  cat("  Średnia:", round(mean(odl_miedzy), 3), "\n")
  cat("  Mediana:", round(median(odl_miedzy), 3), "\n")
  cat("  Min:", round(min(odl_miedzy), 3), "\n")
  cat("  Max:", round(max(odl_miedzy), 3), "\n\n")
  
  # 3. WSKAŹNIK SEPARACJI
  ratio <- mean(odl_miedzy) / mean(odl_wewnatrz)
  cat("WSKAŹNIK SEPARACJI:", round(ratio, 3), "\n")
  cat("(Im wyższy, tym lepsze klasy - powinien być > 1.2)\n\n")
  
  # 4. ODLEGŁOŚCI MIĘDZY MEDOIDAMI
  medoidy <- pam_temp$medoids
  cat("Odległości między medoidami (centrami klas):\n")
  for(i in 1:(length(medoidy)-1)) {
    for(j in (i+1):length(medoidy)) {
      odl_medoidy <- macierz[medoidy[i], medoidy[j]]
      cat("  Klasa", i, "vs Klasa", j, ":", round(odl_medoidy, 3), "\n")
    }
  }
  
  # 5. ŚREDNIE ODLEGŁOŚCI OD MEDOIDÓW W KAŻDEJ KLASIE
  cat("\nŚrednie odległości od medoidów w klasach:\n")
  for(i in 1:k) {
    obs_w_klasie <- which(klasy == i)
    medoid <- medoidy[i]
    odl_od_medoidu <- macierz[obs_w_klasie, medoid]
    cat("  Klasa", i, ":", round(mean(odl_od_medoidu), 3), 
        "(rozrzut:", round(sd(odl_od_medoidu), 3), ")\n")
  }
  
  return(list(
    wewnatrz = odl_wewnatrz,
    miedzy = odl_miedzy, 
    ratio = ratio
  ))
}

# PORÓWNANIE RÓŻNYCH LICZB KLAS
wyniki_2 <- weryfikuj_klasy_gower(2)
wyniki_3 <- weryfikuj_klasy_gower(3) 
wyniki_4 <- weryfikuj_klasy_gower(4)
wyniki_5 <- weryfikuj_klasy_gower(5)

# PODSUMOWANIE PORÓWNAWCZE
cat("PODSUMOWANIE WSKAŹNIKÓW SEPARACJI:\n")
cat("2 klasy: ", round(wyniki_2$ratio, 3), "\n")
cat("3 klasy: ", round(wyniki_3$ratio, 3), "\n") 
cat("4 klasy: ", round(wyniki_4$ratio, 3), "\n")
cat("5 klas:  ", round(wyniki_5$ratio, 3), "\n")
cat("\nNajlepszy wskaźnik separacji ma:", 
    c(2,3,4,5)[which.max(c(wyniki_2$ratio, wyniki_3$ratio, wyniki_4$ratio, wyniki_5$ratio))], "klas\n")

# WYKRES PORÓWNAWCZY
library(ggplot2)
porownanie <- data.frame(
  klasy = rep(c("2", "3", "4", "5"), each = 2),
  typ = rep(c("Wewnątrz", "Między"), 4),
  srednia = c(
    mean(wyniki_2$wewnatrz), mean(wyniki_2$miedzy),
    mean(wyniki_3$wewnatrz), mean(wyniki_3$miedzy), 
    mean(wyniki_4$wewnatrz), mean(wyniki_4$miedzy),
    mean(wyniki_5$wewnatrz), mean(wyniki_5$miedzy)
  )
)

ggplot(porownanie, aes(x = klasy, y = srednia, fill = typ)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Średnie odległości wewnątrz vs między klasami",
       x = "Liczba klas", y = "Średnia odległość Gowera") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "orange"))

#####Macierz gowera podsumowanie#####

cat("CHARAKTERYSTYKA MACIERZY ODLEGŁOŚCI GOWERA:\n")
cat("Wymiary macierzy:", nrow(odleglosci_gower), "x", ncol(odleglosci_gower), "\n")
cat("Typ macierzy:", class(odleglosci_gower), "\n")
cat("Min odległość:", round(min(odleglosci_gower), 3), "\n")
cat("Max odległość:", round(max(odleglosci_gower), 3), "\n")
cat("Średnia odległość:", round(mean(odleglosci_gower), 3), "\n")
cat("Mediana odległość:", round(median(odleglosci_gower), 3), "\n")
cat("Odchylenie standardowe:", round(sd(odleglosci_gower), 3), "\n")

odleglosci_wektor <- as.vector(odleglosci_gower)
odleglosci_wektor <- odleglosci_wektor[odleglosci_wektor != 0] # usuwamy przekątną

hist_data <- data.frame(odleglosc = odleglosci_wektor)

p_hist <- ggplot(hist_data, aes(x = odleglosc)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = mean(odleglosci_wektor), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median(odleglosci_wektor), color = "orange", linetype = "dashed", size = 1) +
  labs(title = "Rozkład odległości w macierzy Gowera",
       subtitle = paste("Czerwona linia: średnia =", round(mean(odleglosci_wektor), 3),
                        "| Pomarańczowa: mediana =", round(median(odleglosci_wektor), 3)),
       x = "Odległość Gowera", y = "Częstość") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(p_hist)

# 3. MAPA CIEPŁA MACIERZY GOWERA (PRÓBKA)
# ========================================

# Dla czytelności - weź tylko pierwszych 50 obserwacji
n_sample <- min(50, nrow(odleglosci_gower))
sample_matrix <- as.matrix(odleglosci_gower)[1:n_sample, 1:n_sample]

# Sprawdź zakres wartości
cat("Zakres odległości Gowera:", range(sample_matrix), "\n")
cat("Średnia odległość:", mean(sample_matrix), "\n")

# Opcja 1: Podstawowa mapa ciepła z legendą
library(pheatmap)
pheatmap(sample_matrix,
         main = paste("Macierz odległości Gowera (próbka", n_sample, "obserwacji)"),
         color = colorRampPalette(c("darkblue", "blue", "white", "red", "darkred"))(100),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = FALSE,
         show_colnames = FALSE,
         fontsize = 10,
         legend = TRUE,
         legend_breaks = seq(0, 1, 0.2),
         legend_labels = c("0.0\n(identyczne)", "0.2", "0.4", "0.6", "0.8", "1.0\n(maksymalnie różne)"))
# 4. KWANTYLE ROZKŁADU ODLEGŁOŚCI
# ===============================
kwantyle <- quantile(odleglosci_wektor, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
cat("\nKWANTYLE ROZKŁADU ODLEGŁOŚCI:\n")
print(round(kwantyle, 3))

# 5. TABELA PODSUMOWUJĄCA WŁAŚCIWOŚCI MACIERZY
# =============================================
tabela_wlasciwosci <- data.frame(
  Właściwość = c("Liczba obserwacji", "Liczba par", "Min odległość", "Max odległość", 
                 "Średnia", "Mediana", "Odchylenie std.", "Q1", "Q3", "Zakres"),
  Wartość = c(nrow(odleglosci_gower),
              nrow(odleglosci_gower) * (nrow(odleglosci_gower) - 1) / 2,
              round(min(odleglosci_gower), 3),
              round(max(odleglosci_gower), 3),
              round(mean(odleglosci_wektor), 3),
              round(median(odleglosci_wektor), 3),
              round(sd(odleglosci_wektor), 3),
              round(kwantyle[3], 3),
              round(kwantyle[5], 3),
              round(max(odleglosci_gower) - min(odleglosci_gower), 3))
)

cat("\nTABELA WŁAŚCIWOŚCI MACIERZY GOWERA:\n")
print(tabela_wlasciwosci)

###ANALIZA DOMINUJĄCYCH KOLORÓW W KLASTRACH Z PRAWDZIWYMI KOLORAMI ########
# Funkcja do znajdowania dominującego koloru w każdym klastrze
find_dominant_colors <- function() {
  dominant_colors <- dane_do_klas %>%
    group_by(klasa) %>%
    summarise(
      dom_firma = names(sort(table(pyt7_firma), decreasing = TRUE))[1],
      dom_cechy = names(sort(table(filar_cechy), decreasing = TRUE))[1],
      dom_hierarchia = names(sort(table(filar_hierarchia), decreasing = TRUE))[1],
      dom_stres = names(sort(table(filar_stres), decreasing = TRUE))[1],
      .groups = 'drop'
    )
  
  # Przekształć do długiego formatu
  dominant_long <- dominant_colors %>%
    pivot_longer(cols = starts_with("dom_"), 
                 names_to = "filar", 
                 values_to = "kolor") %>%
    mutate(filar = gsub("dom_", "", filar))
  
  return(dominant_long)
}

dominant_colors_data <- find_dominant_colors()
# Utwórz mapę kolorów dla dominujących kolorów
all_dominant_colors <- unique(dominant_colors_data$kolor)
dominant_color_scale <- get_color_scale(all_dominant_colors)

wykres_dominujace <- ggplot(dominant_colors_data, aes(x = factor(klasa), y = filar, fill = kolor)) +
  geom_tile(color = "black", size = 1) +
  geom_text(aes(label = kolor), size = 3, color = "black", fontface = "bold") +
  scale_fill_manual(values = dominant_color_scale, name = "Dominujący\nkolor") +
  labs(title = "Dominujące kolory w każdej klasie według filarów i pytania 7",
       x = "Klasa",
       y = "Filar") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

print(wykres_dominujace)

# Tabela kontyngencji jako mapa ciepła - Firma
heatmap_firma <- dane_do_klas %>%
  count(klasa, pyt7_firma) %>%
  ggplot(aes(x = pyt7_firma, y = factor(klasa), fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Liczba\nosób") +
  labs(title = "Mapa ciepła: Klastry vs Kolor firmy",
       x = "Kolor firmy",
       y = "Kalsa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap_firma)
for (k in unique(dane_do_klas$klasa)) {
  cat("\n--- KLASTER", k, "---\n")
  dane_klasa <- dane_do_klas[dane_do_klas$klasa == k, ]  # Zmiana nazwy zmiennej
  
  cat("Firma (punkt odniesienia):\n")
  print(table(dane_klasa$pyt7_firma))
  
  cat("Filar cechy:\n")
  print(table(dane_klasa$filar_cechy))
  
  cat("Filar hierarchia:\n") 
  print(table(dane_klasa$filar_hierarchia))
  
  cat("Filar stres:\n")
  print(table(dane_klasa$filar_stres))
  
  cat("Stabilność - statystyki:\n")
  print(summary(dane_klasa$stabilnosc))
}

#########Analiza stabilności weryfikacja pearson####################
################## PORÓWNANIE METOD ANALIZY ###################
# Gamma vs Chi-kwadrat vs Pearson dla danych porządkowych
# =========================================================

print("PORÓWNANIE METOD ANALIZY STATYSTYCZNEJ")
print("======================================")
print("Analiza dla skali porządkowej kolorów (1-5)")
print("")

# Przygotowanie danych
library(vcd)        # dla gamma
library(Hmisc)      # dla rcorr
library(corrplot)   # dla wizualizacji
library(ggplot2)    # dla wykresów

# Funkcje pomocnicze
interpretuj_gamma <- function(gamma) {
  abs_gamma <- abs(gamma)
  if (abs_gamma < 0.1) return("bardzo słaby")
  if (abs_gamma < 0.3) return("słaby")
  if (abs_gamma < 0.5) return("umiarkowany")
  if (abs_gamma < 0.7) return("silny")
  return("bardzo silny")
}

interpretuj_pearsona <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("bardzo słaby")
  if (abs_r < 0.3) return("słaby")
  if (abs_r < 0.5) return("umiarkowany")
  if (abs_r < 0.7) return("silny")
  return("bardzo silny")
}

# Sprawdzenie założeń dla każdej metody
print("1. SPRAWDZENIE ZAŁOŻEŃ METOD")
print("============================")

# Test normalności dla każdej zmiennej (dla Pearsona)
shapiro_wyniki <- list()
for(col in names(dane_korelacji_clean)) {
  if(length(unique(dane_korelacji_clean[[col]])) > 2) {
    test <- shapiro.test(dane_korelacji_clean[[col]])
    shapiro_wyniki[[col]] <- test
    cat(col, "- Shapiro-Wilk p-value:", round(test$p.value, 4), 
        ifelse(test$p.value > 0.05, "(normalny)", "(NIE normalny)"), "\n")
  }
}
print("")

# Sprawdzenie linearności wizualnie
print("2. SPRAWDZENIE LINEARNOŚCI (WIZUALNE)")
print("====================================")

# Funkcja do tworzenia wykresów rozrzutu
utworz_wykres_rozrzutu <- function(x, y, x_name, y_name) {
  df_temp <- data.frame(x = x, y = y)
  
  ggplot(df_temp, aes(x = x, y = y)) +
    geom_point(alpha = 0.6, position = position_jitter(width = 0.1, height = 0.1)) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = paste("Związek:", x_name, "vs", y_name),
         subtitle = "Czerwona linia = liniowa, niebieska = nieliniowa",
         x = x_name, y = y_name) +
    theme_minimal() +
    scale_x_continuous(breaks = 1:5, labels = c("Czerwona", "Bursztyn.", "Pomaran.", "Zielona", "Turkus.")) +
    scale_y_continuous(breaks = 1:5, labels = c("Czerwona", "Bursztyn.", "Pomaran.", "Zielona", "Turkus."))
}

print("")
print("3. PORÓWNANIE TRZECH METOD")
print("==========================")

# Przygotowanie ramki wyników
wyniki_porownanie <- data.frame(
  Para_zmiennych = character(),
  Gamma = numeric(),
  Gamma_p = numeric(),
  Chi2_stat = numeric(),
  Chi2_p = numeric(),
  Pearson_r = numeric(),
  Pearson_p = numeric(),
  Spearman_rho = numeric(),
  Spearman_p = numeric(),
  stringsAsFactors = FALSE
)

# Pobierz nazwy zmiennych
nazwy_zmiennych <- names(dane_korelacji_clean)

# Analiza dla każdej pary zmiennych
for(i in 1:(length(nazwy_zmiennych)-1)) {
  for(j in (i+1):length(nazwy_zmiennych)) {
    
    x <- dane_korelacji_clean[[i]]
    y <- dane_korelacji_clean[[j]]
    nazwa_pary <- paste(nazwy_zmiennych[i], "↔", nazwy_zmiennych[j])
    
    # 1. GAMMA (dla danych porządkowych)
    # Poprawione wywołanie funkcji gamma
    gamma_wynik <- tryCatch({
      gamma_val <- GoodmanKruskalGamma(x, y)
      # Sprawdź strukturę zwróconego obiektu
      if(is.list(gamma_val)) {
        list(gamma = gamma_val$gamma, p.value = gamma_val$p.value)
      } else {
        # Jeśli zwraca tylko wartość, oblicz p-value osobno
        list(gamma = gamma_val, p.value = NA)
      }
    }, error = function(e) {
      # Alternatywna metoda obliczania gamma
      tab <- table(x, y)
      gamma_val <- assocstats(tab)$gamma
      list(gamma = gamma_val, p.value = NA)
    })
    
    # 2. CHI-KWADRAT (test niezależności)
    tabela_kontyngencji <- table(x, y)
    chi2_wynik <- tryCatch({
      chisq.test(tabela_kontyngencji)
    }, warning = function(w) {
      # Ignoruj ostrzeżenia o aproksymacji
      chisq.test(tabela_kontyngencji)
    })
    
    # 3. PEARSON (korelacja liniowa)
    pearson_wynik <- cor.test(x, y, method = "pearson")
    
    # 4. SPEARMAN (korelacja rangowa)
    spearman_wynik <- tryCatch({
      cor.test(x, y, method = "spearman", exact = FALSE)
    }, warning = function(w) {
      cor.test(x, y, method = "spearman", exact = FALSE)
    })
    
    # Dodaj do ramki wyników
    wyniki_porownanie <- rbind(wyniki_porownanie, data.frame(
      Para_zmiennych = nazwa_pary,
      Gamma = gamma_wynik$gamma,
      Gamma_p = ifelse(is.na(gamma_wynik$p.value), NA, gamma_wynik$p.value),
      Chi2_stat = as.numeric(chi2_wynik$statistic),
      Chi2_p = chi2_wynik$p.value,
      Pearson_r = as.numeric(pearson_wynik$estimate),
      Pearson_p = pearson_wynik$p.value,
      Spearman_rho = as.numeric(spearman_wynik$estimate),
      Spearman_p = spearman_wynik$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

# Wyświetl wyniki
print("WYNIKI PORÓWNANIA METOD:")
print("========================")
print(wyniki_porownanie)

# Sformatowane wyniki z interpretacją
print("")
print("INTERPRETACJA WYNIKÓW:")
print("======================")

for(i in 1:nrow(wyniki_porownanie)) {
  cat("\n", wyniki_porownanie$Para_zmiennych[i], "\n")
  cat("----------------------------------------\n")
  cat("Gamma:", round(wyniki_porownanie$Gamma[i], 3), 
      " (", interpretuj_gamma(wyniki_porownanie$Gamma[i]), ")\n")
  cat("Chi-kwadrat: statystyka =", round(wyniki_porownanie$Chi2_stat[i], 3), 
      ", p-value =", round(wyniki_porownanie$Chi2_p[i], 4), "\n")
  cat("Pearson r:", round(wyniki_porownanie$Pearson_r[i], 3), 
      " (", interpretuj_pearsona(wyniki_porownanie$Pearson_r[i]), ")\n")
  cat("Spearman ρ:", round(wyniki_porownanie$Spearman_rho[i], 3), 
      " (", interpretuj_pearsona(wyniki_porownanie$Spearman_rho[i]), ")\n")
}

# Wizualizacja porównawcza
print("")
print("4. WIZUALIZACJA PORÓWNAWCZA")
print("===========================")

# Przekształć dane do formatu długiego dla ggplot
library(reshape2)
dane_do_wykresu <- wyniki_porownanie[, c("Para_zmiennych", "Gamma", "Pearson_r", "Spearman_rho")]
dane_dluge <- melt(dane_do_wykresu, id.vars = "Para_zmiennych", 
                   variable.name = "Metoda", value.name = "Wspolczynnik")

# Wykres porównawczy
wykres_porownawczy <- ggplot(dane_dluge, aes(x = Para_zmiennych, y = Wspolczynnik, fill = Metoda)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Porównanie współczynników korelacji",
       subtitle = "Gamma vs Pearson vs Spearman",
       x = "Para zmiennych", y = "Wartość współczynnika") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(type = "qual", palette = "Set2")

print(wykres_porownawczy)

# Podsumowanie rekomendacji
print("")
print("5. REKOMENDACJE")
print("===============")
cat("Dla danych porządkowych (jak Twoja skala kolorów 1-5):\n")
cat("1. GAMMA - najlepszy wybór dla danych porządkowych\n")
cat("2. SPEARMAN - dobra alternatywa, odporna na nietypowe wartości\n")
cat("3. CHI-KWADRAT - do testowania niezależności\n")
cat("4. PEARSON - najmniej odpowiedni (zakłada normalność i linearność)\n")
cat("\nTwoje dane NIE są normalne, więc Pearson może być mylący.\n")
cat("Gamma i Spearman to najlepsze opcje dla Twoich danych.\n")




####### Analiza stabilności wyborów - rozdziały 4.2 i 4.3########
# ===================================================

library(ggplot2)
library(reshape2)
library(vcd)
library(corrplot)
library(gridExtra)

# Funkcje pomocnicze
interpretuj_gamma <- function(gamma) {
  abs_gamma <- abs(gamma)
  if (abs_gamma < 0.1) return("bardzo słaby")
  if (abs_gamma < 0.3) return("słaby")
  if (abs_gamma < 0.5) return("umiarkowany")
  if (abs_gamma < 0.7) return("silny")
  return("bardzo silny")
}

interpretuj_istotnosc <- function(p_value) {
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("ns")
}

print("4.2 BADANIE STABILNOŚCI WYBORÓW WEDŁUG FILARÓW")
print("===============================================")
cat("Analiza współczynników gamma dla związków między filarami:\n\n")

# Przygotowanie wyników dla filarów (bez Pytania 7)
filary_tylko <- c("Filar_Cechy", "Filar_Hierarchia", "Filar_Stres")
wyniki_filary <- data.frame()

for(i in 1:(length(filary_tylko)-1)) {
  for(j in (i+1):length(filary_tylko)) {
    
    x <- dane_korelacji_clean[[filary_tylko[i]]]
    y <- dane_korelacji_clean[[filary_tylko[j]]]
    nazwa_pary <- paste(filary_tylko[i], "↔", filary_tylko[j])
    
    # Gamma
    gamma_wynik <- tryCatch({
      gamma_val <- GoodmanKruskalGamma(x, y)
      if(is.list(gamma_val)) {
        list(gamma = gamma_val$gamma, p.value = gamma_val$p.value)
      } else {
        list(gamma = gamma_val, p.value = NA)
      }
    }, error = function(e) {
      tab <- table(x, y)
      gamma_val <- assocstats(tab)$gamma
      list(gamma = gamma_val, p.value = NA)
    })
    
    # Chi-kwadrat
    chi2_wynik <- chisq.test(table(x, y))
    
    wyniki_filary <- rbind(wyniki_filary, data.frame(
      Para = nazwa_pary,
      Gamma = gamma_wynik$gamma,
      Chi2_p = chi2_wynik$p.value,
      Interpretacja = interpretuj_gamma(gamma_wynik$gamma),
      Istotnosc = interpretuj_istotnosc(chi2_wynik$p.value)
    ))
  }
}

print("WYNIKI ANALIZY STABILNOŚCI MIĘDZY FILARAMI:")
print("==========================================")
for(i in 1:nrow(wyniki_filary)) {
  cat(wyniki_filary$Para[i], "\n")
  cat("Gamma:", round(wyniki_filary$Gamma[i], 3), 
      "(", wyniki_filary$Interpretacja[i], ")", wyniki_filary$Istotnosc[i], "\n\n")
}

# Interpretacja wyników filarów
cat("INTERPRETACJA STABILNOŚCI FILARÓW:\n")
cat("==================================\n")
cat("• Filar_Cechy ↔ Filar_Hierarchia: γ = 0.224 (słaby związek*)\n")
cat("  Studenci wykazują pewną konsekwencję między postrzeganiem cech a hierarchii\n\n")
cat("• Filar_Cechy ↔ Filar_Stres: γ = -0.135 (słaby związek ujemny, ns)\n")
cat("  Negatywny związek - wyższe wartości w cechach wiążą się z niższymi w stresie\n\n")
cat("• Filar_Hierarchia ↔ Filar_Stres: γ = 0.134 (słaby związek*)\n")
cat("  Studenci nieco konsekwentnie postrzegają hierarchię i zarządzanie stresem\n\n")

print("")
print("4.3 ANALIZA STABILNOŚCI: FILARY VS WYBRANA FIRMA")
print("================================================")

# Przygotowanie wyników dla Pytania 7 vs filary
wyniki_pytanie7 <- data.frame()

for(filar in filary_tylko) {
  x <- dane_korelacji_clean[["Pytanie_7"]]
  y <- dane_korelacji_clean[[filar]]
  
  # Gamma
  gamma_wynik <- tryCatch({
    gamma_val <- GoodmanKruskalGamma(x, y)
    if(is.list(gamma_val)) {
      list(gamma = gamma_val$gamma, p.value = gamma_val$p.value)
    } else {
      list(gamma = gamma_val, p.value = NA)
    }
  }, error = function(e) {
    tab <- table(x, y)
    gamma_val <- assocstats(tab)$gamma
    list(gamma = gamma_val, p.value = NA)
  })
  
  # Spearman
  spearman_wynik <- cor.test(x, y, method = "spearman", exact = FALSE)
  
  # Chi-kwadrat
  chi2_wynik <- chisq.test(table(x, y))
  
  wyniki_pytanie7 <- rbind(wyniki_pytanie7, data.frame(
    Filar = filar,
    Gamma = gamma_wynik$gamma,
    Spearman = as.numeric(spearman_wynik$estimate),
    Spearman_p = spearman_wynik$p.value,
    Chi2_p = chi2_wynik$p.value,
    Interpretacja_Gamma = interpretuj_gamma(gamma_wynik$gamma),
    Interpretacja_Spearman = interpretuj_gamma(as.numeric(spearman_wynik$estimate)),
    Istotnosc = interpretuj_istotnosc(spearman_wynik$p.value)
  ))
}

print("WYNIKI ANALIZY: PYTANIE 7 VS FILARY")
print("===================================")
for(i in 1:nrow(wyniki_pytanie7)) {
  cat("Pytanie_7 ↔", wyniki_pytanie7$Filar[i], "\n")
  cat("Gamma:", round(wyniki_pytanie7$Gamma[i], 3), 
      "(", wyniki_pytanie7$Interpretacja_Gamma[i], ")\n")
  cat("Spearman ρ:", round(wyniki_pytanie7$Spearman[i], 3), 
      "(", wyniki_pytanie7$Interpretacja_Spearman[i], ")", 
      wyniki_pytanie7$Istotnosc[i], "\n\n")
}

# WYKRES 1: Porównanie współczynników między filarami
print("WYKRES 1: WSPÓŁCZYNNIKI GAMMA MIĘDZY FILARAMI")
print("=============================================")

dane_filary_wykres <- data.frame(
  Para = c("Cechy ↔ Hierarchia", "Cechy ↔ Stres", "Hierarchia ↔ Stres"),
  Gamma = wyniki_filary$Gamma,
  Istotnosc = wyniki_filary$Istotnosc
)

wykres_filary <- ggplot(dane_filary_wykres, aes(x = Para, y = Gamma, fill = Para)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Gamma, 3), " ", Istotnosc)), 
            vjust = ifelse(dane_filary_wykres$Gamma >= 0, -0.3, 1.3),
            size = 4, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Współczynniki Gamma między filarami",
       subtitle = "Analiza stabilności wyborów studentów (*** p<0.001, ** p<0.01, * p<0.05)",
       x = "Para filarów", 
       y = "Współczynnik Gamma",
       caption = "n = 209 studentów") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  ylim(-0.2, 0.3)

print(wykres_filary)

# WYKRES 2: Porównanie Pytania 7 z filarami
print("")
print("WYKRES 2: ZWIĄZKI PYTANIA 7 Z FILARAMI")
print("======================================")

dane_pytanie7_wykres <- data.frame(
  Filar = c("Cechy", "Hierarchia", "Stres"),
  Gamma = wyniki_pytanie7$Gamma,
  Spearman = wyniki_pytanie7$Spearman,
  Istotnosc = wyniki_pytanie7$Istotnosc
)

# Przekształć dane do formatu długiego
dane_pytanie7_dlugie <- melt(dane_pytanie7_wykres[,c("Filar", "Gamma", "Spearman")], 
                             id.vars = "Filar", variable.name = "Metoda", value.name = "Wspolczynnik")

# Dodaj informacje o istotności
dane_pytanie7_dlugie$Istotnosc <- rep(dane_pytanie7_wykres$Istotnosc, 2)

wykres_pytanie7 <- ggplot(dane_pytanie7_dlugie, aes(x = Filar, y = Wspolczynnik, fill = Metoda)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(Wspolczynnik, 3), 
                               ifelse(Metoda == "Spearman", paste0(" ", Istotnosc), ""))), 
            position = position_dodge(width = 0.7),
            vjust = ifelse(dane_pytanie7_dlugie$Wspolczynnik >= 0, -0.3, 1.3),
            size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Związki wyboru firmy (Pytanie 7) z filarami organizacyjnymi",
       subtitle = "Porównanie współczynników Gamma i Spearman (*** p<0.001, ** p<0.01, * p<0.05)",
       x = "Filary organizacyjne", 
       y = "Wartość współczynnika",
       fill = "Metoda",
       caption = "n = 209 studentów") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = c("Gamma" = "#66c2a5", "Spearman" = "#fc8d62")) +
  ylim(-0.05, 0.35)

print(wykres_pytanie7)

# PODSUMOWANIE KOŃCOWE
print("")
print("PODSUMOWANIE WYNIKÓW ANALIZY STABILNOŚCI")
print("========================================")

cat("4.2 STABILNOŚĆ MIĘDZY FILARAMI:\n")
cat("Współczynniki gamma wykazują słabe związki między filarami (0.134-0.224),\n")
cat("co wskazuje na względną niezależność wymiarów organizacyjnych w percepcji studentów.\n")
cat("Najsilniejszy związek: Cechy ↔ Hierarchia (γ = 0.224*)\n\n")

cat("4.3 ZWIĄZKI WYBORU FIRMY Z FILARAMI:\n")
cat("Istotny statystycznie związek tylko z Filarem Hierarchia:\n")
cat("• Filar Hierarchia: γ = 0.324, ρ = 0.294*** (umiarkowany związek)\n")
cat("• Filar Cechy: γ = 0.126, ρ = 0.093 (bardzo słaby, nieistotny)\n")
cat("• Filar Stres: γ = -0.014, ρ = -0.008 (praktycznie brak związku)\n\n")

cat("WNIOSKI KLUCZOWE:\n")
cat("1. Studenci wykazują konsekwencję głównie w wymiarze hierarchii\n")
cat("2. Wybór firmy koreluje z postrzeganiem struktur hierarchicznych\n")
cat("3. Pozostałe wymiary (cechy, stres) są niezależne od wyboru firmy\n")
cat("4. Stabilność wyboru jest umiarkowana i selektywna\n")

# Tabela podsumowująca dla tekstu
print("")
print("TABELA PODSUMOWUJĄCA DLA RAPORTU:")
print("=================================")
tabela_podsumowanie <- data.frame(
  "Związek" = c("Pytanie 7 ↔ Filar Cechy", 
                "Pytanie 7 ↔ Filar Hierarchia", 
                "Pytanie 7 ↔ Filar Stres",
                "Filar Cechy ↔ Filar Hierarchia",
                "Filar Cechy ↔ Filar Stres",
                "Filar Hierarchia ↔ Filar Stres"),
  "Gamma" = c(0.126, 0.324, -0.014, 0.224, -0.135, 0.134),
  "Spearman_rho" = c(0.093, 0.294, -0.008, 0.181, -0.103, 0.128),
  "Interpretacja" = c("Słaby", "Umiarkowany***", "Bardzo słaby", 
                      "Słaby*", "Słaby", "Słaby*"),
  stringsAsFactors = FALSE
)

print(tabela_podsumowanie)
