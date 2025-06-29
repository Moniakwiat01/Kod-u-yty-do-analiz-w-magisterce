# Kod-uzyty-do-analiz-w-magisterce
Upubliczniony kod posłużył do analiz w magisterce pod tytułem: "Turkus czy czerwień? Preferencje studentów wobec modeli kultur organizacyjnych.
Analiza Wzorców Preferencji Wyboru Koloru Organizacji Wśród Studentów
Opis projektu
Repozytorium zawiera kod R i analizy statystyczne z pracy magisterskiej dotyczącej badania preferencji studentów w zakresie wyboru koloru organizacji jako elementu kultury organizacyjnej i zarządzania.
**Zakres badania**
Praca koncentruje się na:
Analizie wzorców preferencji kolorystycznych organizacji wśród studentów
Badaniu stabilności wyborów według różnych charakterystyk
Zastosowaniu wielowymiarowych metod statystycznych do segmentacji respondentów
Konstrukcji  filarów badawczych preferencji

**Metodologia**
Metody statystyczne zastosowane w R:
Analiza wielowymiarowa - badanie złożonych relacji między zmiennymi
Analiza skupień - segmentacja respondentów
Miary związku dla zmiennych porządkowych - badanie korelacji
Analiza stabilności wyboru - badanie konsistencji preferencji
Analiza według charakterystyk demograficznych - segmentacja populacji

**Narzędzia i pakiety R:**
r# Główne pakiety wykorzystane w analizie
library(cluster)          # analiza skupień
library(FactoMineR)       # analiza wielowymiarowa
library(factoextra)       # wizualizacja wyników PCA/CA
library(corrplot)         # macierze korelacji
library(ggplot2)          # wizualizacje
library(dplyr)            # manipulacja danych
library(psych)            # psychometria i statystyki opisowe
Struktura projektu
├── data/                    # Dane z badania ankietowego
├── scripts/                 # Skrypty R z analizami
│   ├── 01_analiza_wstepna.R
│   ├── 02_analiza_demograficzna.R
│   ├── 03_analiza_wielowymiarowa.R
│   └── 04_analiza_skupien.R
├── plots/                   # Wykresy i wizualizacje
├── reports/                 # Raporty R Markdown
└── README.md
**Główne komponenty analizy**
1. Analiza wstępna (Rozdział 3.2)

Statystyki opisowe zmiennych
Analiza rozkładów preferencji kolorystycznych
Podstawowe charakterystyki próby badawczej

2. Analiza demograficzna (Rozdział 3.4)

Segmentacja według płci, wieku, kierunku studiów
Badanie różnic w preferencjach między grupami

3. Konstrukcja filarów badawczych (Rozdział 4.1)

Wysczególnienie filarów dla ułatwienia analiz próby
Walidacja skal pomiarowych

4. Analiza stabilności (Rozdziały 4.2-4.3)

Badanie konsistencji wyborów respondentów
Analiza zgodności preferencji vs rzeczywisty wybór firmy
Miary stabilności

5. Analiza skupień (Rozdział 4.4)

Segmentacja respondentów metodą PAM
Identyfikacja grup o podobnych preferencjach
Charakterystyka profili segmentów

**Kluczowe wyniki**
Badanie pozwoliło na:

Identyfikację głównych wzorców preferencji kolorystycznych wśród studentów
Określenie czynników demograficznych wpływających na wybory
Segmentację populacji na grupy o podobnych preferencjach
Ocenę stabilności i konsistencji wyborów

Jak uruchomić analizy

Przygotowanie środowiska:

r# Instalacja wymaganych pakietów
install.packages(c("cluster", "FactoMineR", "factoextra", 
                   "corrplot", "ggplot2", "dplyr", "psych"))

Uruchomienie analiz:

r# Załaduj dane
load("data/dane_badania.RData")

# Uruchom skrypty w kolejności
source("scripts/01_analiza_wstepna.R")
source("scripts/02_analiza_demograficzna.R")
source("scripts/03_analiza_wielowymiarowa.R")
source("scripts/04_analiza_skupien.R")
Techniczne informacje

Język programowania: R
Środowisko: RStudio
Wielkość próby: [209 respondentów]
Metoda zbierania danych: Badanie sondażowe

Struktura teoretyczna
Praca osadzona jest w teorii zarządzania organizacjami z uwzględnieniem:

Teorii kultury organizacyjnej
Koncepcji barw organizacji
Współczesnych teorii zarządzania i przedsiębiorczości

**Autor**
Monika Kwiatkowska
Praca magisterska - Uniwersytet Gdański,2025
**Licencja**
Kod udostępniony na potrzeby prezentacji wyników badań akademickich.
