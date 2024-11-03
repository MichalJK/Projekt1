Jest to moja pierwsza Shiny. Temat wynika z moich zainteresować analizą danych. Aplikacja jest przykładem współpracy R i Pythona w jednym projekcie. 
Aplikacja jest dostępna na AWS (Amazon Web Services ) pod adresem: http://16.171.39.95:3838/Projekt1/
# Temat: Analiza wstępna bazy danych
Aplikacja wykorzystuje bazy ćwiczebne zawarte w bibliotekach R i Pythona. 

Zaimplementowane bazy R:
- iris,
- airquality, 
- mtcars,
- longley, 
- faithful, 
- women,
- retinopathy.

Bazy Pythona:
-	iris, 
-	diabetes, 
-	wine,              
-	linnerud,
-	california_housing,
-	breast_cancer.

Istnieje możliwość wczytania własnej bazy danych z urządzenia lokalnego. Dopuszczalny format plików zewnętrznych: .txt oraz .csv. Kolumny zmiennych mogą być oddzielone przecinkiem, średnikiem lub tabem. Mogą posiadać nagłówki lub nie.

Aplikacja przeprowadza następującą analizę baz danych:
-	określa typ zmiennych (kolumn bazy),
-	liczbę wierszy i kolumn,
-	wartość średnią, medianę, pierwszy i trzeci kwartyl zmiennych,
-	odchylenie standardowe i rozstęp,
-	kurtozę i skośność,
-	wykreśla histogram wybranej zmiennej oraz wykres kwantylowy Q-Q,
-	wykonuje wykres punktowy zależności wybranych zmiennych wraz z regresją metodą 'loess',
-	wykonuje wykres tabeli korelacji zmiennych ilościowych.

Poszczególne funkcje aplikacji realizowane są na kolejnych zakładkach.

Aplikację utworzono w RStudio. Zastosowane pakiety:
-	System Library,
-	User Library: shiny, shinydashboard, fresh, DT, survival, reticulate, ggplot2, corrplot, moments,
-	oraz biblioteki Pythona (pandas i sklearn) i funkcje Pythona zawarte w plikach zewnętrznych.











