Jest to moja pierwsza aplikacja Shiny. Temat wynika z moich zainteresować analizą danych. Aplikacja jest przykładem współpracy R i Pythona w jednym projekcie. 

Aplikacja jest dostępna pod adresem: michal-kolodziejczyk.shinyapps.io/Projekt1/

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
-	oraz biblioteki Pythona (pandas i scikit-learn) i funkcje Pythona zawarte w plikach zewnętrznych.





&nbsp;

--------------------------------------------------------------------------------------------------------------------------------------





This is my first Shiny app. The topic stems from my interest in data analysis. The app demonstrates how R and Python can work together in a single project.

The app is available at:  michal-kolodziejczyk.shinyapps.io/Projekt1/

# Topic: Preliminary Database Analysis

The application uses sample datasets included in the R and Python libraries.

Implemented R databases:
- iris,
- airquality, 
- mtcars,
- longley, 
- faithful, 
- women,
- retinopathy.


Python databases:
-	iris, 
-	diabetes, 
-	wine,              
-	linnerud,
-	california_housing,
-	breast_cancer.

You can import your own database from a local device. Supported file formats are .txt and .csv. Variable columns can be separated by commas, semicolons, or tabs. They may or may not have headers.

The application performs the following database analysis:
-    determines the type of variables (database columns),
-    the number of rows and columns,
-    the mean, median, first and third quartiles of the variables,
-    standard deviation and range,
-    kurtosis and skewness,
-    plots a histogram of the selected variable and a Q-Q plot,
-    generates a scatter plot of the relationship between selected variables along with a ‘loess’ regression,
-    generates a correlation table for quantitative variables.


The app's various features are accessible via separate tabs.


The application was created in RStudio. Packages used:
-    System Library,
-    User Library: shiny, shinydashboard, fresh, DT, survival, reticulate, ggplot2, corrplot, moments,
-    as well as Python libraries (pandas and scikit-learn) and Python functions included in external files.
