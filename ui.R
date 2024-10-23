
# Plik ui do aplikacji "Analiza wstępna bazy danych"

# ------------------------------------------------------------------------------

require(shiny)
require(shinydashboard)
require(fresh)
require(DT)


# --------------------------- theme aplikacji ----------------------------------

mytheme <- create_theme(    # z pakietu 'fresh'
  adminlte_color(
    light_blue = "#6f8dbf" 
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#465878",
    dark_hover_bg = "#5e6a85", 
    dark_color = "#2E3440"  ),
  adminlte_global(
    box_bg = "#e4e7ed", 
    content_bg = "#b8c5de",
    info_box_bg = "#8c93a1"
  )
)

# --------------------------- Dashboard header ---------------------------------

header <- dashboardHeader(title = "Analiza wstępna bazy danych")  

# --------------------------- Sidebar ------------------------------------------  

sidebar <- dashboardSidebar(width = "17vw",
  radioButtons(inputId = "wybor", label = "Uaktywnij bazę:",
               choices = c("R", "Python", "Plik"), selected = "R"),  
  selectInput("baza_r", label = "Wybierz bazę R",
              choices = c(data(iris), data(airquality), data(mtcars),
              data(longley), data(faithful), data(women), data(retinopathy,
              package="survival"))), # okienko wyboru bazy
  selectInput("baza_p", label = "Wybierz bazę Pythona",
              choices = c("iris_py", "diabetes_py", "wine_py",
              "linnerud_py","california_housing_py","breast_cancer_py")),
  fileInput("plik",label = "Wczytaj z pliku", accept = c(".txt",".csv")),
  checkboxInput("header","Nagłówek pliku",TRUE),
  radioButtons(inputId = "sep", label = "Separator pliku",
               choices = c(Przecinek = ",", Średnik = ";",Tab = "\t"),
               selected = "\t")
  )

# -------------------------- Fliud rows  --------------------------------------

frow0 <- fluidRow(                          # zakładka "Strona tytułowa" - góra
    column( width = 12, 
      
      br(),
      
      p(strong("Analiza wstępna bazy danych", style = 'font-size:4vh; color:#5d759e;')),
      
  #    column(width = 6, height = "auto",img(src="iris.png", height = "auto", width = "100%"),  align = "center"),  
      
     column(width = 6, height = "auto", style = 'font-size:2.5vh; color:#4e6387',
     
            br(),
           
            p("Aplikacja wykorzystuje bazy ćwiczebne zawarte w bibliotekach R
              i Pythona. Istnieje możliwość wczytania własnej
              bazy danych z urządzenia lokalnego."),
      
            p("Aplikacja przeprowadza następującą analizę baz danych:"),
            tags$ul(
              tags$li("określa typ zmiennych (kolumn bazy),"),
              tags$li("liczbę wierszy i kolumn,"),
              tags$li("wartość średnią, medianę, pierwszy i trzeci kwartyl zmiennych,"),
              tags$li("odchylenie standardowe i rozstęp,"),
              tags$li("kurtozę i skośność,"),
              tags$li("wykreśla histogram wybranej zmiennej oraz wykres kwantylowy Q-Q,"),
              tags$li("wykonuje wykres punktowy zależności wybranych zmiennych 
                        wraz z regresją metodą 'loess',"),
              tags$li("wykonuje wykres tabeli korelacji zmiennych ilościowych.")
            ),
          

        ),

 
           column(width = 6, height = "auto",img(src="iris.png", height = "auto", width = "100%"),  align = "center"),     

    
     ),
)

frow02 <- fluidRow(
 column(width = 12,style = 'font-size:2.5vh; color:#6f8dbf',
  p("Poszczególne funkcje aplikacji realizowane są na następnych zakładkach.",
    style = ' color:#4e6387'),
  
  p("Dopuszczalny format plików zewnętrznych: .txt oraz .csv. 
              Kolumny zmiennych mogą być oddzielone przecinkiem, średnikiem lub tabem.
              Mogą posiadać nagłówki lub nie.", style = ' color:#4e6387'),
  )
  
)

frow01 <- fluidRow(                          # zakładka "Strona tytułowa" - dół
  column(width = 12,  style = 'color:#6f8dbf',
      hr(style = "border-top: 1px solid #6f8dbf"),
      p("Aplikację utworzono w R i RStudio. Zastosowane pakiety:"),
      
      tags$ul(
              tags$li("System Library,"),
              tags$li("User Library: shiny, shinydashboard, fresh, DT, survival,
                      reticulate, ggplot2, corrplot, moments"),
              tags$li("oraz biblioteki Pythona (pandas i sklearn) i funkcje Pythona zawarte w plikach zewnętrznych."),
      ),
      
      p("Autor: Michał Kołodziejczyk"),
  )
)

frow1 <- fluidRow(                          # Zakładka "Baza"
  box(
    title = "Baza",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = "auto",
    DTOutput("tabela", height = "auto"),
  ),  
)

frow2 <- fluidRow(                          # Zakładka "Analiza"
  # Fluid row in dashboard body
  box(
    title = "Podstawowe Dane Statystyczne",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = "auto",
    verbatimTextOutput("podsumowanie"),
  ),
  box(
    title = "Dodatkowe informacje",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = "auto",  
    verbatimTextOutput("info"),
  ),
  
)

frow3 <- fluidRow(                         # Zakłądka "Wykresy 1"

  column(width = 6,
    box(
        title = "Histogram",
        status = "primary",
        solidHeader = TRUE, 
        collapsible = TRUE, 
        width = 12,
        height = "auto",
#        plotOutput("histogram", height = "45vh")
        plotOutput("histogram", height = "400")
  ),
  box(
      title = "Wybierz numer kolumny bazy:",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      width = 12,
      height =  "auto",
      numericInput("nr_kolumny",label = NULL ,  1, min = 1, max = 40)    
  ),
  box(
      title = "Liczba słupków w histogramie:",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      width = 12,
      height = "auto", 
      sliderInput("slupki",                                                          
                  label = NULL,
                  min = 1,
                  max = 50,
                  value = 15)
    )
  ),

  column(width = 6,

      box(
        title = "Wykres kwantylowy Q-Q",
        status = "primary",
        solidHeader = TRUE, 
        collapsible = TRUE, 
        width = 12,
        height = "auto",
#        plotOutput("qq", height = "62vh")
        plotOutput("qq", height = "550")
      ), 
      
      box(
        title = "Wybierz numer kolumny bazy:",
        status = "primary",
        solidHeader = TRUE, 
        collapsible = TRUE, 
        width = 12,
        height = "auto", 
        numericInput("nr_kolumny_1",label = NULL ,  1, min = 1, max = 40)    
      ),      
  ) 
)

frow4 <- fluidRow(                         # Zakładka "Wykresy 2"                   
  column(width = 6,
         box(
           title = "Wykres punktowy",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
#           plotOutput("xy", height = "45vh")
          plotOutput("xy", height = "400")
         ),
         box(
           title = "Wybierz nr kolumny x:",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
           numericInput("nr_kolumny_x",label = NULL ,  1, min = 1, max = 40)    
         ),
         box(
           title = "Wybierz nr kolumny y:",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
           numericInput("nr_kolumny_y",label = NULL ,  1, min = 1, max = 40)    
         ),
  ),
  
  column(width = 6,
         
         box(
           title = "Tabela korelacji",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
#           plotOutput("kor", height = "74vh")
          plotOutput("kor", height = "660")
         ),        
  )

)

# ----------------------------- Dashboard body ---------------------------------

body <- dashboardBody( 

    tags$head(tags$style(           # pozycja okienka "shownotification"
      HTML(".shiny-notification {
             position:fixed;
             top: calc(70%);
             left: calc(0%);
             }")
     )),

  box(width=12, height = "auto", 

    tabsetPanel(                          
        tabPanel("Strona tytułowa",height = "auto",  
                 frow0,
                 frow02,
                 frow01
       ),  
        tabPanel("Baza",height = "auto",  
                 frow1
        ),  
          
        tabPanel("Statystyka opisowa",height = "auto",  
                 frow2
        ),
        tabPanel("Wykresy1",height = "auto",  
                 frow3
        ),        
        tabPanel("Wykresy2",height = "auto",  
                 frow4
        ),        
      )
  )
)

# ------------------------------ Dashboard page --------------------------------

ui <- dashboardPage(
  header,
  sidebar,
  body,
  use_theme(mytheme)
)
