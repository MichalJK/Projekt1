
# Plik ui do aplikacji "Analiza wstępna bazy danych"

# ------------------------------------------------------------------------------

require(shiny)
require(shinydashboard)
require(fresh)
require(DT)

require(shiny.i18n)

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

i18n <- Translator$new(translation_csvs_path = "./data/", separator_csv = ",")
i18n$set_translation_language("pl") 
shiny.i18n::usei18n(i18n)
# --------------------------- Dashboard header ---------------------------------

header <- dashboardHeader(titleWidth = 600, title = i18n$t("Analiza wstępna bazy danych / Initial database analysis"))

# --------------------------- Sidebar ------------------------------------------  

sidebar <- dashboardSidebar(width = "17vw",
  shiny.i18n::usei18n(i18n),
  selectInput('selected_language', "Przełącz język / Switch the language",
              choices = i18n$get_languages(),
              selected = i18n$get_key_translation()),
  radioButtons(inputId = "wybor", label = i18n$t("Uaktywnij bazę:"),
               choices = c("R", "Python", "Plik / File"), selected = "R"),  
  selectInput("baza_r", label = i18n$t("Wybierz bazę R"),
              choices = c(data(iris), data(airquality), data(mtcars),
              data(longley), data(faithful), data(women), data(retinopathy,
              package="survival"))), # okienko wyboru bazy
  selectInput("baza_p", label = i18n$t("Wybierz bazę Pythona"),
              choices = c("iris_py", "diabetes_py", "wine_py",
              "linnerud_py","california_housing_py","breast_cancer_py")),
  fileInput("plik",label = i18n$t("Wczytaj z pliku"), accept = c(".txt",".csv")),
  div(style = "margin-top:-55px"),
  checkboxInput("header",i18n$t("Nagłówek pliku"),TRUE),
  div(style = "margin-top:-25px"),
  radioButtons(inputId = "sep", label = i18n$t("Separator pliku"),
               choices = c( ",", ";",Tab = "\t"),
               selected = "\t")
  )

# -------------------------- Fliud rows  --------------------------------------

frow0 <- fluidRow(                          # zakładka "Strona tytułowa" - góra
    column( width = 12, 
      
      br(),
      
      p(strong(i18n$t("Analiza wstępna bazy danych"), style = 'font-size:4vh; color:#5d759e;')),
      
      column(width = 6, height = "auto", style = 'font-size:2.5vh; color:#4e6387',
     
            br(),
           
            p(i18n$t("Aplikacja wykorzystuje bazy ćwiczebne zawarte w bibliotekach R i Pythona. Istnieje możliwość wczytania własnej bazy danych z urządzenia lokalnego.")),
      
            p(i18n$t("Aplikacja przeprowadza następującą analizę baz danych:"),style = "margin-bottom: 0px;"),
            # tags$ul(
            #     tags$li("określa typ zmiennych (kolumn bazy),"),
            #     tags$li("liczbę wierszy i kolumn,"),
            #     tags$li("wartość średnią, medianę, pierwszy i trzeci kwartyl zmiennych,"),
            #     tags$li("odchylenie standardowe i rozstęp,"),
            #     tags$li("kurtozę i skośność,"),
            #     tags$li("wykreśla histogram wybranej zmiennej oraz wykres kwantylowy Q-Q,"),
            #     tags$li("wykonuje wykres punktowy zależności wybranych zmiennych 
            #             wraz z regresją metodą 'loess',"),
            #     tags$li("wykonuje wykres tabeli korelacji zmiennych ilościowych.")
            # ),
            
            p(i18n$t("- określa typ zmiennych (kolumn bazy);"),style = "margin-bottom: 0px;"),
            p(i18n$t("- liczbę wierszy i kolumn;"),style = "margin-bottom: 0px;"),
            p(i18n$t("- wartość średnią"),",",i18n$t(" medianę"),",",i18n$t(" pierwszy i trzeci kwartyl zmiennych;"),style = "margin-bottom: 0px;"),
            p(i18n$t("- odchylenie standardowe i rozstęp;"),style = "margin-bottom: 0px;"),
            p(i18n$t("- kurtozę i skośność;"),style = "margin-bottom: 0px;"),
            p(i18n$t("- wykreśla histogram wybranej zmiennej oraz wykres kwantylowy Q-Q;"),style = "margin-bottom: 0px;"),
            p(i18n$t("- wykonuje wykres punktowy zależności wybranych zmiennych;"),style = "margin-bottom: 0px;"),
            p(i18n$t("- wykonuje wykres tabeli korelacji zmiennych ilościowych."),style = "margin-bottom: 0px;"),
        ),

      column(width = 6, height = "auto",img(src="iris.png", height = "auto", width = "100%"),  align = "center"),          
      
     ),
)

frow02 <- fluidRow(                            # zakładka "Strona tytułowa" - środek
  column(width = 12,style = 'font-size:2.5vh;  color:#4e6387',
         
      p(i18n$t("Poszczególne funkcje aplikacji realizowane są na następnych zakładkach.")),
      
      p(i18n$t("Dopuszczalny format plików zewnętrznych: .txt oraz .csv. Kolumny zmiennych mogą być oddzielone przecinkiem"),",",
        i18n$t(" średnikiem lub tabem. Mogą posiadać nagłówki lub nie.")),
  )
)

frow01 <- fluidRow(                             # zakładka "Strona tytułowa" - dół
  column(width = 12,  style = 'color:#6f8dbf',
      hr(style = "border-top: 1px solid #6f8dbf"),
      p(i18n$t("Aplikację utworzono w R i RStudio. Zastosowane pakiety:"),style = "margin-bottom: 0px;"),
      
      # tags$ul(
      #         tags$li("System Library,"),
      #         tags$li("User Library: shiny, shinydashboard, fresh, DT, survival,
      #                 reticulate, ggplot2, corrplot, moments"),
      #         tags$li("oraz biblioteki Pythona (pandas i sklearn) i funkcje Pythona zawarte w plikach zewnętrznych.")
      # ),
      
      p("- System Library;",style = "margin-bottom: 0px;"),
      p("- User Library: shiny, shinydashboard, fresh, DT, survival,
                       reticulate, ggplot2, corrplot, moments, shiny.i18n;",style = "margin-bottom: 0px;"),
      p(i18n$t("- oraz biblioteki Pythona (pandas i sklearn) i funkcje Pythona zawarte w plikach zewnętrznych.")),
      
      p(i18n$t("Autor"),": Michał Kołodziejczyk"),
  )
)

frow1 <- fluidRow(                           # Zakładka "Baza"
  box(
    title = i18n$t("Baza"),
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = "auto",
    DTOutput("tabela", height = "auto"),
  ),  
)

frow2 <- fluidRow(                          # Zakładka "Statystyka opisowa"
  # Fluid row in dashboard body
  box(
    title = i18n$t("Podstawowe dane statystyczne"),
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = "auto",
    verbatimTextOutput("podsumowanie"),
  ),
  box(
    title = i18n$t("Dodatkowe informacje"),
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
      title = i18n$t("Wybierz numer kolumny bazy:"),
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      width = 12,
      height =  "auto",
      numericInput("nr_kolumny",label = NULL ,  1, min = 1, max = 40)    
  ),
  box(
      title = i18n$t("Liczba słupków w histogramie:"),
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
        title = i18n$t("Wykres kwantylowy Q-Q"),
        status = "primary",
        solidHeader = TRUE, 
        collapsible = TRUE, 
        width = 12,
        height = "auto",
#        plotOutput("qq", height = "62vh")
        plotOutput("qq", height = "550")
      ), 
      
      box(
        title = i18n$t("Wybierz numer kolumny bazy:"),
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
           title = i18n$t("Wykres punktowy"),
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
           plotOutput("xy", height = "400")
         ),
         box(
           title = i18n$t("Wybierz nr kolumny x:"),
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
           numericInput("nr_kolumny_x",label = NULL ,  1, min = 1, max = 40)    
         ),
         box(
           title = i18n$t("Wybierz nr kolumny y:"),
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
           title = i18n$t("Tabela korelacji"),
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = "auto",
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
        tabPanel(i18n$t("Strona tytułowa"),height = "auto",  
                 frow0,
                 frow02,
                 frow01
       ),  
        tabPanel(i18n$t("Baza"),height = "auto",  
                 frow1
        ),  
          
        tabPanel(i18n$t("Statystyka opisowa"),height = "auto",  
                 frow2
        ),
        tabPanel(i18n$t("Wykresy1"),height = "auto",  
                 frow3
        ),        
        tabPanel(i18n$t("Wykresy2"),height = "auto",  
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
