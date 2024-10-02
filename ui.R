
require(shiny)
require(shinydashboard)
require(fresh)
require(DT)

require(reticulate)         # hub do Pythona

# ---------------------------

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

# ------------------------------------------------------------------------------
# Dashboard header 

header <- dashboardHeader(title = "Analiza wstępna bazy danych")  

# Sidebar  

sidebar <- dashboardSidebar(
  radioButtons(inputId = "wybor", label = "Uaktywnij bazę:", choices = c("R", "Python"), selected = "R"),  
  selectInput("baza_r", label = "Wybierz bazę R", choices = c(data(iris), data(airquality), data(mtcars),
              data(longley), data(faithful), data(women))), # okienko wyboru bazy
  selectInput("baza_p", label = "Wybierz bazę Pythona", choices = c("iris_py", "diabetes_py", "wine_py",
              "linnerud_py","california_housing_py","breast_cancer_py"))
)

# ------------------------------------------------------------------------------
# Fliud rows

frow0 <- fluidRow(
  box(width = 12, height = 300,
  br(),
  p(strong("Opis1", style = 'font-size:20px;color:#6f8dbf;')),
  p("qwereruuii", style = "color: #6f8dbf"),
  br(),br(),br(),br(),br(),
  br(),
  ),
  box(width = 12, height = 400,
      br(),
      p(strong("Opis2", style = 'font-size:20px;color:#6f8dbf;')),
      p("qwereruuii", style = "color: #6f8dbf"),
      br(),br(),br(),br(),br(),
      br(),
      hr(style = "border-top: 1px solid #6f8dbf"),
      p("Aplikację utworzono w R i RStudio. Zastosowane pakiety:", style = "color: #6f8dbf"),
      p("* System Library,", style = "color: #6f8dbf"),
      p("* User Library: shiny, shinydashboard, fresh, DT, reticulate,", style = "color: #6f8dbf"),
      p("* oraz funkcje Pythona zawarte w plikach zewnętrznych.", style = "color: #6f8dbf"),
      p("Autor: ", style = "color: #6f8dbf")
  ),
)

frow1 <- fluidRow(
  box(
    title = "Baza",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = 730,
    DTOutput("tabela"),
  ), 
)

frow2 <- fluidRow( 
  # Fluid row in dashboard body
  box(
    title = "Podsumowanie",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = 600, 
    verbatimTextOutput("podsumowanie"),
  ),
)

frow3 <- fluidRow(                                            # Fluid row in dashboard body
  column(width = 6,
    box(
        title = "Histogram",
        status = "primary",
        solidHeader = TRUE, 
        collapsible = TRUE, 
        width = 12,
        height = 460,
        plotOutput("histogram")
  ),
  box(
    title = "Wybierz numer kolumny bazy:",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,
    height = 100,
    numericInput("nr_kolumny",label = NULL ,  1, min = 1, max = 10)    
  ),
  box(
      title = "Liczba słupków w histogramie:",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      width = 12,
      height = 130,
      sliderInput("slupki",                                                          
                  label = NULL,
                  min = 1,
                  max = 50,
                  value = 15)
    )
  ),
  box(
    title = "Tablica korelacji",
    status = "primary",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 6,
    height = 600,
#    plotOutput("histogram")
  )  
)

frow4 <- fluidRow(                                            # Fluid row in dashboard body
  column(width = 6,
         box(
           title = "Wykres punktowy",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = 460,
 #          plotOutput("histogram")
         ),
         box(
           title = "Wybierz nr kolumny x:",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = 100,
           numericInput("nr_kolumny_x",label = NULL ,  1, min = 1, max = 10)    
         ),
         box(
           title = "Wybierz nr kolumny y:",
           status = "primary",
           solidHeader = TRUE, 
           collapsible = TRUE, 
           width = 12,
           height = 100,
           numericInput("nr_kolumny_y",label = NULL ,  1, min = 1, max = 10)    
         ),
  )
)
# ------------------------------------------------------------------------------
# Dashboard body

body <- dashboardBody(
  box(width=12, height =800,
      tabsetPanel(                            
        tabPanel("Strona tytułowa",
                 frow0
          ),  
        tabPanel("Baza",
                 frow1
          ),  
          
        tabPanel("Analiza",
                 frow2
        ),
        tabPanel("Wykresy1",
                 frow3
        ),        
        tabPanel("Wykresy2",
                 frow4
        ),        
      )
  )
)

# ------------------------------------------------------------------------------
# Dashboard page

ui <- dashboardPage(
  header,
  sidebar,
  body,
  use_theme(mytheme)
)
