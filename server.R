
require(shiny)
require(shinydashboard)
require(DT)

require(reticulate)

# Define server logic required to draw a histogram

# ------------------------------------------------------------------------------

function(input, output, session) {
  
# --------------------------  
  
# ładowanie baz z Pythona  

  pd <- import("pandas")
  sk <- import("sklearn.datasets")

  iris_py = sk$load_iris(as_frame=TRUE)$frame
  diabetes_py = sk$load_diabetes(as_frame=TRUE)$frame
  wine_py = sk$load_wine(as_frame=TRUE)$frame
  linnerud_py = sk$load_linnerud(as_frame=TRUE)$frame
  breast_cancer_py = sk$load_breast_cancer(as_frame=TRUE)$frame
  california_housing_py = sk$fetch_california_housing(as_frame=TRUE)$frame
  
# ------------------------  
  
# dataset - aktualnie analizowana baza
  
# ------------------------  
    
  output$tabela <- renderDT({
 
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else {
      dataset <- get(input$baza_p)
    }
    dataset
  }, options = list(pageLength = 13,  scrollX = T )
  )         

# -----------------------
  
  output$podsumowanie <- renderPrint(                      
    width =800,
    {
    if (input$wybor == "R"){
      cat(paste("Baza: ", input$wybor,": ", input$baza_r,"\n\n"))
      dataset <- get(input$baza_r)
      print(summary(dataset))
    } else {
      cat(paste("Baza: ", input$wybor,": ", input$baza_p,"\n\n"))
      source_python("podsumowanie.txt")
      dataset <- get(input$baza_p)
      print(summary(dataset))   #funkcja Pythona
    }
    summary(dataset)
  })

  output$info <- renderPrint(
    width = 800,
    {                      
      if (input$wybor == "R"){
        dataset <- get(input$baza_r)
      } else {
        source_python("podsumowanie.txt")
        dataset <- get(input$baza_p)
      }
      cat("liczba wierszy =",as.character(nrow(dataset)))
      cat("\nliczba kolumn =",as.character(ncol(dataset)),"\n\n")
      cat("Typy zmiennych:\n")
      print(sapply(dataset, class))
      cat("\nOdchylenie standardowe:\n")
      # licznik faktorów
      dfff <- dataset
      licznik <- 0                     
      wek <- rep(0, ncol(dataset))
      for (i in 1:ncol(dataset)){
        if(class(dataset[,i]) == "factor"){
          licznik <- licznik + 1
          wek[licznik] <- i
        }
      }
      # wyznaczanie sd dla danych ilościowych
      if (licznik != 0){
        dfff <- dfff[-wek]
      }
      
      print(sapply(dfff,sd))
      
    })

  
  # ---------------------
  
  output$histogram <- renderPlot({                         
    
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else {
      dataset <- get(input$baza_p)
    }
    
    ii <- input$nr_kolumny                                     # numer kolumny
    x    <- dataset[,ii]                                       # kolumna w wybranej bazie
    ss <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    bins <- seq(min(x), max(x), length.out = input$slupki+ 1)  # wektor punktów pomiędzy słupkami histogramu
                                                               # length.out	- pożądana liczba słupków
    hist(x, breaks = bins, col = '#6f8dbf', border = 'white',
         xlab = ss,
         main = paste('Histogram: ',ss)
    )
  })    

}
