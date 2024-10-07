
require(shiny)
require(shinydashboard)
require(DT)

require(reticulate)
require(car)
require(ggplot2)
require(corrplot)

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
        if(class(dataset[,i]) != "numeric" & class(dataset[,i]) != "integer"){
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
    x    <- na.omit(dataset[,ii])                                       # kolumna w wybranej bazie
    ss <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    bins <- seq(min(x), max(x), length.out = input$slupki+ 1)  # wektor punktów pomiędzy słupkami histogramu
                                                               # length.out	- pożądana liczba słupków
    hist(x, breaks = bins, col = '#6f8dbf', border = 'white',
         xlab = ss,
         main = paste('Histogram: ',ss)
    )
  })    

  output$qq <- renderPlot({                         
    
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else {
      dataset <- get(input$baza_p)
    }
    
    ii <- input$nr_kolumny_1                                     # numer kolumny
    x    <- na.omit(dataset[,ii])                                       # kolumna w wybranej bazie
    ss <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    
  
      if(class(x) == "numeric" || class(x) == "integer") {
        srednia <- mean(x)
        odch <- sd(x)
        
        xx <- pnorm(x, mean = srednia, sd = odch)
        xx <- qnorm(xx)
        
        qqPlot(xx, dist = "norm", col = "#6f8dbf", col.lines = "#b8c5de",
               pch = 16, xlab = "Kwantyle rozkładu normalnego", 
               ylab = paste("Kwantyle rozkładu: ", ss), 
               main = paste("Wykres Q-Q dla: ", ss)
        )
        
    }
  })    
  
  output$xy <- renderPlot({                         
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else {
      dataset <- get(input$baza_p)
    }
    ii <- input$nr_kolumny_x                                     # numer kolumny
    x    <- na.omit(dataset[,ii])                              # kolumna w wybranej bazie
    ssx <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    jj <- input$nr_kolumny_y                                     # numer kolumny
    y    <- na.omit(dataset[,jj])                              # kolumna w wybranej bazie
    ssy <- colnames(dataset)[jj]                                # nazwa tej kolumny  
    dane <- data.frame(x,y)
    ggplot(dane, aes(x,y)) +
      geom_point(colour = "#6f8dbf") +
      geom_smooth(method = "lm", color = "#6f8dbf", fill = "#b8c5de") +
      labs(
        title = paste("Wykres zależności:   ", ssy, "=f(",ssx,")"),
        x = ssx,
        y = ssy
      )+
      theme(plot.title = element_text(hjust = 0.5))
    
  })      
  
  output$kor <- renderPlot({                         
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else {
      dataset <- get(input$baza_p)
    }
    
    # licznik faktorów
    dfff <- dataset
    licznik <- 0                     
    wek <- rep(0, ncol(dataset))
    for (i in 1:ncol(dataset)){
      if(class(dataset[,i]) != "numeric" & class(dataset[,i]) != "integer"){
        licznik <- licznik + 1
        wek[licznik] <- i
      }
    }
   
    if (licznik != 0){
      dfff <- dfff[-wek]
    }
    
    correlation <- cor(na.omit(dfff))
    corrplot(correlation, order = "AOE", type = "upper",tl.pos = "td")
    
  })     
  
  
}
