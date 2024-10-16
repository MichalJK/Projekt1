#Plik serwera do alplikacji "..."


#--------------------Bibioteki---------------------------
require(shiny)
require(shinydashboard)
require(DT)

require(reticulate)     # hub do pythona
require(car)            # qqPlot
require(ggplot2)        # gglplot
require(corrplot)       # corrplot
require(DescTools)      # Kurt(), #Skew()

# ------------------------Funkcja serwera--------------------------

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
  
  source_python("podsumowanie.txt")
  
  wczytaj <- function() {
    dd <- read.csv(input$plik$datapath, header = input$header, sep = input$sep)
    return(dd)
  }
  
  baza <- function(){                                                   # realizuje wybów bazy
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else if (input$wybor == "Python"){
      dataset <- get(input$baza_p)
    } else {
      dataset <- wczytaj()
    }
  }

eliminator <- function(dataset) {                                       #eliminuje wielkości nieilościowe
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
  return(dfff)
}   

rozst <- function(dane){
  n <- ncol(dane)
  rozstep <- rep(0,n)
  for (i in 1:n){
    rozstep[i] <- max(dane[,i]) - min(dane[,i])
  }
  rozstep <-t(rozstep)
  rozstep <- data.frame(rozstep)
  colnames(rozstep) <- colnames(dane)
  return(rozstep)
}
# ------------------------  
  
# dataset - aktualnie analizowana baza
  
# ------------------------  
    
  output$tabela <- renderDT({           #tabela
    dataset <- baza()
  }, options = list(pageLength = 13,  scrollX = T )
  )         

# -----------------------
notka <- function(dataset,ii){                                      # wywołuje komunikat w przypadku powstania błędu
  # ii - numer kolumny  
  nn <- ncol(dataset)                      # liczba kolumn bazy dataset
  if ( ii > nn || class(dataset[,ii]) == "factor" || class(dataset[,ii ]) == "character"){
    showNotification(paste("Baza nie zawiera liczbowej kolumny nr ",
                           as.character(ii)), type = "error", duration = 3)
  }   
}


#---------------------------------
  output$podsumowanie <- renderPrint(                      
    width =800,
    {
      dataset <- baza()
    if (input$wybor == "R"){
      cat(paste("Baza: ", input$wybor,": ", input$baza_r,"\n\n"))
      print(summary(dataset))
    } else if (input$wybor == "Python"){
      cat(paste("Baza: ", input$wybor,": ", input$baza_p,"\n\n"))
      print(podsumowanie(dataset))
    } else {
      cat(paste("Baza: ", input$wybor, ": ", input$plik$name, "\n\n"))
      print(summary(dataset))
    }
    
  })

  output$info <- renderPrint(                                            #statystyka opisowa
    width = 800,
    {                      
      dataset <- baza()
      
      cat("liczba wierszy =",as.character(nrow(dataset)))
      cat("\nliczba kolumn =",as.character(ncol(dataset)),"\n\n")
      cat("Typy zmiennych:\n")
      print(sapply(dataset, class))
      dfff <- eliminator(dataset)
      dfff <- na.omit(dfff)
      cat("\nDodatkowe informacje statystyczne:\n")
      odch <- sapply(dfff,sd)
      suma <- sapply(dfff,sum)
      rozstep <-rozst(dfff)
      kurtoza <- sapply(dfff, Kurt)
      skosnosc <- sapply(dfff, Skew)
      
      informacja <- data.frame(NA,1:ncol(dfff),ncol = ncol(dfff), nrow = 5)
      informacja <- rbind(odch, rozstep,kurtoza, skosnosc, suma)
      row.names(informacja) <- c("sd:","rozstęp:", "kurtoza:", "skośność:", "suma:")
      print(informacja)
    })

  
  # ---------------------
  
  output$histogram <- renderPlot({                         
    
    dataset <- baza()
    
    ii <- input$nr_kolumny   # numer kolumny
    
    notka(dataset, ii)
    
    x    <- na.omit(dataset[,ii])                                       # kolumna w wybranej bazie
    ss <- colnames(dataset)[ii]                                         # nazwa tej kolumny  
    bins <- seq(min(x), max(x), length.out = input$slupki+ 1)           # wektor punktów pomiędzy słupkami histogramu
                                                                        # length.out	- pożądana liczba słupków
    hist(x, breaks = bins, col = '#6f8dbf', border = 'white',
         xlab = ss,
         main = paste('Histogram: ',ss)
    )
  })    

  output$qq <- renderPlot({                                             #wykres kwantylowy z wyborem kolumn                   
    
    dataset <- baza()
    
    ii <- input$nr_kolumny_1                                            # numer kolumny
    
    notka(dataset, ii)
    
    x    <- na.omit(dataset[,ii])                                       # kolumna w wybranej bazie
    ss <- colnames(dataset)[ii]                                         # nazwa tej kolumny  
    
  
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
  
  output$xy <- renderPlot({                                       # wykres punktowy
    dataset <- baza()
    
    ii <- input$nr_kolumny_x                                      # numer kolumny
    
    notka(dataset, ii)
    
    x    <- dataset[,ii]                                          # kolumna w wybranej bazie
    ssx <- colnames(dataset)[ii]                                  # nazwa tej kolumny  
    jj <- input$nr_kolumny_y                                      # numer kolumny
    
    notka(dataset, jj)
    
    y    <- dataset[,jj]                                          # kolumna w wybranej bazie
    ssy <- colnames(dataset)[jj]                                  # nazwa tej kolumny  
    dane <- data.frame(x,y)
    ggplot(dane, aes(x,y)) +
      geom_point(colour = "#6f8dbf") +
      geom_smooth(color = "#6f8dbf", fill = "#b8c5de") +
      labs(
        title = paste("Wykres zależności:   ", ssy, "=f(",ssx,")"),
        x = ssx,
        y = ssy
      )+
      theme(plot.title = element_text(hjust = 0.5))
    
  })      
  
  output$kor <- renderPlot({                                      #tabela korelacji zmiennych iościowych                     
    dataset <- baza()
    
    # licznik faktorów
    dfff <- eliminator(dataset)
    
    
    correlation <- cor(na.omit(dfff))
    corrplot(correlation, order = "AOE", type = "upper",tl.pos = "td", tl.col = "#465878")
    
  })     
  
  
}
