
# Plik servera do aplikacji "Analiza wstępna bazy danych"

# ----------------------- biblioteki -------------------------------------------

require(shiny)
require(shinydashboard)
require(DT)

library(shiny.i18n)

require(reticulate)        # hub do Pythona
require(ggplot2)           # ggplot()
require(corrplot)          # corrplot() 
require(moments)           # skewness(), kurtosis()

# ------------------------ funkcja servera -------------------------------------

function(input, output, session) {
  
  i18n <- Translator$new(translation_csvs_path = "./data/", separator_csv = ",")

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
  
  source_python("podsumowanie.txt")         # plik zewnetrzny z funkcja Pythona .describe()

# -----------------------
  
  wczytaj <- function(){                     # wczytuje bazę z pliku .txt i .csv
     dd <- read.csv(input$plik$datapath, header = input$header, sep = input$sep)
     return(dd)
  }
  
# -----------------------
  
  baza <- function(){                        # realizuje wybór bazy  
    if (input$wybor == "R"){
      dataset <- get(input$baza_r)
    } else if (input$wybor == "Python"){
      dataset <- get(input$baza_p)
    } else {
      dataset <- wczytaj()
    }
  }
  
# -----------------------  
  
  eliminator <- function(dataset){           # eliminuje wielkości nieilościowe
  
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

      #
      if (licznik != 0){
        dfff <- dfff[-wek]
      }
      
      return(dfff)
      
  } 

# --------------------
  
  notka <- function(dataset,ii){             # wywołuje komunikat w przypadku powstania błędu
                                             # ii - numer kolumny  
    
    nn <- ncol(dataset)                      # liczba kolumn bazy dataset
    
    if ( ii > nn || class(dataset[,ii]) == "factor" || class(dataset[,ii ]) == "character"){
      showNotification(paste(i18n$t("Baza nie zawiera liczbowej kolumny nr "),
                             as.character(ii)), type = "error", duration = 3)
    }   
    
  }
  
# -----------------------
  
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
  
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
  })
  
  
# ------------------------  
    
  output$tabela <- renderDT({   # ładna tabela
 
    dataset <- baza()
  }, options = list(pageLength = 13,  scrollX = T )
  )         

# -----------------------
  
  output$podsumowanie <- renderPrint(  # najważniejsze wielkości statystyczne dotyczące bazy
    width = 800,
    { 
      dataset <- baza()      

    if (input$wybor == "R"){
      cat(paste(i18n$t("Baza"),": ", input$wybor,": ",input$baza_r,"\n\n") )
      print(summary(dataset))
    } else if (input$wybor == "Python"){
      cat(paste(i18n$t("Baza"),": ", input$wybor,": ",input$baza_p,"\n\n") )
      summary <- podsumowanie(dataset)             # funkcja Pythona
      print(summary)
    } else {
      cat(paste(i18n$t("Baza"),": ", input$wybor,": ",input$plik$name,"\n\n") ) 
      print(summary(dataset))      
    }
  })
  

  
# -----------------------  

  output$info <- renderPrint(     # typ zmiennych bazy i odchylenie standardowe
    width = 800,
    {                      
      dataset <- baza() 

      cat(i18n$t("liczba wierszy ="),as.character(nrow(dataset)), "\n")
      cat(i18n$t("liczba kolumn ="),as.character(ncol(dataset)),"\n\n")
      cat(i18n$t("Typy zmiennych:"),"\n")
      print(sapply(dataset, class))
      
      dfff <- eliminator(dataset)
      dfff <- na.omit(dfff)
      
      cat("\n",i18n$t("Dodatkowe informacje statystyczne"),":\n")
      
      odch <- sapply(dfff,sd)
      suma <- sapply(dfff,sum)
      rozstep <-rozst(dfff)
      kurtoza <- sapply(dfff, kurtosis)
      skosnosc <- sapply(dfff, skewness)

      informacja <- data.frame(NA,1:ncol(dfff),ncol = ncol(dfff), nrow = 5)
      
      informacja <- rbind(odch, rozstep,kurtoza, skosnosc, suma)
      c1 <- "sd:"
      c2 <- i18n$t("rozstęp:")
      c3 <- i18n$t("kurtoza:")
      c4 <- i18n$t("skośność:")
      c5 <- i18n$t("suma:")
      row.names(informacja) <- c(c1,c2,c3,c4,c5)
     
      print(informacja)
      
    })
  # ---------------------
  
   output$histogram <- renderPlot({                         
    
     dataset <- baza() 
     
    ii <- input$nr_kolumny                                     # numer kolumny
    
    notka(dataset,ii)                                          # komunikat

    x    <- na.omit(dataset[,ii])                              # kolumna w wybranej bazie
    ss <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    bins <- seq(min(x), max(x), length.out = input$slupki+ 1)  # wektor punktów pomiędzy słupkami histogramu
                                                               # length.out	- pożądana liczba słupków
    hist(x, breaks = bins, col = '#6f8dbf', border = 'white',
         xlab = ss,
         main = paste('Histogram: ',ss)
    )
    
    
  })    

# ----------------------------------------------------------------------------------------------  
  output$qq <- renderPlot({        # wykres kwantylowy z wyborem kolumn                  
    
    dataset <- baza() 
    
    ii <- input$nr_kolumny_1                                   # numer kolumny
    
    notka(dataset, ii)                                         # komunikat
    
    x  <- na.omit(dataset[,ii])                                # kolumna w wybranej bazie (pomija NAsy)
    ss <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    
    if(class(x) == "numeric" || class(x) == "integer"){
      
      srednia <- mean(x)
      odch <- sd(x)
      
      # standaryzacja rozkładu
      
      xx <- pnorm(x, mean = srednia, sd = odch)            # dystrybuanta (prawdopodobieństwo skumulowane) rozkłądu       
      xx <- qnorm(xx) 

      xx <- sort(xx)                                       # kwantyle rozkładu x
      
      n <- length(x)
      yy <- rep(0,n)

      pom = 0.5^(1/n)                                      # dystrybuanta rozkładu normalnego
      for (i in 1:n){
        if (i != 1 & i != n){
          yy[i] = (i - 0.3175) / (n + 0.365)
        } else if ( i == 1){
          yy[i] = 1 - pom
        } else if (i == n) {
          yy[i] = pom
        }
      }
      
      yy <- qnorm(yy)                                     # kwantyle rozkładu normalnego
      
      dane <- data.frame(yy,xx)
     
      ggplot(dane, aes(yy,xx)) +
        geom_point(colour = "#6f8dbf", size = 0.7) +
   #     geom_smooth(method = "lm",color = "#6f8dbf", fill = "#b8c5de") +
        geom_abline(intercept = 0, slope = 1, color="#6f8dbf", size=0.5)+
        labs(
          title = paste(i18n$t("Wykres Q-Q dla: "), ss),
           x = i18n$t("Kwantyle rozkładu normalnego"),
           y = paste(i18n$t("Kwantyle rozkładu: "),ss)
          ) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text = element_text(size = 13),
              axis.title = element_text(size = 13),
              plot.title = element_text(size = 15))
      
    }
    
  })     
  
 # -------------------------------------------------------------------------------
  
  output$xy <- renderPlot({      # wykres punktowy z wyborem kolumn                   
    
    dataset <- baza() 
    
    ii  <- input$nr_kolumny_x                                   # numer kolumny
    
    notka(dataset, ii)                                          # komunikat
    
    x   <- dataset[,ii]                                         # kolumna w wybranej bazie
    ssx <- colnames(dataset)[ii]                                # nazwa tej kolumny  
    
    jj  <- input$nr_kolumny_y                                   # numer kolumny
    
    notka(dataset, jj)                                          # komunikat
    
    y   <- dataset[,jj]                                         # kolumna w wybranej bazie
    ssy <- colnames(dataset)[jj]                                # nazwa tej kolumny  
    
    dane <- data.frame(x,y)
    
    ggplot(dane, aes(x,y)) +
      geom_point(colour = "#6f8dbf") +
      geom_smooth(color = "#6f8dbf", fill = "#b8c5de") +
      labs(
          title = paste(ssy,"= f(",ssx,")"),
          x = ssx,
          y = ssy
      ) +
      theme(plot.title = element_text(hjust = 0.5))

  })      
  
# ----------------------------------------------------------------------------
  
  output$kor <- renderPlot({       # tabela korelacji dla zmiennych ilościowych                  
    
    dataset <- baza()  

    dfff <- eliminator(dataset)   
    
    correlation <- cor(na.omit(dfff))
    corrplot(correlation, order = "original", type = "upper", tl.pos = "td", tl.col = "#465878")
 
  })       
  

}
