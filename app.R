

library(plyr)
library(shiny)
library(stringr)
library(mgsub)
library(tidyverse)
library(quanteda)
library(tidytext)
library(grDevices)
library(quanteda)
library(DT)
library(ggwordcloud)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(rtweet)

#setwd("/home/ricardo/Documents/UNESCO/apps/version_2/sexdictionary4_06/")

source("funciones2.r")
#### FUNCIONES ####

#readRDS("indice.rds") -> indice

list.files("BBDD/","rds") -> lista_bs
str_split(lista_bs, pattern = "_", simplify = TRUE, n = 2) -> b_bs; b_bs
gsub(".rds|AND|OR|_OK|_tidy","", b_bs) -> b_bs; b_bs
b_bs[,2] -> conc1


#lista_bs[grepl("sexo", lista_bs)][1]

#corona = readRDS("BBDD/200401084210_coronavirus_tidy.rds")
#
#table(corona$n_SEX_tweet)

#em(sample(corona$text[!duplicated(corona$text) & !is.na(corona$text)],1))



#corona %>% 
#filter(concepto1 == "coronavirus") %>% 
#distinct(id_tweet) %>% nrow()

#length(unique(corona$text))
#length(unique(corona$id_tweet))
#table(duplicated(corona$text))
#table(duplicated(corona$text[!duplicated(corona$id_tweet)]))


####################################################### solo permite filtrar una base osino los filtro dinámicos se confunden, se puede trabbajar con solo una base, pero es necesario 
############################## generar buenos IDs
#tidy_tweets = readRDS("tidy_tweets_FIN")
#tweets = readRDS("tweets_FIN")
#
#unique(tidy_tweets$concepto1) -> conc1
#unique(tidy_tweets$concepto2) -> conc2
#unique(tidy_tweets$concepto3) -> conc3
#
#unique(conc1)[!is.na(unique(conc1))] -> conc1
#unique(conc2)[!is.na(unique(conc2))] -> conc2
#unique(conc3)[!is.na(unique(conc3))] -> conc3
#
#conceptos = c(conc1, conc2, conc3)
#fix_text(conceptos) -> conceptos

#### UI ####
ui <- fluidPage(headerPanel("SexDictionary Plataforma de análisis"),
  
  theme = shinytheme("spacelab"),

    # Application title
    titlePanel(h2("  Plataforma de búsqueda y análisis de conceptos en Twitter")),
h4("   Esta plataforma es la sintesis de la investigación realizada para UNESCO durante 2019 -2020  sobre temáticas de sexualidad en redes sociales"),
hr(),
#h4("Instrucciones:"), 
#h4("Esta versión de la plataforma permite trabajar con dos fuentes de datos:"),
#h4("1) Realizar busquedas diarias de nuevos conceptos, todas estas búsquedas son guardadas para su posterior análisis, existe un límite díario de 17.000 tweets"),
#   h4("2) Trabajar con bases de datos de conceptos buscados previamente"),
#h4("*estas opciones son excluyentes"),
br(),
    # Sidebar with a slider input for number of bins 
    fluidRow(
      #### sidebar ####
        sidebarPanel(width = 4,
                     radioGroupButtons(
                       inputId = "Id004",
                       label = h4("Opciones de trabajo"), 
                       choices = c("Búsqueda ONLINE", "Conceptos Almacenados"),
                       status = "primary",
                       justified = TRUE
                     ), 
                     ###### buscador de conpectos 
                     uiOutput("busquedaUI"),
    #  textInput("busquedaIMP","Ingrese el o los conceptos que desea buscar, si es más de uno utilice el conector AND, ej: sexo AND casual", value = ""),
    #  numericInput("busquedaN","Ingrese en números cantidad de tweets, máximo 16.000", value = ""),
    #  actionButton("actbuscar","Buscar!"),
    #  br(),
    #  br(),
    #        tags$head(
    #      tags$style(HTML("hr {border-top: 1px solid #000000;}"))),  
    #### conceptos descargados 
    uiOutput("cargaUI"),
  #   h4("2. Trabajo con conceptos ya descargados"),
  #   h5("Explora los conceptos que ya hemos descargado, cuando selecciones uno, pulsa el botón \"Cargar Tweets\""),
  #   selectInput("concepto1", label = h4("1er Concepto buscado"),
  #                choices = c("", conc1), selected = 1 ),
  #   actionButton("actcargar","Cargar Tweets!"), br(),br(),
  #   uiOutput("slideSEXDIC"),
  #   uiOutput("slideEMPOS"),
  #   uiOutput("slideEMNEG"),
     # checkboxInput("concepto2_si",h6("¿Desea ver otros conceptos utilizados al buscar el 1er concepto?")),
  uiOutput("concepto2UI")
#  ,  uiOutput("concepto3UI")
     ),
      ###### selección para trabajar con bases de datos existentes
     

        # Show a plot of the generated distribution
#### mainPanel ####
        mainPanel(

          #tabsetPanel(     tabPanel("Análisis Frecuencias",
                     column(12,
          h3("Frecuencia de tweets y de tokens en SexDictionary y emociones positivas y/o negativas "),
          "Este porcentaje nos permite comprender cuantas tokens relacionadas con sexualidad tienen los tweets del concepto que estamos analizando",
          hr()),
          #### print prueba UI ####
          verbatimTextOutput("fe2"),verbatimTextOutput("fe3"),
          fluidRow(
            column(3, h4("Cantidad de tweets obtenidos:"),align="center",
                   h2(textOutput("cantidad"))),
            column(3, h4("% tokens de sexualidad:"),align="center",
                   h2(textOutput("porcentaje")) ),
            column(3, h4("% tokens de Positivas:"),align="center",
                   h2(textOutput("porcentajePOS")) ),
            column(3, h4("% tokens de Negativas:"),align="center",
                   h2(textOutput("porcentajeNEG")) )),
  fluidRow(
    column(12,hr()),
    column(12,  uiOutput("nube"), hr(),), 
    column(2, actionButton("asktabla", "Tabla de frecuencias")),
    column(12,uiOutput("tabla")),
  fluidRow(column(12, align="center", uiOutput("texto_tweets", ))),

  
    br(),
    br(),
    hr(),
                 )
  #) ,  tabPanel("Análisis Sentimientos"     )  )
            )
        )
    )



server <- function(input, output, session) {
  ##### 1 switch ui - Búsqueda o Carga ####
  
 output$busquedaUI = renderUI({
    req(input$Id004 == "Búsqueda ONLINE")
    tagList(
      h4("Bucador de conceptos en twitter"),
      h5("Realiza busquedas de nuevos conceptos para saber que se está hablando de ellos y como se relacionan con sexualidad"),
      br(),
      textInput("busquedaIMP","Ingrese el o los conceptos que desea buscar, si es más de uno utilice el conector AND, ej: sexo AND casual", value = ""),
      numericInput("busquedaN","Ingrese en números cantidad de tweets, máximo 16.000", value = ""),
      actionButton("actbuscar","Buscar Tweets!"),
      br(),
      br(),
      uiOutput("slideSEXDIC_b"),
      uiOutput("slideEMPOS_b"),
      uiOutput("slideEMNEG_b"),
            tags$head(
          tags$style(HTML("hr {border-top: 1px solid #000000;}"))
            )
    )
  })
 
 output$cargaUI = renderUI({
   req(input$Id004 == "Conceptos Almacenados")
   
   list.files("BBDD/","rds") -> lista_bs
   str_split(lista_bs, pattern = "_", simplify = TRUE, n = 2) -> b_bs; b_bs
   gsub(".rds|AND|OR|_OK|_tidy","", b_bs) -> b_bs; b_bs
   b_bs[,2] -> conc2
   
  
   tagList(
   h4("Trabajo con conceptos ya descargados"),
   h5("Explora los conceptos que ya hemos descargado, cuando selecciones uno, pulsa el botón \"Cargar Tweets\""),
   selectInput("concepto1", label = h4("1er Concepto buscado"),
                choices = c("", conc2), selected = 1 ),
   actionButton("actcargar","Cargar Tweets!"), br(),br(),
   uiOutput("slideSEXDIC"),
   uiOutput("slideEMPOS"),
   uiOutput("slideEMNEG")
     )
 })
 


  ############################# busquedas
  
  #### 2 BUSC. tweets ####

 tidy_tweetsb <- eventReactive(input$actbuscar,{ 
  
   busqueda_tweets_free(query = input$busquedaIMP, cantidad = input$busquedaN)
     
   })
  
 #  ##### 2.1 slideB SEX DIC ####
 
 maxSLIDb <- reactive({
   #req(input$busquedaIMP)
   tidy_tweetsb() %>% 
    # filter(concepto1 == input$concepto1) %>% 
  #   filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE) %>% 
     select(n_SEX_tweet) %>% max()
 })
 
 output$slideSEXDIC_b = renderUI({
   #req(input$busquedaIMP)
   sliderInput("prop_sexB", "Puntaje de SexDictionary en el Tweets", min = 0, max = maxSLIDb() , value = c(0, maxSLIDb()))
 })  
 
 ##### 2.2 slideB Emocion Positiva ####
 
 maxPOSb <- reactive({
   #req(input$busquedaIMP)
   tidy_tweetsb() %>% 
   #  filter(concepto1 == input$concepto1) %>% 
   #  filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE) %>% 
     select(t_pos) %>% max()
 })
 
 output$slideEMPOS_b = renderUI({
   #req(input$busquedaIMP)
   sliderInput("prop_emoPOSB", "Puntaje de emociones positivas en el Tweets", min = 0, max = maxPOSb() , value = c(0, maxPOSb()))
 })  
 
 
 #  ##### 2.3 slideB Emocion negativa ####
 
 maxNEGb <- reactive({
   #req(input$busquedaIMP)
   tidy_tweetsb() %>% 
    # filter(concepto1 == input$concepto1) %>% 
    # filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE) %>% 
     select(t_neg) %>% max()
 })
 
 output$slideEMNEG_b = renderUI({
   #req(input$busquedaIMP)
   sliderInput("prop_emoNEGB", "Puntaje de emociones negativas en el Tweets", min = 0, max = maxNEGb() , value = c(0, maxNEGb()))
 })  
 
 ##### 2.4 filtramos tweets ####
  tidyB =  reactive({
   #req(input$busquedaIMP)
   tidy_tweetsb() %>% 
     filter(!word %in% input$busquedaIMP) %>% 
     filter(if(!is.null(input$prop_sexB)) (n_SEX_tweet >= input$prop_sexB[1] & n_SEX_tweet <= input$prop_sexB[2]) else TRUE) %>%
     filter(if(!is.null(input$prop_emoPOSB)) (t_pos >= input$prop_emoPOSB[1] & t_pos <= input$prop_emoPOSB[2]) else TRUE) %>%
     filter(if(!is.null(input$prop_emoNEGB)) (t_neg >= input$prop_emoNEGB[1] & t_neg <= input$prop_emoNEGB[2]) else TRUE) 
 })
 
 
 
#  ####print prueba ####
#     output$fe2 = renderPrint({
#       input$prop_emoNEGB
#     })

  
  
  #### fin BUSC. tweets ####
  
  
  
  #### 3 CARG. tweets ####
    #### f. leer tweets seleccionados - uniendo bases con palabras similares ####
      readFILE <- function(v){
        nuevos <- list()
        for(i in seq(v)){
          nuevos[[i]] <- readRDS(paste0("BBDD/",v[i]))
        }
        
        df <- bind_rows(nuevos) 
        df <- df %>% distinct(id_tweet, .keep_all = T)
        
        return(df)
      }
   
    #### 3 carga ####
        tidy_tweets <- eventReactive(input$actcargar,{
          
          dat <- data.frame(x = numeric(0), y = numeric(0))
          withProgress(message = 'Cargando Tweets', value = 0, {
            # Number of times we'll go through the loop
            n <- 1000
            for (i in 1:n) {
              # Each time through the loop, add another row of data. This is
              # a stand-in for a long-running computation.
              dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
              
              # Increment the progress bar, and update the detail text.
              incProgress(1/n, detail = paste("Queda poco", i))
              
              # Pause for 0.1 seconds to simulate a long computation.
           #   Sys.sleep(0.15)
       
              }
          })
          
        
          
          lista_bs[grep(input$concepto1, lista_bs)] -> v
          
          # filename <- paste0("BBDD/",indice$nombre_archivo[indice$concepto1 == input$concepto1])
          #filename <- paste0("BBDD/",lista_bs[grepl(input$concepto1, lista_bs)][1])
          readFILE(v)
                })
   
       ##### carg. inputs que filtran tweets ####
      #  
      #  ##### 3.1 arg. concepto 2 se actualiza  ####
          x = reactive({
            x = tidy_tweets()$concepto2[tidy_tweets()$concepto1 == input$concepto1]
            x = as.character(unique(x))
            x[!is.na(x)] -> x 
            })
        
         output$concepto2UI <- renderUI({
           req(x())
           selectInput(inputId = "concepto2", h4("2do concepto de busqueda"),choices = x() , multiple = T)
         })
        
      #  ##### 3.2 carg. slide SEX DIC ####
        
          maxSLID <- reactive({
              req(input$actcargar)
             tidy_tweets() %>% 
              filter(concepto1 == input$concepto1) %>% 
              filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE) %>% 
                       select(n_SEX_tweet) %>% max()
          })
            
          output$slideSEXDIC = renderUI({
             req(input$actcargar)
            sliderInput("prop_sex", "Puntaje de SexDictionary en el Tweets", min = 0, max = maxSLID() , value = c(0, maxSLID()))
         })  
         
      #  ##### 3.3 carg. slide Emocion Positiva ####
         
         maxPOS <- reactive({
           req(input$actcargar)
           tidy_tweets() %>% 
             filter(concepto1 == input$concepto1) %>% 
             filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE) %>% 
             select(t_pos) %>% max()
         })
     
         output$slideEMPOS = renderUI({
           req(input$actcargar)
             sliderInput("prop_emoPOS", "Puntaje de emociones positivas en el Tweets", min = 0, max = maxPOS() , value = c(0, maxPOS()))
         })  
         
         
      #  ##### 3.4 carg. slide Emocion negativa ####
         
         maxNEG <- reactive({
           req(input$actcargar)
           tidy_tweets() %>% 
             filter(concepto1 == input$concepto1) %>% 
             filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE) %>% 
             select(t_neg) %>% max()
         })
         
         output$slideEMNEG = renderUI({
           req(input$actcargar)
           sliderInput("prop_emoNEG", "Puntaje de emociones negativas en el Tweets", min = 0, max = maxNEG() , value = c(0, maxNEG()))
         })  
         
        
      #  ##### 3.5 carg.  filtramos tweets ####
         
          tidyC =  reactive({
             req(input$concepto1)
             tidy_tweets() %>% 
             filter(concepto1 == input$concepto1) %>% 
             filter(if(!is.null(input$prop_sex)) (n_SEX_tweet >= input$prop_sex[1] & n_SEX_tweet <= input$prop_sex[2]) else TRUE) %>%
             filter(if(!is.null(input$prop_emoPOS)) (t_pos >= input$prop_emoPOS[1] & t_pos <= input$prop_emoPOS[2]) else TRUE) %>%
               filter(if(!is.null(input$prop_emoNEG)) (t_neg >= input$prop_emoNEG[1] & t_neg <= input$prop_emoNEG[2]) else TRUE) %>%
             filter(if(!is.null(input$concepto2)) (concepto2 %in% input$concepto2) else TRUE)
               
              })
         

  #  ######### fin CARG. de tweets ####
  #

 #### SWITCH busq/carg ##### 
 #https://stackoverflow.com/questions/56072384/shiny-app-with-multiple-input-modules-that-creates-the-same-output
 
 tidy <- reactiveVal(NULL)
 
 observeEvent(tidyC(), {
   tidy(tidyC())
 })
 
  observeEvent(tidyB(), {
    tidy(tidyB())
  })
  
#  ### print prueba ####
#  output$fe3 = renderPrint({
#   # req(input$prop_sex)
#    tidy()
#    })
 
 
 
 
 ####### 4 OUTPUTs ####
 
 #  

##  #### 4.1 generamos tablas de frencuencias
   
  t_freq = reactive({
     tidy() %>% 
      dplyr::count(word, sort = T)
   })
# 
#
 #  ### 4.2 cantidad de tweets  ####
  #  
      output$cantidad =  renderText({ 
        if(is.null(tidy())){return(NULL)}else{
            tidy() %>%  distinct(id_tweet) %>% nrow() -> cantidad_tweets 
        }
                    })
  #  
    
  #  ##### 4.3 indicadores  ####

         porcentaje = reactive(
           if(!is.null(tidy())){
             paste0(round(as.data.frame(prop.table(table(tidy()$e_sexdic)))[2,2]*100,1),"%")
           }
         )
         
         porcentajePOS = reactive({
           if(!is.null(tidy())){
             paste0(round(as.data.frame(prop.table(table(tidy()$Positive)))[2,2]*100,1),"%")
           }
         })
         
         porcentajeNEG = reactive({
           if(!is.null(tidy())){
             paste0(round(as.data.frame(prop.table(table(tidy()$Negative)))[2,2]*100,1),"%")
           }
         })
  #  
         
    output$porcentaje = renderText(
    porcentaje()
        )
    
    output$porcentajePOS = renderText({
 porcentajePOS()
         })
    
    output$porcentajeNEG = renderText({
porcentajeNEG()
    })
    
   ### 
  ##  output$table1 <- renderTable({ 
  ##  tidy1()
  ##    })
  #  
    output$table <- DT::renderDataTable({
      req(tidy())
          DT::datatable(t_freq() , options = list(scrollX = T),  rownames = FALSE)
    })

    
  ######## 4.4 tabla de frecuencias  ####
    observeEvent(input$asktabla,{
    output$tabla = renderUI({
     dataTableOutput("table")
     })
    })
  #  
  #  ##### 4.5 gráfico de palabras ####
    ###  tabla gráfico ####
    tidy1 = reactive({
      req(tidy())
      tidy() %>% 
        dplyr::count(word, sort = T) %>% 
        filter(row_number() <= 250,
               !word %in% conc1) %>% 
       # if(!is.null(concepto1)) (!word %in% conc1) else TRUE) %>% 
        mutate(angle = (90 * sample(c(0, 1),length(word),  replace = TRUE, prob = c(40, 60))))
    })
    
    
    colors = rainbow(150, start =  0, end = 0.25, s = 1, v = 0.8)
  
    output$plot1 <- renderPlot({
     # if(!is.null(tidyB()) | !is.null(tidyC()))
      req(tidy())
      set.seed(42)
      ggplot(tidy1(), aes(label = word, size = n, angle = angle, color = factor(sample.int(150, nrow(tidy1()), replace = TRUE)) )) +
        geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = .35, shape = "square") +
        scale_size_area(max_size = 80) +
        theme_minimal() 
        
     })
    
    
     output$nube = renderUI({
       tagList(
       h2("Palabras más frecuentes"),
        plotOutput("plot1")
       )
     })
     
    #### 4.6 tweets ejemplos ####
    output$texto_tweets <- renderUI({
      req(tidy())
      tagList(
        br(),
        hr(),
        h3("Tweets de ejemplo"),
      h4(renderText({sample(tidy()$text[!duplicated(tidy()$text) & !is.na(tidy()$text)],1)})),
      br(),
      h4(renderText({sample(tidy()$text[!duplicated(tidy()$text) & !is.na(tidy()$text)],1)})),
      br(),
      h4( renderText({sample(tidy()$text[!duplicated(tidy()$text) & !is.na(tidy()$text)],1)})),
      br()
      )
      })
    
    #sample(corona$text[!duplicated(corona$text) & !is.na(corona$text)],1)

    #####################################################################################################################################################
    #####################################################################################################################################################

 # #### concepto 3 está pendiente 
  
##  x2 = reactive({
##    x = tidy_tweets$concepto3[tidy_tweets$concepto1 == input$concepto1 & tidy_tweets$concepto2 %in% input$concepto2]
##    x = as.character(unique(x))
##    x[!is.na(x)] -> x })
##  
##  output$concepto3UI <- renderUI({
##    req(x2())
##    selectInput(inputId = "concepto2", h4("3er concepto de busqueda"),choices = x2() , multiple = T)
##  })
##     

#  

}

# Run the application 
shinyApp(ui = ui, server = server)
#shiny::runApp(display.mode="showcase")


