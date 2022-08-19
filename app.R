## app.R ##
library(shinydashboard)
library(readxl)

source("utils.R")


ui <- dashboardPage(skin = "blue",
  
  dashboardHeader(title = "Plano de Desenvolvimento Institucional - UFERSA",
                  titleWidth = 500),
  
  dashboardSidebar(id="", width = 350,
                   sidebarMenu(
                     menuItem(h5("Acompanhamento"), 
                              tabName = "dashboard", 
                              icon = icon("th-large", lib = "glyphicon")),
                     
                     menuItem(h5("Sobre a Ufersa"), 
                              tabName = "about_ufersa",
                              icon = icon("book", lib = "glyphicon")),
                     
                     menuItem(h5("Sobre o PDI"), 
                              tabName = "about_pdi",
                              icon = icon("envelope", lib = "glyphicon"))
                   )),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    
    tabItems(
      
      tabItem(tabName = "dashboard",
              
              
              # Boxes need to be put in a row (or column)
              fluidRow(
                column(width = 12,
                       
                       div(
                         box(
                           width = 12,
                           title = "Selecione a Meta do PDI",
                           status = "warning",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           selectInput("choice1","Metas", choices = lista)
                         )
                       ),
                       
                       div(
                         box(
                           width = 12,
                           title = "Acompanhamento da Meta",
                           status = "warning",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("plot1")
                         ), style = "text-align: center;"
                       )
                       
                )
                
                
                
              ),
              
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("perspectiva"),
                infoBoxOutput("objetivo"),
                infoBoxOutput("status")
              )
              
      ), 
      
      tabItem(tabName = "about_pdi", h2("Sobre o PDI", align="center")),
      
      tabItem(tabName = "about_ufersa", 
  
              div(tags$img(src="imagename.png", height = "100px", width="220px", 
                       alt="Something went wrong", align="center",
                       deleteFile = FALSE), style = "text-align: center;"),
              
              div(box(
                width = NULL,
                title = "Saiba mais sobre a Universidade Federal Rural do Semi-Árido",
                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/ScMzIvxBSi4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
              ), style = "text-align: center;")
      )
      
      
    )
    
    
)
)

server <- function(input, output) {
  
  # recebe a meta
  meta <- reactive({test = input$choice1 %>% filtra_meta()
                   print(test)
                   test})
  
  
  # grafico
  output$plot1 <- renderPlot({
    plot_meta(meta())
  })
  
  # dynamic infobox perspectiva
  output$perspectiva <- renderInfoBox({
    infoBox(
      "Perspectiva",  input$choice1 %>% filtra_perspectiva, 
      icon = icon("tag", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  # dynamic infobox objetivo
  output$objetivo <- renderInfoBox({
    infoBox(
      "Objetivo",  input$choice1 %>% filtra_objetivo, 
      icon = icon("screenshot", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  
  reactive({input$choice1 %>% filtra_status}) -> status_meta
  
  
    
    # dynamic infobox objetivo
    output$status <- renderInfoBox({
      
      if(status_meta() == "Atingida"){
       cor <- "green" 
       icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta() == "Parcialmente Atingida"){
        cor <- "yellow"
        icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta() == "Não Atingida"){
        cor <- "red"
        icone <- icon("thumbs-down", lib = "glyphicon")
      }
      
      
      infoBox(
        "Status atual",  status_meta(), 
        icon = icone,
        color = cor
      )
    })
    
  
  
  
  
}
  
  


shinyApp(ui, server)