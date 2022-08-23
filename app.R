## app.R ##
library(shinydashboard)
library(readxl)

source("utils.R")

header <- dashboardHeader(titleWidth = 500)
anchor <- tags$a(href='https://documentos.ufersa.edu.br/planejamentos/pdi/',
                 tags$img(src='imagename.png', height='40', width='90'),
                 'Plano de Desenvolvimento Institucional - UFERSA')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: #E4E5E5 }"))),
  anchor,
  class = 'name')


ui <- dashboardPage(skin = "blue",
  
  header,  
  # dashboardHeader(title = "Plano de Desenvolvimento Institucional - UFERSA",
  #                 titleWidth = 500),
  
  dashboardSidebar(id="", width = 350,
                   sidebarMenu(
                     menuItem(h5("Perspectiva Financeira"), 
                              tabName = "Financeira", 
                              icon = icon("usd", lib = "glyphicon")),
                     
                     menuItem(h5("Perspectiva Sociedade"), 
                              tabName = "Sociedade",
                              icon = icon("user", lib = "glyphicon")),
                     
                     menuItem(h5("Perspectiva Processos Internos"), 
                              tabName = "Processos_internos",
                              icon = icon("transfer", lib = "glyphicon")),
                     
                     menuItem(h5("Perspectiva Aprendizagem e Crescimento"), 
                              tabName = "Aprendizagem_crescimento",
                              icon = icon("education", lib = "glyphicon")),
                     
                     menuItem(h5("Planilha de Metas"), 
                              tabName = "table",
                              icon = icon("th", lib = "glyphicon"))
                   )),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    
    tabItems(
      
      tabItem(tabName = "Financeira",
              
              
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
                           selectInput("choice1","Metas", choices = lista_financeira)
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
      
      tabItem(tabName = "Sociedade",
              
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
                           selectInput("choice2","Metas", choices = lista_sociedade)
                         )
                       ),
                       
                       div(
                         box(
                           width = 12,
                           title = "Acompanhamento da Meta",
                           status = "warning",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("plot2")
                         ), style = "text-align: center;"
                       )
                       
                )
                
                
                
              ),
              
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("perspectiva2"),
                infoBoxOutput("objetivo2"),
                infoBoxOutput("status2")
              )
              
              
              ),
      
      tabItem(tabName = "Processos_internos",
              
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
                           selectInput("choice3","Metas", choices = lista_processos_internos)
                         )
                       ),
                       
                       div(
                         box(
                           width = 12,
                           title = "Acompanhamento da Meta",
                           status = "warning",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("plot3")
                         ), style = "text-align: center;"
                       )
                       
                )
                
                
                
              ),
              
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("perspectiva3"),
                infoBoxOutput("objetivo3"),
                infoBoxOutput("status3")
              )
              
              
              ),
      
      tabItem(tabName = "Aprendizagem_crescimento",
              
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
                           selectInput("choice4","Metas", choices = lista_aprendizagem_crescimento)
                         )
                       ),
                       
                       div(
                         box(
                           width = 12,
                           title = "Acompanhamento da Meta",
                           status = "warning",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("plot4")
                         ), style = "text-align: center;"
                       )
                       
                )
                
                
                
              ),
              
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("perspectiva4"),
                infoBoxOutput("objetivo4"),
                infoBoxOutput("status4")
              )
              
              
              ),
      
      tabItem(tabName = "table", 
  
            # A FAZER
            fluidPage(DTOutput('tbl'))
      )
      
      
    )
    
    
)
)

server <- function(input, output) {
  
  # recebe a meta
  meta <- reactive({test = input$choice1 %>% filtra_meta()
                   print(test)
                   test})
  
  # recebe a meta
  meta2 <- reactive({test = input$choice2 %>% filtra_meta()
                   print(test)
                   test})
  
  # recebe a meta
  meta3 <- reactive({test = input$choice3 %>% filtra_meta()
                   print(test)
                   test})
  # recebe a meta
  meta4 <- reactive({test = input$choice4 %>% filtra_meta()
                   print(test)
                   test})
  
  
  # grafico
  output$plot1 <- renderPlot({
    plot_meta(meta())
  })
  
  # grafico
  output$plot2 <- renderPlot({
    plot_meta(meta2())
  })
  
  # grafico
  output$plot3 <- renderPlot({
    plot_meta(meta3())
  })
  
  # grafico
  output$plot4 <- renderPlot({
    plot_meta(meta4())
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
  
  # dynamic infobox perspectiva
  output$perspectiva2 <- renderInfoBox({
    infoBox(
      "Perspectiva",  input$choice2 %>% filtra_perspectiva, 
      icon = icon("tag", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  # dynamic infobox objetivo
  output$objetivo2 <- renderInfoBox({
    infoBox(
      "Objetivo",  input$choice2 %>% filtra_objetivo, 
      icon = icon("screenshot", lib = "glyphicon"),
      color = "blue"
    )
  })

    # dynamic infobox perspectiva
  output$perspectiva3 <- renderInfoBox({
    infoBox(
      "Perspectiva",  input$choice3 %>% filtra_perspectiva, 
      icon = icon("tag", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  # dynamic infobox objetivo
  output$objetivo3 <- renderInfoBox({
    infoBox(
      "Objetivo",  input$choice3 %>% filtra_objetivo, 
      icon = icon("screenshot", lib = "glyphicon"),
      color = "blue"
    )
  })
  
    # dynamic infobox perspectiva
  output$perspectiva4 <- renderInfoBox({
    infoBox(
      "Perspectiva",  input$choice4 %>% filtra_perspectiva, 
      icon = icon("tag", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  # dynamic infobox objetivo
  output$objetivo4 <- renderInfoBox({
    infoBox(
      "Objetivo",  input$choice4 %>% filtra_objetivo, 
      icon = icon("screenshot", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  
  # tabela geral
  output$tbl <- renderDT(
    metas_para_BI[, -(12:13)] %>% datatable(rownames = FALSE)
  )
  
  
  reactive({input$choice1 %>% filtra_status}) -> status_meta
  reactive({input$choice2 %>% filtra_status}) -> status_meta2
  reactive({input$choice3 %>% filtra_status}) -> status_meta3
  reactive({input$choice4 %>% filtra_status}) -> status_meta4
  
  
    
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
    
    # dynamic infobox objetivo
    output$status2 <- renderInfoBox({
      
      if(status_meta2() == "Atingida"){
       cor <- "green" 
       icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta2() == "Parcialmente Atingida"){
        cor <- "yellow"
        icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta2() == "Não Atingida"){
        cor <- "red"
        icone <- icon("thumbs-down", lib = "glyphicon")
      }
      
      
      infoBox(
        "Status atual",  status_meta2(), 
        icon = icone,
        color = cor
      )
    })
    
    output$status3 <- renderInfoBox({
      
      if(status_meta3() == "Atingida"){
       cor <- "green" 
       icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta3() == "Parcialmente Atingida"){
        cor <- "yellow"
        icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta3() == "Não Atingida"){
        cor <- "red"
        icone <- icon("thumbs-down", lib = "glyphicon")
      }
      
      
      infoBox(
        "Status atual",  status_meta3(), 
        icon = icone,
        color = cor
      )
    })
    
    output$status4 <- renderInfoBox({
      
      if(status_meta4() == "Atingida"){
       cor <- "green" 
       icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta4() == "Parcialmente Atingida"){
        cor <- "yellow"
        icone <- icon("thumbs-up", lib = "glyphicon")
      }
      
      if(status_meta4() == "Não Atingida"){
        cor <- "red"
        icone <- icon("thumbs-down", lib = "glyphicon")
      }
      
      
      infoBox(
        "Status atual",  status_meta4(), 
        icon = icone,
        color = cor
      )
    })
    
  
  
  
  
}
  
  


shinyApp(ui, server)