shiny::tabPanel(
  title = "Configurações",
  
  # fluidRow(
  #   column(width = 4,
  #          
  #          # Copy the line below to make a slider bar 
  #          sliderInput("sldRaioVizinhanca", label = "Raio da vizinhança", min = 1, 
  #                      max = 4, value = 1)
  #   )
  # ),
  # 
  hr(),
  
  fluidRow(
    column(width = 12,
           multiInput(
             inputId = "multiSelectNodesPJ",
             label = "Empresas:",
             choices = NULL,
             choiceNames = "",
             choiceValues = "",
             width = '100%'
           )
    ),
    column(width = 12, align="center",
           actionButton("btnIncluiTodosPJ", label = "Selecionar/Excluir Todos")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(width = 12,
           multiInput(
             inputId = "multiSelectNodesPF",
             label = "Pessoas Físicas:",
             choices = NULL,
             choiceNames = "",
             choiceValues = "",
             width = '100%'
           )
           
    ),
    column(width = 12, align="center",
           actionButton("btnIncluiTodosPF", label = "Selecionar/Excluir Todos")
    )
  ),
  #fluidRow(
    #tableOutput("all_nodes")
  #),
  
  hr()
)