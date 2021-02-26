require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(visNetwork)
require(shinybusy)

# shiny::shinyUI(
#   shiny::navbarPage(
#     "Consulta Nice",
#     source("./src/ui/proxy_viz_ui.R", local = TRUE, encoding = 'UTF-8')$value,
#     source("./src/ui/proxy_data_source_ui.R", local = TRUE, encoding = 'UTF-8')$value,
#     source("./src/ui/proxy_graph_structure_ui.R", local = TRUE, encoding = 'UTF-8')$value
#   )
# )

shiny::shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Visualisador de Relacionamentos",
      titleWidth = 340),
    dashboardSidebar(
      width = 340,
      collapsed = TRUE,
      fileInput("data_file", "Arquivo de consulta do NICE (xlsx)",
                multiple = FALSE,
                buttonLabel = 'Carregar',
                placeholder = 'Nenhum arquivo selecionado...',
                width = '100%'),
      
      # Copy the line below to make a slider bar 
      sliderInput("sldRaioVizinhanca", label = "Raio da vizinhança", min = 1, 
                  max = 4, value = 1, width = '100%'),
      
      selectInput("selectFocusNode", "Foco no nó :", choices = NULL),
      sliderInput("sliderFocusScale", "Escala do foco : ", min = 1, max = 4, value = 2),
      
      materialSwitch(
        inputId = "switchAutoRefresh",
        label = "Ativar atualização automática", 
        value = TRUE,
        status = "primary"
      ),
      materialSwitch(
        inputId = "switchEditMode",
        label = "Ativar modo de edição",
        value = FALSE,
        status = "primary"
      ),
      
      column(
        width = 12,
        align = "left",
        offset = 0,
        downloadButton("btnDownload", "Baixar dados da visualização")
        )
    ),
    dashboardBody(
       # Acrescenta um icone de carregamento
      add_busy_spinner(spin = "fading-circle", color = "blue", timeout = 1000, position = "full-page"),
      source("./src/ui/proxy_viz_ui.R", local = TRUE, encoding = 'UTF-8')$value,
      source("./src/ui/proxy_data_source_ui.R", local = TRUE, encoding = 'UTF-8')$value,
      source("./src/ui/proxy_graph_structure_ui.R", local = TRUE, encoding = 'UTF-8')$value
    )
  )
)