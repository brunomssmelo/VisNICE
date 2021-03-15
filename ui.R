require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(visNetwork)

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
      
      sliderInput("sldRaioVizinhanca", label = "Raio da vizinhança", min = 1, 
                  max = 4, value = 1, width = '100%'),
      
      sliderInput("sldFiltroTemporal", label = "Filtro temporal",
                  min = as.Date("2011-01-01"),
                  max = as.Date("2021-03-15"),
                  value = c(
                    as.Date("2011-01-01"),
                    as.Date("2021-03-15")
                  ),
                  width = '100%'
      ),
      
      selectInput("selectFocusNode", "Foco no nó :", choices = NULL, width = '100%'),
      sliderInput("sliderFocusScale", "Escala do foco : ",
                  min = 1, max = 4, value = 2, width = '100%'),
      
      materialSwitch(
        inputId = "switchAutoRefresh",
        label = "Ativar atualização automática", 
        value = TRUE,
        status = "primary",
        width = '100%'
      ),
      materialSwitch(
        inputId = "switchEditMode",
        label = "Ativar modo de edição",
        value = FALSE,
        status = "primary",
        width = '100%'
      ),
      
      column(
        width = 12,
        align = "left",
        offset = 0,
        downloadButton("btnDownload", "Baixar dados da visualização", width = '100%')
        )
    ),
    dashboardBody(
      source("./src/ui/proxy_viz_ui.R", local = TRUE, encoding = 'UTF-8')$value,
      source("./src/ui/proxy_data_source_ui.R", local = TRUE, encoding = 'UTF-8')$value,
      source("./src/ui/proxy_graph_structure_ui.R", local = TRUE, encoding = 'UTF-8')$value
    )
  )
)