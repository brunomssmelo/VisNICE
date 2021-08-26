require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(visNetwork)
require(shinybusy)
require(dashboardthemes)

# shiny::shinyUI(
#   shiny::navbarPage(
#     "Consulta Nice",
#     source("./src/ui/proxy_viz_ui.R", local = TRUE, encoding = 'UTF-8')$value,
#     source("./src/ui/proxy_data_source_ui.R", local = TRUE, encoding = 'UTF-8')$value,
#     source("./src/ui/proxy_graph_structure_ui.R", local = TRUE, encoding = 'UTF-8')$value
#   )
# )

enableBookmarking(store = "server")

shiny::shinyUI( function(){
  dashboardPage(
    dashboardHeader(
      title = "Visualizador de Relacionamentos",
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
      
      sliderInput("sldFiltroTemporal", label = "Filtro temporal societario",
                  min = as.Date("2000-01-01"),
                  max = as.Date("2021-03-15"),
                  value = c(
                    as.Date("2000-01-01"),
                    as.Date("2021-03-15")
                  ),
                  width = '100%'
      ),
      
      sliderInput("sldFiltroTemporalServ", label = "Filtro temporal de vinculo empregaticio",
                  min = as.Date("2000-01-01"),
                  max = as.Date("2021-08-19"),
                  value = c(
                    as.Date("2000-01-01"),
                    as.Date("2021-08-19")
                  ),
                  width = '100%'
      ),
      
      selectInput("selectFocusNode", "Foco no nó :", choices = NULL, width = '100%'),
      sliderInput("sliderFocusScale", "Escala do foco : ",
                  min = 1, max = 4, value = 2, width = '100%'),
      div(
             #pickerInput("selectEdges", label = "Exibir Apenas:", choices = list("Socios" = "sociedade", "Parentes" = "parentesco", "Vinculo Empregaticio" = "vinculo_emp"), multiple = TRUE),
             checkboxGroupInput("selectEdges", label = "Exibir Apenas:",choices = list("Socios" = "sociedade", "Parentes" = "parentesco", "Vinculo Empregaticio" = "vinculo_emp"), selected = c("sociedade", "parentesco", "vinculo_emp")),
             selectizeInput("op_parentes", label = "Escolha um tipo de Relacionamento:", choices = NULL, multiple = TRUE, width = '100%')
             ),
      
      
      column(
        width = 12,
        style = "color: #000",
        materialSwitch(
        inputId = "switchAutoRefresh",
        label = "Ativar atualização automática", 
        value = TRUE,
        status = "primary",
        width = '100%'
      )) ,
      column(
        width = 12,
        style = "color: #000",
        align = "left",
        offset = 0,
        materialSwitch(
        inputId = "switchEditMode",
        label = "Ativar modo de edição",
        value = FALSE,
        status = "primary",
        width = '100%',
      )),
      
      column(
        width = 12,
        align = "left",
        offset = 0,
        downloadButton("btnDownload", "Baixar dados da visualização", width = '100%')
        ),
      tags$head(tags$style(type="text/css", "#btnDownload {background-color:#428bca;color: #fff};"),
                tags$style(HTML(".span {color: black}"))
                )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "onenote"
      ),
       # Acrescenta um icone de carregamento
      fluidRow(column(width = 12, align="right",
      bookmarkButton(label = "Salvar para depois"))),
      br(),
      add_busy_spinner(spin = "fading-circle", color = "blue", timeout = 1000, position = "full-page"),
      source("./src/ui/proxy_viz_ui.R", local = TRUE, encoding = 'UTF-8')$value,
      source("./src/ui/proxy_data_source_ui.R", local = TRUE, encoding = 'UTF-8')$value,
      source("./src/ui/proxy_graph_structure_ui.R", local = TRUE, encoding = 'UTF-8')$value
    )
  )}
)