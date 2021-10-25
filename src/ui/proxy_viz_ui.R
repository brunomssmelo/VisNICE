shiny::tabPanel(
  title = "Rede de Relacionamentos",
  conditionalPanel(
    condition = "input.switchAutoRefresh == true",
    visNetworkOutput("network_auto",
                     height = "670px", width = '100%')),
  conditionalPanel(
    condition = "input.switchAutoRefresh == false",
    visNetworkOutput("network_not_auto",
                     height = "640px", width = '100%'),
    fluidRow(
      column(
        width = 12,
        align = 'right',
        actionButton(inputId = 'actionRefresh', label = 'Atualiza')
      )
    )
  )
)