shiny::tabsetPanel(id = 'tabset_principal',
  tabPanel(id = 'tabpanel_detalhes',
    title = "Detalhes dos elementos selecionados",
    fluidRow(
      tabBox(
        id = "tabbox_detalhes",
        width = 12,
        tabPanel('PJ',
          DT::DTOutput("tblPjDetalhes")),
        tabPanel('PF',
          DT::DTOutput("tblPfDetalhes")),
        tabPanel('Sócio',
          id = 'tabpanel_detalhes_socio',
          DT::DTOutput("tblSocioDetalhes")),
        tabPanel('Parente',
          DT::DTOutput("tblParenteDetalhes")),
        tabPanel('Empenho',
          DT::DTOutput("tblEmpenhoDetalhes"))
        ))),
  tabPanel(
    title = "Dados retornados pela Consulta",
    fluidRow(
      tabBox(
        width = 12,
        tabPanel(
          title = 'Empresas/Órgãos Públicos',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectNodesPJ",
              # label = "Empresas/Orgaos Públicos:",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosPJ",
                         label = "Selecionar/Excluir Todos"))),
        tabPanel(
          title = 'Pessoas Físicas',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectNodesPF",
              # label = "Pessoas Físicas:",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosPF",
                         label = "Selecionar/Excluir Todos")))))),
  tabPanel(
    title = "Destacar elementos do gráfico",
    fluidRow(
      tabBox(
        width = 12,
        tabPanel(
          title = 'Empresas/Órgãos Públicos',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectNodesPJ",
              # label = "Empresas/Orgaos Públicos:",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosPJ",
                         label = "Selecionar/Excluir Todos"))),
        tabPanel(
          title = 'Pessoas Físicas',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectNodesPF",
              # label = "Pessoas Físicas:",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosPF",
                         label = "Selecionar/Excluir Todos"))),
        tabPanel(
          title = 'Parentesco',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectEdgesParentes",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosParentes",
                         label = "Selecionar/Excluir Todos"))),
        tabPanel(
          title = 'Vínculos Empregatícios',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectEdgesEmployment",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosEmployment",
                         label = "Selecionar/Excluir Todos"))),
        tabPanel(
          title = 'Empenhos',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectEdgesCommitment",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosCommitment",
                         label = "Selecionar/Excluir Todos"))),
        tabPanel(
          title = 'Sanções',
          column(
            width = 12,
            multiInput(
              inputId = "multiSelectEdgesSanction",
              label = '',
              choices = NULL,
              choiceNames = "",
              choiceValues = "",
              width = '100%')),
          column(
            width = 12,
            align="center",
            actionButton("btnIncluiTodosSanction",
                         label = "Selecionar/Excluir Todos"))),
        ))),
  tabPanel(
    title = "Filtrar relacionamentos do Grafico",
    fluidRow(
      tabBox(
        width = 12,
        tabPanel(
          'Parentesco'),
        tabPanel(
          'Vínculos empegatícios'),
        tabPanel(
          'Vínculos societários'),
        tabPanel(
          'Empenhos'),
        tabPanel(
          'Sanções'))))
)