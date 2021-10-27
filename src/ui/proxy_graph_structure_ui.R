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
          DT::DTOutput("tblEmpenhoDetalhes")),
        tabPanel('Sanções',
                 DT::DTOutput("tblSancoesDetalhes"))
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
              inputId = "multiSelectNodesPJAba3",
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
              inputId = "multiSelectNodesPFAba3",
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
                         label = "Selecionar/Excluir Todos")))
        ))),
  tabPanel(
    title = "Filtrar relacionamentos do Grafico",
    fluidRow(
      tabBox(
        width = 12,
        tabPanel(
          'Exibir Relacionamentos',
          column(
            width = 12,
            pickerInput("selectEdges",
                                choices = list("Socios" = "sociedade", "Parentes" = "parentesco", 
                                              "Vinculo Empregaticio" = "vinculo_empregaticio", "Telefones" = "telefone_empresa", "Empenhos" = "empenho"),
                               selected = c("sociedade", "parentesco", "vinculo_empregaticio", "telefone_empresa", "empenho"), 
                               multiple = TRUE, width = '60%', options = pickerOptions(multipleSeparator = ';')))),
        tabPanel(
          'Parentesco',
          column(
            width = 12,
            selectizeInput("op_parentes", 
                           label = "Escolha um tipo de Relacionamento:", 
                           choices = NULL, 
                           multiple = TRUE, 
                           width = '100%'))),
        tabPanel(
          'Vínculos empegatícios',
          column(
            width = 6,
            dateRangeInput("sldFiltroTemporalServ", label = "Filtro temporal de vinculo empregaticio",
                           min = as.Date("2000-01-01"),
                           max = as.Date("2021-03-15"),
                           start = as.Date("2000-01-01"),
                           end = as.Date("2018-12-31"),
                           width = '100%',
                           separator = 'à',
                           startview = 'year'
            ), actionButton("sincronizar_serv", label = "Sincronizar Todos"),
              tags$head(tags$style(type="text/css", "#sincronizar_serv {background-color:#428bca;color: #fff}"))
            )
          ),
        tabPanel(
          'Vínculos societários',
          column(
           width = 6,
           dateRangeInput("sldFiltroTemporal", label = "Filtro temporal societario",
                          min = as.Date("2000-01-01"),
                          max = as.Date("2021-03-15"),
                          start = as.Date("2000-01-01"),
                          end = as.Date("2021-03-15"),
                          width = '100%',
                          separator = 'à',
                          startview = 'year'
           ),
           actionButton("sincronizar_socio", label = "Sincronizar Todos"),
           tags$head(tags$style(type="text/css", "#sincronizar_socio {background-color:#428bca;color: #fff}"))
          )
        ),
        tabPanel(
          'Empenhos',
          column(
            width = 6,
            dateRangeInput("sldFiltroTemporalEmpenhos", label = "Filtro temporal empenhos",
                           min = as.Date("2000-01-01"),
                           max = as.Date("2021-03-15"),
                           start = as.Date("2000-01-01"),
                           end = as.Date("2018-12-31"),
                           width = '100%',
                           separator = 'à',
                           startview = 'year'
            ))
          ),
        tabPanel(
          'Sanções'))))
)