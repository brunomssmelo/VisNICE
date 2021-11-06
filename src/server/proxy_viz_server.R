visNodesEdges <- reactive({
  graph <- dataos()$graph
  center_nodes <- selected_nodes()
  radius <- ego_radius()

  # atualiza filtro temporal de sócio
  filter_start_date(input$sldFiltroTemporal[1])
  filter_end_date(input$sldFiltroTemporal[2])
  
  # atualiza filtro temporal vinculo empregaticio
  filter_start_date_serv(input$sldFiltroTemporalServ[1])
  filter_end_date_serv(input$sldFiltroTemporalServ[2])
  
  # atualiza filtro temporal empenhos
  filter_start_date_emp(input$sldFiltroTemporalEmpenhos[1])
  filter_end_date_emp(input$sldFiltroTemporalEmpenhos[2])
  
  
  if(is.null(center_nodes))
    center_nodes <- dataos()$center_nodes
  
  ego <- ego_graph(graph, radius, center_nodes)
  
  graph_edges <- igraph::as_data_frame(graph, what = 'edges')
  graph_nodes <- igraph::as_data_frame(graph, what = 'vertices') %>% 
    rename(id = name)
  
  ego_edges <- igraph::as_data_frame(ego, what = 'edges')
  ego_nodes <- igraph::as_data_frame(ego, what = 'vertices') %>% 
    rename(id = name) %>% 
    left_join(graph_nodes, by = 'id')


  # aplicação dos filtros temporais
  vedges <- ego_edges %>%
    filter(check_filter_interval(.,filter_start_date(),
                                 filter_end_date(),
                                 'sociedade')) %>% 
    filter(check_filter_interval(.,filter_start_date_serv(),
                                 filter_end_date_serv(),
                                 'vinculo_empregaticio')) %>%
    filter(check_filter_interval(.,filter_start_date_emp(),
                               filter_end_date_emp(),
                               'empenho'))
  
  if(!is.null(input$op_parentes)){
  
    op_parentes <- input$op_parentes
    vedges <- vedges %>%
      filter(role %in% op_parentes | type != 'parentesco') %>%
      unique()
  }
  
  if(!is.null(input$selectEdges)){
    vedges <- vedges %>%
      filter(type %in% input$selectEdges) %>%
      unique()
  }
  
  vnodes <- ego_nodes %>%
    filter((id %in% vedges$from) | (id %in% vedges$to)) %>%
    unique()
  
  parentesco_selecionados <- vedges %>%
    filter(type == 'parentesco') %>%
    select(role) %>%
    unlist() %>%
    unique()
  
  todos_parentesco <- graph_edges %>%
    filter(type == 'parentesco') %>%
    select(role) %>%
    unlist() %>%
    unique()
  
  updateSelectizeInput(session, "op_parentes",
                       choices = todos_parentesco, selected = parentesco_selecionados)
  
  list(vnodes = vnodes, vedges = vedges)
})

output$network_auto <- renderVisNetwork({
  
  vnodes <- visNodesEdges()$vnodes
  vedges <- visNodesEdges()$vedges
  
  title <- ''
  subtitle <- ''
  
  if(is_sample_data()){
    title <- "Rede de exemplo (fictícia)"
    subtitle <- "<--- carregue um arquivo xlsx de consulta ao NICE"
  }
  
  vis <- visNetwork(vnodes, vedges, main = title, submain = subtitle) %>%
    visIgraphLayout(physics = TRUE, smooth = TRUE) %>%
    # visPhysics(stabilization = FALSE) %>%
    # visEdges(smooth = FALSE) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PJ_PRIVADO", shape = "icon",
              icon = list(code = "f1ad", color = "orange")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PJ_PUBLICO", shape = "icon",
              icon = list(code = "f19c", color = "seagreen")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PF", shape = "icon",
              icon = list(code = "f007")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "TEL", shape = "icon",
              icon = list(code = "f098", color = "black"))%>%
    addFontAwesome(name = "font-awesome-visNetwork") %>%
    visEdges(arrows = "to") %>%
    visLegend(addEdges = dataos()$ledges, position = 'right') %>%
    visOptions(manipulation = input$switchEditMode) %>%
    visEvents(selectEdge = "function(data) {
            Shiny.onInputChange('current_nodes_selected', data.nodes);
            Shiny.onInputChange('current_edges_selected', data.edges);
            ;}") %>%
    visExport(type = 'jpeg', label = "Exportar como jpeg")
  
  vis
})


output$network_not_auto <- renderVisNetwork({
  
  input$actionRefresh
  
  is_sample_data <- F
  title <- ''
  subtitle <- ''
  
  isolate({
    if(is_sample_data()){
      title <- "Rede de exemplo (fictícia)"
      subtitle <- "<--- carregue um arquivo xlsx de consulta ao NICE"
    }
    
    vnodes <- visNodesEdges()$vnodes
    vedges <- visNodesEdges()$vedges
  })
  
  vis <- visNetwork(vnodes, vedges, main = title, submain = subtitle) %>%
    visIgraphLayout(physics = TRUE, smooth = TRUE) %>%
    # visPhysics(stabilization = FALSE) %>%
    # visEdges(smooth = FALSE) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PJ_PRIVADO", shape = "icon",
              icon = list(code = "f1ad", color = "orange")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PJ_PUBLICO", shape = "icon",
              icon = list(code = "f19c", color = "seagreen")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PF", shape = "icon",
              icon = list(code = "f007")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "TEL", shape = "icon",
              icon = list(code = "f098", color = "black"))%>%
    addFontAwesome(name = "font-awesome-visNetwork") %>%
    visEdges(arrows = "to") %>%
    visLegend(addEdges = dataos()$ledges, position = 'right') %>%
    visOptions(manipulation = input$switchEditMode) %>%
    visExport(type = 'jpeg', label = "Exportar como jpeg")

  vis
})

#Destaca os elementos do gráfico
observeEvent(input$switchSelecao, {
  visNetworkProxy("network_auto")%>%
    visNodes(shadow = TRUE)%>%
    visEdges(shadow = TRUE)
  
  shinyjs::disable("multiSelectNodesPJAba3")
  
})

observe({
  vnodes <- visNodesEdges()$vnodes
  vedges <- visNodesEdges()$vedges
  nos_select <- nos_selecionados()
  valor_switch <- input$switchTitle
  
  nodes <- vnodes %>%
    mutate(shadow = case_when(
      id %in% nos_select ~ TRUE,
      T ~ FALSE
    ), shadow.size = 30)
  
  if(valor_switch == TRUE){
    nodes <- vnodes %>%
      mutate(font.size = 15)%>%
      mutate(label = case_when(
        id %in% nos_select ~ title
      ))
  } else {
    nodes <- vnodes %>%
      mutate(font.size = 0)
  }
  
  edges <- vedges %>%
    mutate(shadow = case_when(
      from %in% nos_select ~ TRUE,
      to %in% nos_select ~ TRUE,
      T ~ FALSE
    ), shadow.size = 30)
  
  visNetworkProxy("network_auto") %>%
    visUpdateNodes(nodes = nodes) %>%
    visUpdateEdges(edges = edges)
})

observe({
  
  updateDateRangeInput(session, 'sldFiltroTemporal',
                    min = as.Date(data_start_date()),
                    max = as.Date(data_end_date()),
                    start = as.Date(filter_start_date()),
                    end = as.Date(filter_end_date()))
  
  updateDateRangeInput(session, 'sldFiltroTemporalServ',
                    min = as.Date(data_start_date_serv()),
                    max = as.Date(data_end_date_serv()),
                          start = as.Date(filter_start_date_serv()),
                          end = as.Date(filter_end_date_serv()))
  #verificação para impedir o campo de receber valores nulos
  if(is.na(filter_end_date_emp())){
    
    max_emp <- data_end_date_emp()
    
  }else{
    max_emp <- filter_end_date_emp()
  }
  
  updateDateRangeInput(session, 'sldFiltroTemporalEmpenhos',
                       min = as.Date(data_start_date_emp()),
                       max = as.Date(data_end_date_emp()),
                       start = as.Date(filter_start_date_emp()),
                       end = as.Date(max_emp))
  
    #sincroniza os filtros temporais
  # if(!is.null(input$sincronizar_serv)){
  #   Sincroniza_Dados(input$tab_filtros,input$sldFiltroTemporalServ[1],
  #                    input$sldFiltroTemporalServ[2])
  # }
  # if(!is.null(input$sincronizar_socio)){
  #   Sincroniza_Dados(input$tab_filtros,input$sldFiltroTemporal[1],
  #                    input$sldFiltroTemporal[2])
  # }
  # if(!is.null(input$sincronizar_emp)){
  #   Sincroniza_Dados(input$tab_filtros,input$sldFiltroTemporalEmpenhos[1],
  #                    input$sldFiltroTemporalEmpenhos[2])
  # }
  
})

# Sincroniza_Dados <- function(tab_selected,data1,data2){
#   if(tab_selected == 'tab_servidor'){
#     data_start_sinc <- data1
#     data_end_sinc <- data2
#     updateDateRangeInput(session, 'sldFiltroTemporal',
#                          min = as.Date(data_start_date()),
#                          max = as.Date(data_end_date()),
#                          start = as.Date(data_start_sinc),
#                          end = as.Date(data_end_sinc))
#     updateDateRangeInput(session, 'sldFiltroTemporalEmpenhos',
#                          min = as.Date(data_start_date()),
#                          max = as.Date(data_end_date()),
#                          start = as.Date(data_start_sinc),
#                          end = as.Date(data_end_sinc))
#   }
#   if(tab_selected == 'tab_socio'){
#     data_start_sinc <- data1
#     data_end_sinc <- data2
#     updateDateRangeInput(session, 'sldFiltroTemporalServ',
#                          min = as.Date(data_start_date()),
#                          max = as.Date(data_end_date()),
#                          start = as.Date(data_start_sinc),
#                          end = as.Date(data_end_sinc))
#     updateDateRangeInput(session, 'sldFiltroTemporalEmpenhos',
#                          min = as.Date(data_start_date()),
#                          max = as.Date(data_end_date()),
#                          start = as.Date(data_start_sinc),
#                          end = as.Date(data_end_sinc))
#   }
#   if(tab_selected == 'tab_empenho'){
#     data_start_sinc <- data1
#     data_end_sinc <- data2
#     updateDateRangeInput(session, 'sldFiltroTemporalServ',
#                          min = as.Date(data_start_date()),
#                          max = as.Date(data_end_date()),
#                          start = as.Date(data_start_sinc),
#                          end = as.Date(data_end_sinc))
#     updateDateRangeInput(session, 'sldFiltroTemporal',
#                          min = as.Date(data_start_date()),
#                          max = as.Date(data_end_date()),
#                          start = as.Date(data_start_sinc),
#                          end = as.Date(data_end_sinc))
#   }
# }

output$tblPjDetalhes <- renderDT({
  
  data <- NULL
  
  if(!is.null(input$current_nodes_selected)){
    selected_nodes <- visNodesEdges()$vnodes %>% 
      filter(id %in% input$current_nodes_selected) %>% 
      distinct()
    
    if(selected_nodes$group == 'PJ_PRIVADO'){
      data <- dataos()$data$pessoa_juridica %>% 
        inner_join(select(selected_nodes, id), by = c('NUM_CNPJ' = 'id')) %>% 
        distinct() %>% 
        arrange(NOME)
    }
    
    showTab(inputId = 'tabbox_detalhes',
            target = 'tabpanel_detalhes_pj',
            select = TRUE, session = getDefaultReactiveDomain())
  }else{
    hideTab(inputId = 'tabbox_detalhes',
            target = 'tabpanel_detalhes_pj',
            session = getDefaultReactiveDomain())
  }
  
  data
}, options = list(scrollX = TRUE))

output$tblPfDetalhes <- renderDT({
  
  selected_nodes <- input$current_nodes_selected
  
  if (!is.null(selected_nodes)){
    selected_nodes <- visNodesEdges()$vnodes %>% 
      filter(id %in% selected_nodes) %>%
      filter(group == 'PF') %>% 
      distinct()
    
    if (!is.null(selected_nodes) & !is.null(dataos()$data$pessoa_fisica)){
      selected_nodes <- dataos()$data$pessoa_fisica %>%
        inner_join(select(selected_nodes, id, title), by = c("id" = "id")) %>% 
        distinct() %>%
        arrange(id)
    }
  }
  
  selected_nodes
}, options = list(scrollX = TRUE))

output$tblSancoesDetalhes <- renderDT({
  
  selected_edges <- NULL
  
  selected_edges
}, options = list(scrollX = TRUE))

output$tblEmpenhoDetalhes <- renderDT({
  
  selected_edges <- input$current_edges_selected
  
  if (!is.null(selected_edges)){
    selected_edges <- visNodesEdges()$vedges %>% 
      filter(id %in% selected_edges) %>%
      filter(type == 'empenho') %>% 
      distinct()
    
    if (!is.null(selected_edges) & !is.null(dataos()$data$empenho)){
      selected_edges <- dataos()$data$empenho %>%
        inner_join(select(selected_edges, CnpjUnidadeGestora = from, CpfCnpjCredor = to)) %>% 
        distinct() %>% 
        arrange(DataInicio)
    }
  }
  
  selected_edges
}, options = list(scrollX = TRUE))

output$tblParenteDetalhes <- renderDT({
  
  selected_edges <- input$current_edges_selected
  
  if (!is.null(selected_edges)){
    selected_edges <- visNodesEdges()$vedges %>% 
      filter(id %in% selected_edges) %>%
      filter(type == 'parentesco') %>% 
      distinct()
    
    if (!is.null(selected_edges) & !is.null(dataos()$data$parentesco)){
      selected_edges <- dataos()$data$parentesco %>%
        inner_join(select(selected_edges, CPF1 = from, CPF2 = to)) %>% 
        distinct()
    }
  }
  
  selected_edges
}, options = list(scrollX = TRUE))

output$tblSocioDetalhes <- renderDT({
  
  selected_edges <- input$current_edges_selected
  
  if (!is.null(selected_edges)){
    selected_edges <- visNodesEdges()$vedges %>% 
      filter(id %in% selected_edges) %>%
      filter(type == 'sociedade') %>% 
      distinct()
    
    if (!is.null(selected_edges) & !is.null(dataos()$data$socio)){
      selected_edges <- dataos()$data$socio %>%
        inner_join(select(selected_edges, NUM_CPF = from, NUM_CNPJ_EMPRESA = to)) %>% 
        distinct()
    }
  }
  
  selected_edges
}, options = list(scrollX = TRUE))

output$tblSancoesDetalhes <- renderDT({
  
  selected_edges <- input$current_edges_selected
  
  if (!is.null(selected_edges)){
    selected_edges <- visNodesEdges()$vedges %>% 
      filter(id %in% selected_edges) %>%
      #filter() %>% 
      distinct()
    
    if (!is.null(selected_edges) & !is.null(dataos()$data$sancionado)){
      selected_edges <- dataos()$data$sancionado %>%
        inner_join(select(selected_edges, by = c("id" = "id"))) %>% 
        distinct() %>%
        arrange(id)
    }
  }
})
