visNodesEdges <- reactive({
  graph <- dataos()$graph
  center_nodes <- selected_nodes()
  radius <- ego_radius()
  input_nos <- nos_selecionados()

  # atualiza filtro temporal de sócio
  filter_start_date(input$sldFiltroTemporal[1])
  filter_end_date(input$sldFiltroTemporal[2])
  
  # atualiza filtro temporal vinculo empregaticio
  filter_start_date_serv(input$sldFiltroTemporalServ[1])
  filter_end_date_serv(input$sldFiltroTemporalServ[2])
  
  
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
                                 'vinculo_empregaticio'))
  
  # vnodes <- ego_nodes %>%
  #   destaca_elementos(.,input_nos)
  
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
  
  if(!is.null(input_nos)){
    isolate({
      vnodes <- vnodes %>%
    mutate(shadow = case_when(
      id %in% input_nos ~ TRUE,
      T ~ FALSE
    ))
    })
    
  }
  
  choices_nodes <- vnodes$id
  names(choices_nodes) <- paste("[", vnodes$id, "]: ", vnodes$title)
  
  updateSelectInput(session, 'selectFocusNode',
                    choices = choices_nodes, selected = center_nodes[1])
  
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

observe({
  visNetworkProxy("network_auto") %>%
    visFocus(id = input$selectFocusNode, scale = input$sliderFocusScale)
})

observe({
  visNetworkProxy("network_not_auto") %>%
    visFocus(id = input$selectFocusNode, scale = input$sliderFocusScale)
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
  
    #sincroniza os filtros temporais
   if(input$sincronizar_serv){
     updateDateRangeInput(session, 'sldFiltroTemporal',
                          min = as.Date(data_start_date()),
                          max = as.Date(data_end_date()),
                          start = input$sldFiltroTemporalServ[1],
                          end = input$sldFiltroTemporalServ[2])
   }
  if(input$sincronizar_socio){
    updateDateRangeInput(session, 'sldFiltroTemporalServ',
                         min = as.Date(data_start_date_serv()),
                         max = as.Date(data_end_date_serv()),
                         start = input$sldFiltroTemporal[1],
                         end = input$sldFiltroTemporal[2])
  }
  
})

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