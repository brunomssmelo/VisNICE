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
  
  if(!is.null(input$selectEdges)){
    vedges <- vedges %>%
      filter(type %in% input$selectEdges) %>%
      unique()
  }
  
  if(!is.null(input$op_parentes)){
    op_parentes <- input$op_parentes
    
    vedges <- vedges %>%
      filter(role %in% op_parentes | type != 'parentesco') %>%
      unique()
  }
  
  vnodes <- ego_nodes %>%
    filter((id %in% vedges$from) | (id %in% vedges$to)) %>%
    unique()
  
  choices_nodes <- vnodes$id
  names(choices_nodes) <- paste("[", vnodes$id, "]: ", vnodes$title)
  
  updateSelectInput(session, 'selectFocusNode',
                    choices = choices_nodes, selected = center_nodes[1])
  
  list(vnodes = vnodes, vedges = vedges)
})

output$network_auto <- renderVisNetwork({
  
  # browser()
  
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
  
  updateSliderInput(session, 'sldFiltroTemporal',
                    min = as.Date(data_start_date()),
                    max = as.Date(data_end_date()),
                    value = c(as.Date(filter_start_date()),
                              as.Date(filter_end_date())))
  
  updateSliderInput(session, 'sldFiltroTemporalServ',
                    min = as.Date(data_start_date_serv()),
                    max = as.Date(data_end_date_serv()),
                    value = c(as.Date(filter_start_date_serv()),
                              as.Date(filter_end_date_serv())))
})