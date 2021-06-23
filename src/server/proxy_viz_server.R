visNodesEdges <- reactive({
  graph <- dataos()$graph
  center_nodes <- selected_nodes()
  radius <- ego_radius()
  
  if(is.null(center_nodes))
    center_nodes <- dataos()$center_nodes
  
  vnode_ids <- ego_graph_nodes(graph, radius, center_nodes)
  
  graph_edges <- igraph::as_data_frame(graph, what = 'edges')
  graph_nodes <- igraph::as_data_frame(graph, what = 'vertices') %>% 
    rename(id = name)
  
  vedges <- graph_edges %>%
    filter(from %in% vnode_ids | to %in% vnode_ids) %>%
    filter((as.Date(start) >= as.Date(filter_start_date())) &
             (as.Date(end) <= as.Date(filter_end_date()) | end == 'Tempo indefinido') |
             role != 'socio') %>%
    unique()

  vnodes <- graph_nodes %>%
    filter((id %in% vedges$from) | (id %in% vedges$to)) %>%
    unique()
  
  updateSelectInput(session, 'selectFocusNode',
                    choices = vnode_ids)

  if(input$selectEdges == 2) {
    vedges <- graph_edges %>%
    filter(from %in% vnode_ids | to %in% vnode_ids) %>%
    filter((as.Date(start) >= as.Date(filter_start_date())) &
             (as.Date(end) <= as.Date(filter_end_date()) | end == 'Tempo indefinido')) %>%
    unique()
    
    vnodes <- graph_nodes %>%
      filter((id %in% vedges$from) | (id %in% vedges$to)) %>%
      unique()
  }
  if(input$selectEdges == 3) {
    vedges <- graph_edges %>%
      filter(from %in% vnode_ids | to %in% vnode_ids) %>%
      filter(type == 'parentesco') %>%
      unique()
    
    vnodes <- graph_nodes %>%
      filter((id %in% vedges$from) | (id %in% vedges$to)) %>%
      unique()
  }
  
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
    visGroups(useDefaultGroups = TRUE, groupname = "PJ", shape = "icon", 
              icon = list(code = "f1ad", color = "orange")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PF", shape = "icon", 
              icon = list(code = "f007")) %>%
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
    visGroups(useDefaultGroups = TRUE, groupname = "PJ", shape = "icon", 
              icon = list(code = "f1ad", color = "orange")) %>%
    visGroups(useDefaultGroups = TRUE, groupname = "PF", shape = "icon", 
              icon = list(code = "f007")) %>%
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
})