visNodesEdges <- reactive({
  graph <- dataos()$graph
  center_nodes <- selected_nodes()
  radius <- ego_radius()
  
  if(is.null(center_nodes))
    center_nodes <- dataos()$center_nodes
  
  vnode_ids <- ego_graph_nodes(graph, ego_radius(), center_nodes)
  
  graph_edges <- igraph::as_data_frame(graph, what = 'edges')
  graph_nodes <- igraph::as_data_frame(graph, what = 'vertices') %>% 
    rename(id = name)
  
  vnodes <- graph_nodes %>%
    filter(id %in% vnode_ids) %>%
    unique()
  
  vedges <- graph_edges %>%
    filter(from %in% vnode_ids | to %in% vnode_ids) %>%
    unique()
  
  updateSelectInput(session, 'selectFocusNode',
                    choices = vnode_ids)
  
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