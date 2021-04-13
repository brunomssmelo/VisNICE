observe({
  
  graph_nodes <- igraph::as_data_frame(dataos()$graph, what = 'vertices') %>% 
    rename(id = name)
  
  nodes_pj <- graph_nodes %>%
    filter(group == 'PJ') %>%
    select(id, title)
  
  choices_pj <- nodes_pj$id
  names(choices_pj) <- paste0("[", nodes_pj$id,"]: ", nodes_pj$title)
  
  updateMultiInput(session, "multiSelectNodesPJ",
                   choices = choices_pj)
                   
  nodes_pf <- graph_nodes %>%
    filter(group == 'PF') %>%
    select(id, title)
  
  choices_pf <- nodes_pf$id
  names(choices_pf) <- paste0("[", nodes_pf$id,"]: ", nodes_pf$title)
  
  updateMultiInput(session, "multiSelectNodesPF",
                   choices = choices_pf)
  
  if(input$btnIncluiTodosPJ %%2 > 0){
    updateMultiInput(session, "multiSelectNodesPJ",
                      selected = choices_pj)
  }
  
  if(input$btnIncluiTodosPF %%2 > 0){
    updateMultiInput(session, "multiSelectNodesPF",
                      selected = choices_pf)
  }
  
})

observe({
    selected <- c(input$multiSelectNodesPJ,
                  input$multiSelectNodesPF)
    
    # atualiza filtro temporal
    filter_start_date(input$sldFiltroTemporal[1])
    filter_end_date(input$sldFiltroTemporal[2])
    
    # atualiza raio de vizinhanca
    ego_radius(input$sldRaioVizinhanca)
    
    # atualiza lista de nós selecionados
    selected_nodes(selected)
    
})

output$btnDownload <- downloadHandler(
  filename = function() { 
    paste("dados_grafo_", Sys.time(), ".xlsx", sep="")
  },
  content = function(file) {
    vnodes <- visNodesEdges()$vnodes %>% 
      select(-type, -starts_with("role")) %>% 
      rename(nome = title,
             tipo = group)
    
    vedges <- visNodesEdges()$vedges %>% 
      select(-color, -type, -title) %>% 
      rename(origem = from,
             destino = to,
             papel = role,
             inicio_relacao = start,
             fim_relacao = end)
    
    data <- vedges %>%
      inner_join(vnodes %>% 
                   rename(nome_origem = nome,
                          tipo_origem = tipo),
                 by = c('origem' = 'id')) %>% 
      inner_join(vnodes %>% 
                   rename(nome_destino = nome,
                          tipo_destino = tipo),
                 by = c('destino' = 'id'))
    write_xlsx(data, file)
  })