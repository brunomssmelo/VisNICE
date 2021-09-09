observe({
  
  graph_nodes <- igraph::as_data_frame(dataos()$graph, what = 'vertices') %>% 
    rename(id = name)
  
  nodes_pj <- graph_nodes %>%
    filter(group != 'PF') %>% # <------- MUDAR AQUI (Contemplar separadamente PJ público e privado)
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
  
  graph_edges <- igraph::as_data_frame(dataos()$graph, what = 'edges')
  
  edges_op <- graph_edges %>%
    filter(type == 'parentesco') %>%
    select(role) %>% unique()
  
  choices_op <- edges_op$role
  
  updateSelectizeInput(session, "op_parentes",
                    choices = sort(choices_op))
  
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
    
    # atualiza raio de vizinhanca
    ego_radius(input$sldRaioVizinhanca)
    
    # atualiza lista de nós selecionados
    selected_nodes(selected)
    
})

observeEvent(input$network_auto_graphChange, {
  
  vnodes <- visNodesEdges()$vnodes
  
  if(input$network_auto_graphChange$cmd == "addNode") {
    
    temp = bind_rows(
      vnodes,
      data.frame(id = input$network_auto_graphChange$id,
                 stringsAsFactors = F)
    )
    
    visNodesEdges()$vnodes = temp
    
  }
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

onRestored(function(state){
  updateMultiInput(session, "multiSelectNodesPF", selected = state$input$multiSelectNodesPF)
  updateMultiInput(session, "multiSelectNodesPJ", selected = state$input$multiSelectNodesPJ)
  updateSelectInput(session, "selectFocusNode", selected = state$input$selectFocusNode)
  updateSelectizeInput(session, "op_parentes", selected = state$input$op_parentes)
})