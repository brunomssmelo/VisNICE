Popula_Multi_Input <- function(tipo, nome_input){
  
  vnodes <- visNodesEdges()$vnodes
  vedges <- visNodesEdges()$vedges
  
  if(tipo == 'PJ'| tipo == 'PF'){
    nodes_data <- vnodes %>%
      filter(str_detect(group, tipo)) %>%
      select(id, title)
    
    choices_data <- nodes_data$id
    names(choices_data) <- paste0("[", nodes_data$id,"]: ", nodes_data$title)
    
    updateMultiInput(session, nome_input,
                     choices = choices_data, selected = NULL)
  } else {
  
  nodes_data <- vedges %>%
    filter(type == tipo) %>%
    select(from,to) %>%
    unlist() %>%
    unique()
  
  recupera_nodes_data <- vnodes %>%
    filter(id %in% nodes_data) %>%
    select(id, title) %>%
    unique()
  
  choices_data <- recupera_nodes_data$id
  names(choices_data) <- paste0("[", recupera_nodes_data$id, "]: ", recupera_nodes_data$title)
  
  updateMultiInput(session, nome_input,
                   choices = choices_data)
  }
  
}
Seleciona_Dados <- function(selected_data, nome_input, tipo){
  if(is.null(selected_data)){
    isolate({
      vnodes <- visNodesEdges()$vnodes
      
      nodes <- vnodes %>%
        filter(str_detect(group, tipo)) %>%
        select(id, title) %>%
        unique()
      
      selected_data <- nodes$id
      
      updateMultiInput(session, nome_input,
                       selected = selected_data)
    }) 
  }
}

observe({
  
  selected_data_pj <- NULL
  selected_data_pf <- NULL
  
  graph_nodes <- igraph::as_data_frame(dataos()$graph, what = 'vertices') %>% 
    rename(id = name)
  
  nodes_pj <- graph_nodes %>%
    filter(str_detect(group, 'PJ')) %>%
    select(id, title)

  choices_pj <- nodes_pj$id
  names(choices_pj) <- paste0("[", nodes_pj$id,"]: ", nodes_pj$title)
  
  updateMultiInput(session, "multiSelectNodesPJ",
                   choices = choices_pj, selected = selected_data_pj)
                   
  nodes_pf <- graph_nodes %>%
    filter(group == 'PF') %>%
    select(id, title)
  
  choices_pf <- nodes_pf$id
  names(choices_pf) <- paste0("[", nodes_pf$id,"]: ", nodes_pf$title)
  
  updateMultiInput(session, "multiSelectNodesPF",
                   choices = choices_pf, selected = selected_data_pf)
  
  Seleciona_Dados(selected_data_pj, "multiSelectNodesPJ", "PJ")
  Seleciona_Dados(selected_data_pf, "multiSelectNodesPF", "PF")

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

  tryCatch({
  Popula_Multi_Input('PF', 'multiSelectNodesPFAba3')
  Popula_Multi_Input('PJ','multiSelectNodesPJAba3')
  Popula_Multi_Input('parentesco','multiSelectEdgesParentes')
  Popula_Multi_Input('empenho','multiSelectEdgesCommitment')
  Popula_Multi_Input('vinculo_empregaticio','multiSelectEdgesEmployment')
  }, error = function(e){
    message("Não há dados presentes na visualização")
  })
  
})

observe({
    selected <- c(input$multiSelectNodesPJ,
                  input$multiSelectNodesPF)
    # atualiza raio de vizinhanca
    ego_radius(input$sldRaioVizinhanca)
    
    # atualiza lista de nÃ³s selecionados
    selected_nodes(selected)
    #nos_selecionados(nos_selecionados_aba3)
})

observeEvent(input$network_auto_graphChange, {
  
  vnodes <- visNodesEdges()$vnodes
  
  if(input$network_auto_graphChange$cmd == "addNode") {
    
    temp = bind_rows(
      vnodes,
      data.frame(id = input$network_auto_graphChange$id,
                 stringsAsFactors = F)
    )
    
    #visNodesEdges()$vnodes = temp
    
  }
})

observe({
  nos_selecionados_aba3 <- c(input$multiSelectNodesPJAba3,
                             input$multiSelectNodesPFAba3,
                             input$multiSelectEdgesParentes,
                             input$multiSelectEdgesEmployment,
                             input$multiSelectEdgesCommitment)
  
  nos_selecionados(nos_selecionados_aba3)
})

output$btnDownload <- downloadHandler(
  filename = function() { 
    paste("dados_grafo_", Sys.Date(), ".xlsx", sep="")
  },
  content = function(file) {
    vnodes <- visNodesEdges()$vnodes %>% 
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