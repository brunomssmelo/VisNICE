dataos <- reactive({
  
  if(is.null(input$data_file)){
    graph_data <- sample_data()
    
    # verificar se o switch esta recebendo true 
    if(input$switchChoose){
      
      cnpj <- NULL
      
      if(input$search_cnpj)
        cnpj <- isolate(input$text_cnpj)

      # passar como parametro o input de cnpjs
      nice_data <- load_data(cnpj) 
      
      # updateMultiInput(updateMultiInput(session, "multiSelectNodesPJ",
      #                                   selected = select_cnpj))
      
     
      is_sample_data(FALSE)

      graph_data <- list(
        edges = nice_data$edges,
        nodes = nice_data$nodes,
        data = nice_data$data
      )
     }
  }else{
    tryCatch(
      {
        nice_data <- load_data(input$data_file$datapath) # load_xlsx_nice(input$data_file$datapath)
        
        is_sample_data(FALSE)
        
        graph_data <- list(
          edges = nice_data$edges,
          nodes = nice_data$nodes
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  }
  # atualiza filtro temporal de sócio
  data_start_date(get_role_min_date(graph_data, 'sociedade'))
  data_end_date(get_role_max_date(graph_data, 'sociedade'))
  
  filter_start_date(data_start_date())
  filter_end_date(data_end_date())
  
  # atualiza filtro temporal vinculo empregaticio
  data_start_date_serv(get_role_min_date(graph_data, 'vinculo_empregaticio'))
  data_end_date_serv(get_role_max_date(graph_data, 'vinculo_empregaticio'))
  
  filter_start_date_serv(data_start_date_serv())
  filter_end_date_serv(data_end_date_serv())
  
  # atualiza filtro temporal empenhos
  data_start_date_emp(get_role_min_date(graph_data, 'empenho'))
  data_end_date_emp(get_role_max_date(graph_data, 'empenho'))

  filter_start_date_emp(data_start_date_emp())
  filter_end_date_emp(data_end_date_emp())

  build_source_graph(graph_data)
})