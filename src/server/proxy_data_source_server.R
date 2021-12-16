dataos <- reactive({
  
  if(is.null(input$data_file)){
    graph_data <- sample_data()
  
  #botão para chamar a busca no banco de dados
  observeEvent(input$search_cnpj,{
    
    if(!is.null(input$text_cnpj) & str_length(input$text_cnpj) >= 8){
      cnpj(input$text_cnpj)
    } else {
      cnpj(NULL)
      show_alert("Por favor insira um número com 8 ou mais caracteres!", type = "warning")
    }
  print(cnpj())
  })
  
  if(!is.null(cnpj())){
   
    tryCatch(
      {
        # passar como parametro o input de cnpjs
        nice_data <- load_data(cnpj())
        
        cnpj_dt <- cnpj()
        
        #verificação para retorno vazio de nós e arestas
        if(nrow(nice_data$nodes) == 0 & nrow(nice_data$edges) == 0){
          
          graph_data <- sample_data()
          updateTextInput(session, "text_cnpj", value = "")
          show_alert(paste("Não há o número de cnpj:",cnpj_dt,"na base de dados pesquisada!"), type = "error")
          
        } else {
          
          is_sample_data(FALSE)
          
          cnpj_dt <- unlist(str_extract_all(cnpj(),"[0-9]{8}"))
          cnpjs <- NULL
          
          for (i in 1:length(cnpj_dt)) {
            if(is_empty(nice_data$nodes$id[str_detect(nice_data$nodes$id, cnpj_dt[i])])){
              
              cnpjs <- append(cnpjs, cnpj_dt[i])
              show_alert(paste("Não há o número de cnpj:",cnpjs[i],"na base de dados pesquisada!"), type = "error")
              
            }
          }
          
          graph_data <- list(
            edges = nice_data$edges,
            nodes = nice_data$nodes,
            data = nice_data$data
          )
    
          #updateMaterialSwitch(session, "switchChoose", value = TRUE)
        }
      }, error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  } else {
    is_sample_data(TRUE)
  }
    
      }
  else {
    tryCatch(
      {
        
        nice_data <- load_data(input$data_file$datapath) # load_xlsx_nice(input$data_file$datapath)
        
        is_sample_data(FALSE)
        
        graph_data <- list(
          edges = nice_data$edges,
          nodes = nice_data$nodes,
          data = nice_data$data
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