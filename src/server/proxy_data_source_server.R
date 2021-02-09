dataos <- reactive({
  
  if(is.null(input$data_file)){
    graph_data <- sample_data()
  }else{
    tryCatch(
      {
        nice_data <- load_xlsx_nice(input$data_file$datapath)
        
        is_sample_data(FALSE)
        
        graph_data <- list(
          a_socio = nice_data$a_socio,
          a_parente = nice_data$a_parente,
          
          v_parente = nice_data$v_parente,
          v_empresa = nice_data$v_empresa,
          v_socio_pj = nice_data$v_socio_pj,
          v_socio_pf = nice_data$v_socio_pf
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  }
  
  build_source_graph(graph_data)
})