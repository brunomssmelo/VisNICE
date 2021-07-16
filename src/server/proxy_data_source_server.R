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
          a_parente_Org_Publico = nice_data$a_parente_Org_Publico,
          
          v_parente = nice_data$v_parente,
          v_empresa = nice_data$v_empresa,
          v_socio_pj = nice_data$v_socio_pj,
          v_socio_pf = nice_data$v_socio_pf,
          v_empregado = nice_data$v_empregado,
          v_empregador = nice_data$v_empregador,
          v_empresa_socio = nice_data$v_empresa_socio
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  }
  
  # atualiza filtro temporal
  filter_start_date(min(graph_data$a_socio$start, na.rm = T)[1])
  filter_end_date(max(graph_data$a_socio$end, na.rm = T)[1])
  data_start_date(min(graph_data$a_socio$start, na.rm = T)[1])
  data_end_date(max(graph_data$a_socio$end, na.rm = T)[1])
  
  build_source_graph(graph_data)
})