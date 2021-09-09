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
          a_vinculo_servidor = nice_data$a_vinculo_servidor,
          a_tel_empresa = nice_data$a_tel_empresa,
          a_socio_parentesco = nice_data$a_socio_parentesco,
          a_vinculo_servidor_pub = nice_data$a_vinculo_servidor_pub,
          a_vinculo_socio_servidor = nice_data$a_vinculo_socio_servidor,
          
          v_parente = nice_data$v_parente,
          v_empresa = nice_data$v_empresa,
          v_socio_pj = nice_data$v_socio_pj,
          v_socio_pf = nice_data$v_socio_pf,
          v_servidor = nice_data$v_servidor,
          v_orgao_publico = nice_data$v_orgao_publico,
          v_empresa_socio = nice_data$v_empresa_socio,
          v_telefones = nice_data$v_telefones,
          v_cnpj = nice_data$v_cnpj,
          v_socio_parentesco = nice_data$v_socio_parentesco,
          v_servidor_pub = nice_data$v_servidor_pub,
          v_org_publico = nice_data$v_org_publico,
          v_socio_servidor = nice_data$v_socio_servidor,
          v_socio_org_publico = nice_data$v_socio_org_publico
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
  
  # atualiza filtro temporal vinculo empregaticio
  filter_start_date_serv(min(graph_data$a_vinculo_servidor$start, na.rm = T)[1])
  filter_end_date_serv(max(graph_data$a_vinculo_servidor$end, na.rm = T)[1])
  data_start_date_serv(min(graph_data$a_vinculo_servidor$start, na.rm = T)[1])
  data_end_date_serv(max(graph_data$a_vinculo_servidor$end, na.rm = T)[1])
  
  build_source_graph(graph_data)
})