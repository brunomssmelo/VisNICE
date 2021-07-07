load_xlsx_nice <- function(data_path){
  library(tidyverse)
  library(readxl)
  
  
  # Carrega dados da planilha de informações do NICE ####
  
  
  # Worksheet "Listados" #################################################################################
  
  base_listados <- read_excel(data_path,
                              sheet = "Listados")
  
  # Worksheet "1_CNPJ" ###################################################################################
  
  base_cnpj <- read_excel(data_path,
                          sheet = "1_CNPJ",
                          col_types = c("text", rep("guess", 25)))
  
  # Worksheet "3_Socio" ##################################################################################
  
  base_socios <- read_excel(data_path,
                            sheet = "3_Socio",
                            na = 'NULL',
                            col_types = c("text", "guess", "text", rep("guess", 5)))
  base_socios <- base_socios %>% 
    mutate(NUM_CNPJ_EMPRESA = str_pad(NUM_CNPJ_EMPRESA, pad = '0', side = 'left', width = 14)) %>% 
    mutate(NUM_CPF_CNPJ_SOCIO = case_when(
      nchar(NUM_CPF_CNPJ_SOCIO) > 11 ~ str_pad(NUM_CPF_CNPJ_SOCIO, pad = '0', side = 'left', width = 14),
      T ~ str_pad(NUM_CPF_CNPJ_SOCIO, pad = '0', side = 'left', width = 11))) %>% 
    mutate(NIVEL = 1)
  
  # %>% 
  #   mutate(NIVEL = as.numeric(!(NUM_CPF_CNPJ_SOCIO %in% c(alvos_agentes_publicos, alvos_socios))))
  
  # Extrai nós e arestas de vínculos societários #########################################################
  
  v_socio_pf <- base_socios %>% 
    filter(nchar(NUM_CPF_CNPJ_SOCIO)==11) %>% 
    select(id = NUM_CPF_CNPJ_SOCIO, title = NOME_SOCIO, type = NIVEL) %>% 
    filter(!is.na(id)) %>% 
    mutate(group = 'PF',
           role = 'socio') %>% 
    filter(!duplicated(id))
   
  v_socio_pj <- base_socios %>% 
    filter(nchar(NUM_CPF_CNPJ_SOCIO)==14) %>% 
    select(id = NUM_CPF_CNPJ_SOCIO, title = NOME_SOCIO, type = NIVEL) %>% 
    filter(!is.na(id)) %>% 
    mutate(group = 'PJ',
           role = 'socio') %>% 
    filter(!duplicated(id))
  
  v_empresa <- base_listados %>% 
    select(id = NUM_CNPJ_CPF, title = NOME, type = NIVEL) %>% 
    filter(!is.na(id)) %>% 
    mutate(group = 'PJ',
           role = 'empresa') %>% 
    arrange(type) %>% 
    filter(!duplicated(id))
  
  a_socio <- base_socios %>% 
    select(from = NUM_CPF_CNPJ_SOCIO,
           to = NUM_CNPJ_EMPRESA,
           start = DATA_ENTRADA_SOCIEDADE,
           end = DATA_SAIDA) %>% 
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>% 
    mutate(role = 'socio',
           type = 'sociedade')
  
  # grafo_vinculos_societarios <- graph_from_data_frame(d = a_vinculos_societarios, 
  #                                                     vertices = v_vinculos_societarios) 
  
  
  # Worksheet "8_Parentesco" #############################################################################
  
  base_parentesco <- read_excel(data_path,
                                sheet = "8_Parentesco",
                                na = 'NULL',
                                col_types = 'text') %>% 
    mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
           CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11))
  
  # Extrai nós e arestas de vínculos de parentesco #######################################################
  # 
  v_parente <- base_parentesco %>%
    select(id = CPF2, title = NOME2) %>%
    filter(!is.na(id)) %>% 
    mutate(group = 'PF',
           role = 'parente')
  
  v_parente <- base_parentesco %>%
    select(id = CPF1, title = NOME1) %>%
    filter(!is.na(id)) %>% 
    mutate(group = 'PF',
           role = 'parente') %>%
    bind_rows(v_parente) %>% 
    filter(!duplicated(id)) %>% 
    mutate(type = 1)
  
  a_parente <- base_parentesco %>%
    filter(!is.na(CPF1)) %>% 
    filter(!is.na(CPF2)) %>% 
    filter(!(CPF1 == CPF2)) %>% 
    select(from = CPF1,
           to = CPF2,
           role = RELACAO) %>%
    mutate(type = 'parentesco',
           role = tolower(role))
  
  # Worksheet "17_Parente_Org_Publico" ############################################################################
  
  base_parente_Org_Publico <- read_excel(data_path,
                                         sheet = "17_Parente_Org_Publico",
                                         na = 'NULL',
                                         col_types = c(rep("text",7), rep("guess", 3)))%>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14))
  
  v_empregado <- base_parente_Org_Publico %>%
    filter(nchar(CO_CPF)==11) %>% 
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'empregado')%>%
    filter(!duplicated(id))
  
  v_empregador <- base_parente_Org_Publico %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ',
           role = 'empregador')%>%
    filter(!duplicated(id))
  
  a_parente_Org_Publico <- base_parente_Org_Publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>% 
    mutate(role = 'parente_pub',
           type = 'vinculo_emp')
  
  ### dados consulta nice  ################################################################################
  
  list(
    ws_listados = base_listados,
    ws_cnpj = base_cnpj,
    ws_socios = base_socios,
    ws_parentesco = base_parentesco,
    ws_base_parente_Org_Publico = base_parente_Org_Publico,
    
    a_socio = a_socio,
    a_parente = a_parente,
    a_parente_Org_Publico = a_parente_Org_Publico,
    
    v_parente = v_parente,
    v_empresa = v_empresa,
    v_socio_pj = v_socio_pj,
    v_socio_pf = v_socio_pf,
    v_empregado = v_empregado,
    v_empregador = v_empregador
  )
  
}