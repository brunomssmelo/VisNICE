load_xlsx_nice <- function(data_path){
  library(tidyverse)
  library(readxl)
  
  
  # Carrega dados da planilha de informações do NICE ####
  
  
  # Worksheet "Listados" #################################################################################
  
  base_listados <- read_excel(data_path,
                              sheet = "Listados")%>%
    mutate(NUM_CNPJ_CPF = str_pad(NUM_CNPJ_CPF, pad = '0', side = 'left', width = 14))
  
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
    filter(nchar(NUM_CNPJ_CPF)==14) %>%  
    select(id = NUM_CNPJ_CPF, title = NOME, type = NIVEL) %>% 
    filter(!is.na(id)) %>% 
    mutate(group = 'PJ',
           role = 'empresa') %>% 
    arrange(type) %>% 
    filter(!duplicated(id))
  
  v_empresa_socio <- base_socios %>%
    filter(nchar(NUM_CNPJ_EMPRESA)==14) %>%
    select(id = NUM_CNPJ_EMPRESA, title = NOME_EMPRESA, type = NIVEL) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ',
           role = 'empresa') %>%
    filter(!duplicated(id))
   
  
  a_socio <- base_socios %>% 
    select(from = NUM_CPF_CNPJ_SOCIO,
           to = NUM_CNPJ_EMPRESA,
           start = DATA_ENTRADA_SOCIEDADE,
           end = DATA_SAIDA) %>% 
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>% 
    mutate(role = 'socio',
           type = 'sociedade')%>%
    unique()
  
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
  
  a_parente_r <- base_parentesco %>%
    filter(RELACAO %in% c('SOGRO/SOGRA'))%>%
    select(from = CPF1,
           to = CPF2,
           role = RELACAO)%>%
    mutate(type = 'parentesco',
           role = 'sogra')
  
  a_parente <- base_parentesco %>%
    filter(RELACAO %in% c('AVÔ/AVÓ'))%>%
    select(from = CPF1,
           to = CPF2,
           role = RELACAO)%>%
    mutate(type = 'parentesco',
           role = 'avó')
  
  a_parente <- base_parentesco %>%
    filter(!is.na(CPF1)) %>% 
    filter(!is.na(CPF2)) %>% 
    filter(!(CPF1 == CPF2)) %>% 
    filter(!(RELACAO %in% c('AVÔ/AVÓ')))%>%
    filter(!(RELACAO %in% c('SOGRO/SOGRA')))%>%
    select(from = CPF1,
           to = CPF2,
           role = RELACAO) %>%
    mutate(type = 'parentesco',
           role = tolower(role))%>%
    bind_rows(a_parente, a_parente_r)
  
  
  
  # Worksheet "17_Parente_Org_Publico" ############################################################################
  
  base_parente_org_publico <- read_excel(data_path,
                                      sheet = "17_Parente_Org_Publico",
                                       na = 'NULL',
                                       col_types = c(rep("text",7), rep("guess", 3)))%>%
  mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
         CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14))

  v_servidor <- base_parente_org_publico %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'servidor')%>%
    filter(!duplicated(id))

  v_orgao_publico <- base_parente_org_publico %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'OP',
           role = 'orgao_publico')%>%
    filter(!duplicated(id))

  a_vinculo_servidor <- base_parente_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'servidor',
           type = 'vinculo_emp')
  
  # Worksheet "2_Telefones" ############################################################################
  
  base_telefones <- read_excel(data_path,
                                         sheet = "2_Telefones",
                                         na = 'NULL',
                                         col_types = rep("text",6))%>%
    mutate(NUM_CNPJ = str_pad(NUM_CNPJ, pad = '0', side = 'left', width = 14),
           TELEFONE = str_pad(TELEFONE, pad = '0', side = "left", width = 10))

  v_telefones <- base_telefones %>%
    filter(nchar(TELEFONE)<=11) %>%
    select(id = TELEFONE, title = TELEFONE) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'TEL',
           role = 'telefones')%>%
    filter(!duplicated(id))

  v_cnpj <- base_telefones %>%
    filter(nchar(NUM_CNPJ)==14) %>%
    select(id = NUM_CNPJ, title = NOME) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ',
           role = 'empresa')%>%
    filter(!duplicated(id))

  a_tel_empresa <- base_telefones %>%
    select(from = NUM_CNPJ,
           to = TELEFONE)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'telefones',
           type = 'telefone_empresa')
  
  # Worksheet "9_Socio_Parentesco" ############################################################################
  
  base_socio_parentesco <- read_excel(data_path,
                                         sheet = "9_Socio_Parentesco",
                                         na = 'NULL',
                                         col_types = c(rep("text",5), rep("guess", 10)))%>%
    mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
           CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11),
           NUM_CNPJ_EMPRESA1 = str_pad(NUM_CNPJ_EMPRESA1, pad = '0', side = 'left', width = 14),
           NUM_CNPJ_EMPRESA2 = str_pad(NUM_CNPJ_EMPRESA2, pad = '0', side = 'left', width = 14))
  
  v_socio_parentesco <- base_socio_parentesco %>%
    filter(nchar(CPF1) == 11) %>%
    select(id = CPF1, title = NOME_CPF1) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'socio')
  
  v_socio_parentesco <- base_socio_parentesco %>%
    filter(nchar(CPF2) == 11) %>%
    select(id = CPF2, title = NOME_CPF2) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'socio') %>% 
    bind_rows(v_socio_parentesco)%>%
    filter(!duplicated(id))%>%
    mutate(type = 1)
  
  a_socio_parentesco <- base_socio_parentesco %>%
    filter(!is.na(CPF1)) %>% 
    filter(!is.na(CPF2)) %>% 
    select(from = CPF1,
           to = CPF2,
           role = RELACAO) %>%
    mutate(type = 'parentesco',
           role = tolower(role))
  
  # Worksheet "15_Func_Org_Publico" ############################################################################
  
  base_func_org_publico <- read_excel(data_path,
                                      sheet = "15_Func_Org_Publico",
                                      na = 'NULL',
                                      col_types = c(rep("text",7), rep("guess", 3)))%>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14))

  v_servidor_pub <- base_func_org_publico %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = NO_PARTIC_RAIS) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'servidor')%>%
    filter(!duplicated(id))

  v_org_publico <- base_func_org_publico %>%
   select(id = CO_CNPJ_CEI, title = NOME_EMPRESA) %>%
  filter(!is.na(id)) %>%
    mutate(group = 'OP',
           role = 'orgao_publico')%>%
    filter(!duplicated(id))

  a_vinculo_servidor_pub <- base_func_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'servidor',
           type = 'vinculo_emp')
  
  # Worksheet "16_Socios_Org_Publico" ############################################################################
  
  base_socios_org_publico <- read_excel(data_path,
                                      sheet = "16_Socios_Org_Publico",
                                      na = 'NULL',
                                      col_types = c(rep("text",7), rep("guess", 3)))%>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14))

  v_socio_servidor <- base_socios_org_publico %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'servidor')%>%
    filter(!duplicated(id))

  v_socio_org_publico <- base_socios_org_publico %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'OP',
           role = 'orgao_publico')%>%
    filter(!duplicated(id))

  a_vinculo_socio_servidor <- base_socios_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'servidor',
           type = 'vinculo_emp')
  
  ### dados consulta nice  ################################################################################
  
  list(
    ws_listados = base_listados,
    ws_cnpj = base_cnpj,
    ws_socios = base_socios,
    ws_parentesco = base_parentesco,
    ws_base_parente_org_publico = base_parente_org_publico,
    ws_base_telefones = base_telefones,
    ws_base_socio_parentesco = base_socio_parentesco,
    ws_base_func_org_publico = base_func_org_publico,
    ws_base_socios_org_publico = base_socios_org_publico,
    
    a_socio = a_socio,
    a_parente = a_parente,
    a_vinculo_servidor = a_vinculo_servidor,
    a_tel_empresa = a_tel_empresa,
    a_socio_parentesco = a_socio_parentesco,
    a_vinculo_servidor_pub = a_vinculo_servidor_pub,
    a_vinculo_socio_servidor = a_vinculo_socio_servidor,
    
    v_parente = v_parente,
    v_empresa = v_empresa,
    v_empresa_socio = v_empresa_socio,
    v_socio_pj = v_socio_pj,
    v_socio_pf = v_socio_pf,
    v_servidor = v_servidor,
    v_orgao_publico = v_orgao_publico,
    v_telefones = v_telefones,
    v_cnpj = v_cnpj,
    v_socio_parentesco = v_socio_parentesco,
    v_servidor_pub = v_servidor_pub,
    v_org_publico = v_org_publico,
    v_socio_servidor = v_socio_servidor,
    v_socio_org_publico = v_socio_org_publico
  )
  
}