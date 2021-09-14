load_xlsx_nice <- function(data_path){
  library(tidyverse)
  library(readxl)
  
  # Carrega dados da planilha de informações do NICE ####

  # Worksheet "Listados" #################################################################################
  
  ws_listados <- read_excel(data_path,
                            sheet = "Listados")
  
  ws_listados <- ws_listados %>% 
    filter(nchar(NUM_CNPJ_CPF)==14) %>%  
    select(id = NUM_CNPJ_CPF, title = NOME, level = NIVEL) %>% 
    arrange(level) %>% 
    filter(!duplicated(id))
  
  # cria um "vetor nomeado" com o objetivo de recuperar CNPJs a partir dos nomes das empresas
  # obs.: isto é só um bacalhau para solucionar parcialmente o fato de a aba "3_Socio" estar trazendo NULL
  # para os sócios do tipo PJ
  company_name_to_id <- ws_listados$id %>% 
    `names<-`(ws_listados$title)
  
  # Worksheet "1_CNPJ" ###################################################################################
  
  ws_cnpj <- read_excel(data_path,
                        sheet = "1_CNPJ",
                        col_types = c("text", rep("guess", 25)))
  
  v_cnpj <- ws_cnpj %>% 
    select(id = NUM_CNPJ, title = NOME) %>% 
    left_join(select(ws_listados, id, level), by='id') %>% 
    filter(nchar(id)==14, !duplicated(id)) %>% 
    mutate(group = 'PJ_PRIVADO')
  
  vertices <- v_cnpj
  
  
  # Worksheet "3_Socio" ##################################################################################
  
  ws_socios <- read_excel(data_path,
                          sheet = "3_Socio",
                          na = 'NULL',
                          col_types = c("text", "guess", "text", rep("guess", 5))) %>% 
    mutate(NUM_CNPJ_EMPRESA = str_pad(NUM_CNPJ_EMPRESA, pad = '0', side = 'left', width = 14)) %>% 
    mutate(NUM_CPF_CNPJ_SOCIO = case_when(
      is.na(NUM_CPF_CNPJ_SOCIO) ~ company_name_to_id[NOME_SOCIO], 
      nchar(NUM_CPF_CNPJ_SOCIO) > 11 ~ str_pad(NUM_CPF_CNPJ_SOCIO, pad = '0', side = 'left', width = 14),
      T ~ str_pad(NUM_CPF_CNPJ_SOCIO, pad = '0', side = 'left', width = 11)))
  
  a_socio <- ws_socios %>% 
    select(from = NUM_CPF_CNPJ_SOCIO,
           to = NUM_CNPJ_EMPRESA,
           start = DATA_ENTRADA_SOCIEDADE,
           end = DATA_SAIDA) %>% 
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>% 
    mutate(role = 'socio',
           type = 'sociedade')%>%
    unique()
  
  v_socio_pf <- ws_socios %>% 
    select(id = NUM_CPF_CNPJ_SOCIO, title = NOME_SOCIO) %>% 
    filter(nchar(id)==11) %>% 
    mutate(group = 'PF') %>% 
    inner_join(select(a_socio, from:to), by=c('id'='from')) %>% 
    left_join(select(ws_listados, id, level), by=c('to'='id')) %>% 
    select(-to) %>% 
    filter(!duplicated(id))
  
  v_socio_pj <- ws_socios %>% 
    select(id = NUM_CPF_CNPJ_SOCIO, title = NOME_SOCIO) %>% 
    left_join(select(ws_listados, id, level), by='id') %>% 
    filter(nchar(id)==14, !duplicated(id)) %>% 
    mutate(group = 'PJ_PRIVADO')
  
  vertices <- vertices %>%
    bind_rows(v_socio_pf) %>% 
    bind_rows(v_socio_pj) %>% 
    arrange(id) %>% 
    filter(!duplicated(id))
  
  # Worksheet "8_Parentesco" #############################################################################
  
  ws_parentesco <- read_excel(data_path,
                              sheet = "8_Parentesco",
                              na = 'NULL',
                              col_types = 'text') %>% 
    mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
           CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11))
  
  a_parente <- ws_parentesco %>%
    mutate(start = NA, end = NA) %>% 
    filter(!is.na(CPF1)) %>% 
    filter(!is.na(CPF2)) %>% 
    filter(!(CPF1 == CPF2)) %>% 
    # filter(!(RELACAO %in% c('AVÔ/AVÓ')))%>%
    # filter(!(RELACAO %in% c('SOGRO/SOGRA')))%>%
    select(from = CPF1,
           to = CPF2,
           start, end,
           role = RELACAO) %>%
    mutate(type = 'parentesco',
           role = tolower(role))
    # bind_rows(a_parente, a_parente_r)
  
  
  
  v_parente <- ws_parentesco %>%
    select(id = CPF2, title = NOME2) %>%
    filter(!is.na(id)) %>% 
    mutate(group = 'PF')
  
  v_parente <- ws_parentesco %>%
    select(id = CPF1, title = NOME1) %>%
    filter(!is.na(id)) %>% 
    mutate(group = 'PF') %>%
    bind_rows(v_parente) %>% 
    filter(!duplicated(id)) %>% 
    left_join(select(vertices, id, level), by='id')
  
  vertices <- vertices %>%
    bind_rows(v_parente) %>% 
    arrange(id) %>% 
    filter(!duplicated(id))

  # Worksheet "17_Parente_Org_Publico" ############################################################################
  
  ws_parente_org_publico <- read_excel(data_path,
                                       sheet = "17_Parente_Org_Publico",
                                       na = 'NULL',
                                       col_types = c(rep("text",7), rep("guess", 3)))%>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14)) %>% 
    mutate(TIPO_VINCULO = case_when(
      str_detect(TIPO, pattern = 'não-efetivo') ~ 'servidor não efetivo',
      str_detect(TIPO, pattern = 'regime jurídico único') ~ 'servidor efetivo',
      str_detect(TIPO, pattern = 'indeterminado') ~ 'servidor contrato prazo indeterminado',
      str_detect(TIPO, pattern = ' determinado') ~ 'servidor contrato prazo determinado',
      str_detect(TIPO, pattern = 'temporário') ~ 'servidor contrato temporário',
      T ~ 'servidor nomeado sem vínculo empregatício',
    ))
  
  v_servidor <- ws_parente_org_publico %>%
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id), nchar(id)==11, !duplicated(id)) %>% 
    mutate(group = 'PF') %>%
    left_join(select(vertices, id, level), by='id')
  
  v_orgao_publico <- ws_parente_org_publico %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id), !duplicated(id)) %>% 
    mutate(group = 'PJ_PUBLICO') %>%
    left_join(select(vertices, id, level), by='id')
  
  a_parente_servidor <- ws_parente_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM,
           role = TIPO_VINCULO) %>% 
    mutate(type = 'vinculo_empregaticio') %>%
    filter(!is.na(from), !is.na(to))
  
  vertices <- vertices %>%
    bind_rows(v_servidor) %>% 
    bind_rows(v_orgao_publico)
  
  # Worksheet "2_Telefones" ############################################################################
  
  ws_telefones <- read_excel(data_path,
                                         sheet = "2_Telefones",
                                         na = 'NULL',
                                         col_types = rep("text",6))%>%
    mutate(NUM_CNPJ = str_pad(NUM_CNPJ, pad = '0', side = 'left', width = 14),
           TELEFONE = str_pad(TELEFONE, pad = '0', side = "left", width = 10))

  v_telefones <- ws_telefones %>%
    filter(nchar(TELEFONE)<=11) %>%
    select(id = TELEFONE, title = TELEFONE) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'TEL',
           role = 'telefones')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id, level), by='id')

  v_cnpj <- ws_telefones %>%
    filter(nchar(NUM_CNPJ)==14) %>%
    select(id = NUM_CNPJ, title = NOME) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ_PRIVADO',
           role = 'empresa')%>%
    filter(!duplicated(id))%>%
  left_join(select(vertices, id, level), by='id')

  a_tel_empresa <- ws_telefones %>%
    mutate(start = NA, end = NA) %>% 
    select(from = NUM_CNPJ,
           to = TELEFONE,
           start, end)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'telefones',
           type = 'telefone_empresa')
  
  vertices <- vertices %>%
    bind_rows(v_telefones) %>% 
    bind_rows(v_cnpj)
  
  # Worksheet "9_Socio_Parentesco" ############################################################################
  
  ws_socio_parentesco <- read_excel(data_path,
                                      sheet = "9_Socio_Parentesco",
                                      na = 'NULL',
                                      col_types = c(rep("text",5), rep("guess", 10)))%>%
    mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
           CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11),
           NUM_CNPJ_EMPRESA1 = str_pad(NUM_CNPJ_EMPRESA1, pad = '0', side = 'left', width = 14),
           NUM_CNPJ_EMPRESA2 = str_pad(NUM_CNPJ_EMPRESA2, pad = '0', side = 'left', width = 14))

  v_socio_parentesco <- ws_socio_parentesco %>%
    filter(nchar(CPF1) == 11) %>%
    select(id = CPF1, title = NOME_CPF1) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'socio')

  v_socio_parentesco <- ws_socio_parentesco %>%
    filter(nchar(CPF2) == 11) %>%
    select(id = CPF2, title = NOME_CPF2) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF',
           role = 'socio') %>%
    bind_rows(v_socio_parentesco)%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id, level), by='id')

  a_socio_parentesco <- ws_socio_parentesco %>%
    mutate(start = NA, end = NA) %>%
    filter(!is.na(CPF1)) %>%
    filter(!is.na(CPF2)) %>%
    filter(!(CPF1 == CPF2)) %>% 
    select(from = CPF1,
           to = CPF2,
           start,end,
           role = RELACAO) %>%
    mutate(type = 'parentesco',
           role = tolower(role))
  
  vertices <- vertices %>%
    bind_rows(v_socio_parentesco) %>% 
    arrange(id) %>% 
    filter(!duplicated(id))
  
  # Worksheet "15_Func_Org_Publico" ############################################################################
  
  ws_func_org_publico <- read_excel(data_path,
                                      sheet = "15_Func_Org_Publico",
                                      na = 'NULL',
                                      col_types = c(rep("text",7), rep("guess", 3)))%>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14))%>%
    mutate(TIPO_VINCULO = case_when(
    str_detect(TIPO, pattern = 'não-efetivo') ~ 'servidor não efetivo',
    str_detect(TIPO, pattern = 'regime jurídico único') ~ 'servidor efetivo',
    str_detect(TIPO, pattern = 'indeterminado') ~ 'servidor contrato prazo indeterminado',
    str_detect(TIPO, pattern = ' determinado') ~ 'servidor contrato prazo determinado',
    str_detect(TIPO, pattern = 'temporário') ~ 'servidor contrato temporário',
    str_detect(TIPO, pattern = 'Aprendiz') ~ 'aprendiz contratado',
    T ~ 'servidor nomeado sem vínculo empregatício',
  ))
  
  v_servidor_pub <- ws_func_org_publico %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = NO_PARTIC_RAIS) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF')%>%
    filter(!duplicated(id)) %>%
    left_join(select(vertices, id, level), by='id')
  
  v_org_publico <- ws_func_org_publico %>%
    select(id = CO_CNPJ_CEI, title = NOME_EMPRESA) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ_PUBLICO')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id, level), by='id')
  
  a_vinculo_servidor_pub <- ws_func_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM,
           role = TIPO_VINCULO)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(type = 'vinculo_empregaticio')%>%
    filter(!is.na(from), !is.na(to))
  
  vertices <- vertices %>%
    bind_rows(v_servidor_pub) %>%
    bind_rows(v_org_publico)
  
  # Worksheet "16_Socios_Org_Publico" ############################################################################
  
  ws_socios_org_publico <- read_excel(data_path,
                                        sheet = "16_Socios_Org_Publico",
                                        na = 'NULL',
                                        col_types = c(rep("text",7), rep("guess", 3)))%>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14))%>%
  mutate(TIPO_VINCULO = case_when(
    str_detect(TIPO, pattern = 'não-efetivo') ~ 'servidor não efetivo',
    str_detect(TIPO, pattern = 'regime jurídico único') ~ 'servidor efetivo',
    str_detect(TIPO, pattern = 'indeterminado') ~ 'servidor contrato prazo indeterminado',
    str_detect(TIPO, pattern = ' determinado') ~ 'servidor contrato prazo determinado',
    str_detect(TIPO, pattern = 'temporário') ~ 'servidor contrato temporário',
    T ~ 'servidor nomeado sem vínculo empregatício',
  ))

  v_socio_servidor <- ws_socios_org_publico %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id, level), by='id')

  v_socio_org_publico <- ws_socios_org_publico %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ_PUBLICO')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id, level), by='id')

  a_vinculo_socio_servidor <- ws_socios_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM,
           role = TIPO_VINCULO)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(type = 'vinculo_empregaticio')%>%
    filter(!is.na(from), !is.na(to))

  vertices <- vertices %>%
    bind_rows(v_socio_servidor) %>%
    bind_rows(v_socio_org_publico)
  
  # neste momento, podemos verificar se algum PJ inicialmente definido como privado era, na realidade
  # publico, e eliminar as diplicatas que estavam considerando-os como PJ provados:
  vertices <- vertices %>% 
    arrange(match(group, c('PF', 'PJ_PUBLICO', 'PJ_PRIVADO'))) %>% 
    filter(!duplicated(id)) # <----- neste filtro prevalecerão os PJs publicos sobre os PJs privados
  

  ### dados consulta nice  ################################################################################
  
  # consolida as arestas e remove eventuais duplicatas e arestas quebradas
  arestas <- a_socio %>% 
    bind_rows(a_parente) %>% 
    bind_rows(a_parente_servidor) %>% 
    bind_rows(a_tel_empresa) %>%
    bind_rows(a_socio_parentesco) %>%
    bind_rows(a_vinculo_servidor_pub) %>%
    bind_rows(a_vinculo_socio_servidor) %>%
    filter(from %in% vertices$id, to %in% vertices$id) %>% 
    unique()
  
  # remove eventuais vertices isolados
  vertices <- vertices %>% 
    filter(id %in% arestas$from | id %in% arestas$to)
  
  list(
    # ws_listados = ws_listados,
    # ws_cnpj = ws_cnpj,
    # ws_socios = ws_socios,
    # ws_parentesco = ws_parentesco,
    # ws_parente_org_publico = ws_parente_org_publico,
    
    edges = arestas,
    
    nodes = vertices
  )
  
}

check_filter_interval <- function(edge_data, filter_start, filter_end, filter_type){
  
  edge_data <- edge_data %>%
    mutate(
      start = if_else(is.na(start), true = as.character(filter_start), false = start),
      end = if_else(is.na(end),
                    true = as.character(filter_end), false = end),
      end = if_else((end=='Tempo indefinido'),
                    true = as.character(filter_end), false = end)) %>%
    mutate(
      ok = case_when(
        ((type == filter_type) & (parse_date(start)>filter_end)) ~ F,
        ((type == filter_type) & (parse_date(end)<filter_start)) ~ F,
        T ~ T))
  
  edge_data$ok
}