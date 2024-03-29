load_From_xlsx <- function(data_path){
  library(tidyverse)
  library(readxl)
  
  # Worksheet cnpjs ###################################################################################
  
  ws_cnpj <- read_excel(data_path,
                        sheet = "cnpjs",
                        col_types = c("text", rep("guess", 25)))
  
  # Worksheet socios ##################################################################################
  ws_socios <- read_excel(data_path,
                       sheet = "socios",
                       col_types = c(rep("text",6), rep("guess", 3)))
  
  # Worksheet empenhos ##################################################################################
  
  ws_empenhos <- read_excel(data_path,
                            sheet = "empenhos",
                            na = "NULL",
                            col_types = rep("guess", 26))
  
  # Worksheet parentesco ############################################################################
  
  ws_parentesco <- read_excel(data_path,
                              sheet = "parentesco",
                              na = "NULL",
                              col_types = rep("text",6))
  
  # Worksheet telefones ############################################################################
  
  ws_telefones <- read_excel(data_path,
                             sheet = "telefones",
                             na = "NULL",
                             col_types = rep("text",4))
  
  # Worksheet func_na_adm_publica ############################################################################
  
  ws_func_na_adm_publica <- read_excel(data_path,
                                       sheet = "func_na_adm_publica",
                                       na = "NULL",
                                       col_types = c(rep("text",7), rep("guess", 3)))
  
  # Worksheet sancionados ############################################################################
  ws_sancionado <- read_excel(data_path,
                              sheet = "sancionados",
                              na = "NULL",
                              col_types = rep("text",27))
  
  # Worksheet socio_org_publico ############################################################################
  ws_socio_org_publico <- read_excel(data_path,
                              sheet = "socio_org_publico",
                              na = "NULL",
                              col_types = c(rep("text",8), rep("guess",2)))
  
  # Retorna lista com as tabelas já convertidas do encoding nativo do Excel (latin1) para UTF8 -----------
  list(
    cnpjs = Corrige_Codificacao_Dataframe(ws_cnpj),
    socios = Corrige_Codificacao_Dataframe(ws_socios),
    empenhos = Corrige_Codificacao_Dataframe(ws_empenhos),
    parentesco = Corrige_Codificacao_Dataframe(ws_parentesco),
    telefones = Corrige_Codificacao_Dataframe(ws_telefones),
    socio_org_publico = Corrige_Codificacao_Dataframe(ws_socio_org_publico),
    func_na_adm_publica = Corrige_Codificacao_Dataframe(ws_func_na_adm_publica),
    sancionados = Corrige_Codificacao_Dataframe(ws_sancionado) 
    #socio_parentesco <- Corrige_Codificacao_Dataframe(ws_socio_parentesco)
  )
  
}


load_from_sgbd <- function(cnpj){
  library(stringi)
  library(readxl)
  library(dplyr)
  # Abertura de conexao com o SQL SERVER -----------------------------------------------------------------
  tryCatch({
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "TCERJVM48",
                     Trusted_Connection = "True",
                     database ="DGI_CONSULTA" )
    message("Conectado")
  }, error = function(e){
    message("Nao foi possivel conectar ao Banco")
  })
  
  # Inicia Transação como o SQL SERVER -------------------------------------------------------------------
  DBI::dbBegin(con)
  
  # Criação a tabela temporária #CNPJ_RAIZ  --------------------------------------------------------------
  
  dbExecute(con, "CREATE TABLE #CNPJ_RAIZ (CNPJ VARCHAR(20))", immediate = TRUE)
  
  if(is.null(cnpj)){
    cnpj <- c('09060537', '02812740', '25195543', '07028841','05769219')
  }
  
  # Retira espaços das strings contidas em cnpj
  cnpj <- str_squish(cnpj)
  
  # Extrai apenas os primeiros 8 numeros do cnpj
  cnpj <- unlist(str_extract_all(cnpj,"[0-9]{8}")) 
  
  # Cria script de inserção dos cnpjs investigados na tabela temporária #CNPJ_RAIZ
  script_cnpj <- paste0("INSERT INTO #CNPJ_RAIZ VALUES ", paste0("('", cnpj, "')", collapse = ','))
  
  
  dbExecute(con, script_cnpj)
  
  ### criando a tabela temporária #CNPJ
  dbExecute(con,"CREATE TABLE #CNPJ (CNPJ VARCHAR(20))", immediate = TRUE)
  
  dbExecute(con,"INSERT INTO #CNPJ SELECT NUM_CNPJ
               FROM DGI_CONSULTA.IMPORTADO.CNPJ
               WHERE NUM_CNPJ_RAIZ IN (SELECT CNPJ FROM #CNPJ_RAIZ)")
  
  
  # 01 - CNPJ - (CADASTRO NACIONAL DE PESSOA JURIDICA) / CNPJ - RELACAO DE MATRIZ E FILIAL  --------------
  
  cnpj_sql <- read_file("./src/sql/ConsultaAbaCnpj.sql")
  
  # 02 - CNPJ - TELEFONES --------------------------------------------------------------------------------
  
  telefones_sql <- read_file("./src/sql/ConsultaAbaTelefones.sql")
  
  # 03 - CNPJ - SOCIO(S), REPRESENTANTE(S) E PARTICIPACOES SOCIETARIAS  ----------------------------------
  
  socio_sql <- read_file("./src/sql/ConsultaAbaSocios.sql")
  
  # 08 - CPF - RELACAO PARENTESCO ------------------------------------------------------------------------
  
  parentesco_sql <- read_file("./src/sql/ConsultaAbaParentesco.sql")
  
  # 09 - CNPJ - RELACAO SOCIO-PARENTESCO  ----------------------------------------------------------------
  
  SocioParentesco_sql <- read_file("./src/sql/ConsultaAbaSocioParente.sql")
  
  # 10 - RAIS - CAPACIDADE OPERACIONAL ------------------------------------------
  
  contagem_rais_sql <- read_file("./src/sql/ConsultaAbaRais.sql")
  
  # 12 - CAGED - CAPACIDADE OPERACIONAL ------------------------------------------
  
  contagem_caged_sql <- read_file("./src/sql/ConsultaAbaCaged.sql")
  
  # 17 - CPF - FUNCIONARIOS EMPREGADOS NA ADMINISTRACAO PUBLICA ------------------------------------------
  
  funcionariosNaAdmPublica_sql <- read_file("./src/sql/ConsultaAbaFuncionariosServidores.sql")
  
  # 18 - CPF - SOCIOS EMPREGADOS NA ADMINISTRACAO PUBLICA ------------------------------------------------
  
  cpfSociosNaAdmPublicaParte1_sql <- read_file("./src/sql/ConsultaAbaSociosServidoresParte1.sql")
  cpfSociosNaAdmPublicaParte2_sql <- read_file("./src/sql/ConsultaAbaSociosServidoresParte2.sql")
  
  # XX - EMPENHOS ----------------------------------------------------------------------------------------
  empenhos_sql <- read_file("./src/sql/ConsultaAbaEmpenhos.sql")
  
  # XX - SANCIONADOS -------------------------------------------------------------------------------------
  sancionado_sql <- read_file("./src/sql/ConsultaAbaSancionados.sql")
  
  # Execução das Consultas -------------------------------------------------------------------------------
  
  cnpjs <- dbGetQuery(con, stri_enc_tonative(cnpj_sql))
  
  telefones <- dbGetQuery(con, stri_enc_tonative(telefones_sql))
  
  socios <- dbGetQuery(con, stri_enc_tonative(socio_sql))
  
  parentesco <- dbGetQuery(con, stri_enc_tonative(parentesco_sql))
  
  func_na_adm_publica <- dbGetQuery(con, stri_enc_tonative(funcionariosNaAdmPublica_sql))
  
  capac_operac_rais <- dbGetQuery(con, stri_enc_tonative(contagem_rais_sql))
  
  capac_operac_caged <- dbGetQuery(con, stri_enc_tonative(contagem_caged_sql))
  
  dbExecute(con, stri_enc_tonative(cpfSociosNaAdmPublicaParte1_sql), immediate = TRUE)
  socio_org_publico <- dbGetQuery(con, stri_enc_tonative(cpfSociosNaAdmPublicaParte2_sql))
  
  #socio_parentesco <- dbGetQuery(con, stri_enc_tonative(SocioParentesco_sql))
  
  empenhos <- dbGetQuery(con, stri_enc_tonative(empenhos_sql))
  
  sancionados <- dbGetQuery(con, stri_enc_tonative(sancionado_sql))
  
  # Fechamento da conexão com o SQL SERVER ---------------------------------------------------------------
  DBI::dbDisconnect(con) 
  
  # Armazena os dados do banco em uma planilha excel ------------------------------------------------------
  # Tratar caso ja exista um arquivo desse salvo
  path <- "./dados/"
  write_xlsx(
    list(cnpjs = Corrige_Codificacao_Dataframe(cnpjs), 
         empenhos = Corrige_Codificacao_Dataframe(empenhos), 
         parentesco = Corrige_Codificacao_Dataframe(parentesco),
         socios = Corrige_Codificacao_Dataframe(socios), 
         socio_org_publico = Corrige_Codificacao_Dataframe(socio_org_publico),
         func_na_adm_publica = Corrige_Codificacao_Dataframe(func_na_adm_publica), 
         telefones = Corrige_Codificacao_Dataframe(telefones),
         sancionados = Corrige_Codificacao_Dataframe(sancionados)),
    path = paste(path,"dados_grafos_sql",Sys.Date(), ".xlsx"),
    col_names = TRUE,
    format_headers = TRUE,
  )
  
  # Une as tabelas com capacidade operacional -------------------------------------------------------------
  capac_operac <- full_join(capac_operac_rais, capac_operac_caged)
  
  # Retorna lista com as tabelas já convertidas do encoding nativo do SQL Server (latin1) para UTF8 ------
  list(
    parentesco = Corrige_Codificacao_Dataframe(parentesco),
    empenhos = Corrige_Codificacao_Dataframe(empenhos),
    socios = Corrige_Codificacao_Dataframe(socios),
    socio_org_publico = Corrige_Codificacao_Dataframe(socio_org_publico),
    cnpjs = Corrige_Codificacao_Dataframe(cnpjs),
    func_na_adm_publica = Corrige_Codificacao_Dataframe(func_na_adm_publica),
    sancionados = Corrige_Codificacao_Dataframe(sancionados),
    telefones = Corrige_Codificacao_Dataframe(telefones)
    #capac_operac = Corrige_Codificacao_Dataframe(capac_operac)
    #socio_parentesco <- Corrige_Codificacao_Dataframe(socio_parentesco)
  )
}

load_data <- function(data_source){
  library(tidyverse)
  library(dplyr)
  library(stringi)
  library(stringr)
  library(formattable)
  library(readxl)
  library(odbc)
  library(DBI)
  
  # melhorar isso aqui com checagens e tratamento de exceções
  if (is.null(data_source) || !file.exists(data_source)[1]) {
    data <- load_from_sgbd(data_source)
  }else{
    data <- load_From_xlsx(data_source)
  }
  
  # Tabela "cnpj" ------------------------------------------------------------------------------------------
  cnpjs <- data$cnpjs
  
  cnpjs <- cnpjs %>%
    mutate(NUM_DDD1 = as.character(NUM_DDD1), NUM_TELEFONE1 = as.character(NUM_TELEFONE1),
           NUM_DDD2 = as.character(NUM_DDD2), NUM_TELEFONE2 = as.character(NUM_TELEFONE2)) %>%
    mutate(
      TEL1 = case_when(
        !is.na(NUM_TELEFONE1) ~ paste(NUM_DDD1, NUM_TELEFONE1)), 
      TEL2 = case_when(
        !is.na(NUM_TELEFONE2) ~ paste(NUM_DDD2, NUM_TELEFONE2)))
  
  v_cnpj <- cnpjs %>%
    select(id = NUM_CNPJ, title = NOME) %>%
    filter(nchar(id)==14, !duplicated(id)) %>%
    mutate(group = 'PJ_PRIVADO')
  
  v_telefones <- cnpjs %>%
    select(id = TEL2, title = TEL2) %>%
    filter(nchar(id)<=11) %>%
    mutate(group = 'TEL',
           role = 'telefones')
  
  v_telefones <- cnpjs %>%
    select(id = TEL1, title = TEL1) %>%
    filter(nchar(id)<=11) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'TEL',
           role = 'telefones') %>%
    bind_rows(v_telefones) %>%
    filter(!duplicated(id))
  
  a_tel_cnpj <- cnpjs %>%
    mutate(start = NA, end = NA) %>%
    select(from = NUM_CNPJ,
           to = TEL1,
           start, end) %>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'telefones',
           type = 'telefone_empresa')
  
  
  vertices <- v_cnpj %>%
    bind_rows(v_telefones)
  
  # Tabela "telefones" -------------------------------------------------------------------------------------
  
  telefones <- data$telefones
  
  v_tel <- telefones %>%
    filter(nchar(TELEFONE)<=11) %>%
    select(id = TELEFONE, title = TELEFONE) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'TEL',
           role = 'telefones')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  v_cnpj_tel <- telefones %>%
    filter(nchar(NUM_CNPJ)==14) %>%
    select(id = NUM_CNPJ, title = NOME) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ_PRIVADO',
           role = 'empresa')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  a_tel_empresa <- telefones %>%
    mutate(start = NA, end = NA) %>%
    select(from = NUM_CNPJ,
           to = TELEFONE,
           start, end)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'telefones',
           type = 'telefone_empresa')
  
  vertices <- vertices %>%
    bind_rows(v_tel) %>%
    bind_rows(v_cnpj_tel)
  
  # Tabela "socios" ----------------------------------------------------------------------------------------
  
  socios <- data$socios
  
  company_name_to_id <- socios$NUM_CNPJ_EMPRESA %>% 
    `names<-`(socios$NOME_SOCIO)
  
  socios <- socios %>% 
    mutate(NUM_CNPJ_EMPRESA = str_pad(NUM_CNPJ_EMPRESA, pad = '0', side = 'left', width = 14)) %>% 
    mutate(NUM_CPF = case_when(
      nchar(NUM_CPF) > 11 ~ str_pad(NUM_CPF, pad = '0', side = 'left', width = 14),
      is.na(NUM_CPF) ~ company_name_to_id[NOME_SOCIO],
      T ~ str_pad(NUM_CPF, pad = '0', side = 'left', width = 11)))
  
  a_socio <- socios %>% 
    select(from = NUM_CPF,
           to = NUM_CNPJ_EMPRESA,
           start = DATA_ENTRADA_SOCIEDADE,
           end = DATA_DE_EXCLUSAO_NA_SOCIEDADE) %>% 
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>% 
    mutate(role = 'socio',
           type = 'sociedade')
  
  v_socio_pf <- socios %>% 
    select(id = NUM_CPF, title = NOME_SOCIO) %>% 
    filter(nchar(id)==11) %>%
    mutate(group = 'PF') %>% 
    inner_join(select(a_socio, from:to), by=c('id'='from')) %>% 
    select(-to) %>% 
    filter(!duplicated(id)) %>%
    left_join(select(vertices, id), by='id')
  
  v_socio_pj <- socios %>% 
    select(id = NUM_CPF, title = NOME_SOCIO) %>% 
    filter(nchar(id)==14, !duplicated(id)) %>% 
    mutate(group = 'PJ_PRIVADO') %>%
    left_join(select(vertices, id), by='id')
  
  vertices <- vertices %>%
    bind_rows(v_socio_pf) %>% 
    bind_rows(v_socio_pj) %>% 
    arrange(id) %>% 
    filter(!duplicated(id))
  
  # Tabela "parentesco" ------------------------------------------------------------------------------------
  
  parentesco <- data$parentesco
  
  parentesco <- parentesco %>% 
    mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
           CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11)) %>%
    mutate(RELACAO = case_when(
      str_detect(RELACAO,'AVÓ')~'AVÓ/AVÔ',
      str_detect(RELACAO,'AVÔ')~'AVÓ/AVÔ',
      str_detect(RELACAO,'SOGRO')~'SOGRO/SOGRA',
      str_detect(RELACAO,'SOGRA')~'SOGRO/SOGRA',
      str_detect(RELACAO,'TIO')~'TIO/TIA',
      str_detect(RELACAO,'TIA')~'TIO/TIA',
      str_detect(RELACAO,'PAI')~'PAI/MÃE',
      str_detect(RELACAO,'MÃE')~'PAI/MÃE',
      str_detect(RELACAO,'IRMÃ')~'IRMÃ/IRMÃO',
      str_detect(RELACAO,'IRMÃO')~'IRMÃ/IRMÃO',
      str_detect(RELACAO,'CUNHADA')~'CUNHADA/CUNHADO',
      str_detect(RELACAO,'CUNHADO')~'CUNHADA/CUNHADO',
      str_detect(RELACAO,'PRIMA')~'PRIMA/PRIMO',
      str_detect(RELACAO,'PRIMO')~'PRIMA/PRIMO',
      T~RELACAO
    ))
  
  a_parente <- parentesco %>%
    mutate(start = NA, end = NA) %>%
    filter(!is.na(CPF1)) %>%
    filter(!is.na(CPF2)) %>%
    filter(!(CPF1 == CPF2)) %>%
    # filter(!(RELACAO %in% c('AVO/AVO')))%>%
    # filter(!(RELACAO %in% c('SOGRO/SOGRA')))%>%
    select(from = CPF1,
           to = CPF2,
           start, end,
           role = RELACAO) %>%
    mutate(type = 'parentesco',
           role = tolower(role))
  #bind_rows(a_parente, a_parente_r)
  
  v_parente <- parentesco %>%
    select(id = CPF2, title = NOME2) %>%
    filter(!is.na(id)) %>% 
    mutate(group = 'PF')
  
  v_parente <- parentesco %>%
    select(id = CPF1, title = NOME1) %>%
    filter(!is.na(id)) %>% 
    mutate(group = 'PF') %>%
    bind_rows(v_parente) %>% 
    filter(!duplicated(id)) %>% 
    left_join(select(vertices, id), by='id')
  
  vertices <- vertices %>%
    bind_rows(v_parente) %>% 
    arrange(id) %>% 
    filter(!duplicated(id))
  
  # Tabela "func_na_adm_publica" ---------------------------------------------------------------------------
  
  func_na_adm_publica <- data$func_na_adm_publica
  
  func_na_adm_publica <- func_na_adm_publica %>%
    mutate(CO_CPF = str_pad(CO_CPF, pad = '0', side = 'left', width = 11),
           CO_CNPJ_CEI = str_pad(CO_CNPJ_CEI, pad = '0', side = 'left', width = 14)) %>%
    mutate(TIPO_VINCULO = case_when(
      str_detect(TIPO, pattern = 'não-efetivo') ~ 'servidor não efetivo',
      str_detect(TIPO, pattern = 'regime jurídico único') ~ 'servidor efetivo',
      str_detect(TIPO, pattern = 'indeterminado') ~ 'servidor contrato prazo indeterminado',
      str_detect(TIPO, pattern = ' determinado') ~ 'servidor contrato prazo determinado',
      str_detect(TIPO, pattern = 'temporário') ~ 'servidor contrato temporário',
      str_detect(TIPO, pattern = 'Aprendiz') ~ 'aprendiz contratado',
      T ~ 'servidor nomeado sem vínculo empregatício',
    ))
  
  v_servidor_pub <- func_na_adm_publica %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF')%>%
    filter(!duplicated(id)) %>%
    left_join(select(vertices, id), by='id')
  
  v_org_publico <- func_na_adm_publica %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ_PUBLICO') %>%
    filter(!duplicated(id)) %>%
    left_join(select(vertices, id), by='id')
  
  a_vinculo_servidor_pub <- func_na_adm_publica %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM,
           role = TIPO_VINCULO) %>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(type = 'vinculo_empregaticio')
  
  vertices <- vertices %>%
    bind_rows(v_servidor_pub) %>%
    bind_rows(v_org_publico)
  
  # Tabela "socio_org_publico" ------------------------------------------------------------------------------
  
  socio_org_publico <- data$socio_org_publico
  
  socio_org_publico <- socio_org_publico %>%
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
  
  v_socio_servidor <- socio_org_publico %>%
    filter(nchar(CO_CPF)==11) %>%
    select(id = CO_CPF, title = EMPREGADO) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PF')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  v_socio_org_publico <- socio_org_publico %>%
    select(id = CO_CNPJ_CEI, title = EMPREGADOR) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'PJ_PUBLICO')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  a_vinculo_socio_servidor <- socio_org_publico %>%
    select(from = CO_CPF,
           to = CO_CNPJ_CEI,
           start = DA_ADMISSAO_RAIS_DMA,
           end = DA_DESLIGAMENTO_RAIS_DM,
           role = TIPO_VINCULO)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(type = 'vinculo_empregaticio')
  
  vertices <- vertices %>%
    bind_rows(v_socio_servidor) %>%
    bind_rows(v_socio_org_publico)
  
  # Tabela "empenhos" ---------------------------------------------------------------------------------------
  
  empenhos <- data$empenhos
  
  empenhos <- empenhos %>% 
    mutate(CpfCnpjCredor= str_pad(CpfCnpjCredor, pad = '0', side = 'left', width = 14) )%>%
    mutate(CpfCnpjCredor = case_when(
      nchar(CpfCnpjCredor) > 11 ~ str_pad(CpfCnpjCredor, pad = '0', side = 'left', width = 14),
      T ~ str_pad(CpfCnpjCredor, pad = '0', side = 'left', width = 11))) %>%
    filter(TotalValorPago > 0)
  
  a_empenho <- empenhos %>%
    mutate(start = DataInicio, end = DataFim) %>%
    select(from = CnpjUnidadeGestora,
           to = CpfCnpjCredor,
           start, 
           end,
           valor = TotalValorPago,
           qntd = QntdEmpenhos) %>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    group_by(from, to) %>% 
    summarise(valor_total = sum(valor, na.rm = T), qntd, start,end) %>% 
    ungroup() %>% 
    mutate(role = paste0('[', qntd, '] - ',
                         as.character(currency(valor_total,
                                               symbol = "R$ ",
                                               big.mark = ".")))) %>% 
    select(-valor_total, -qntd) %>% 
    mutate(type = 'empenho')
  
  
  v_cnpj_credor <- empenhos %>%
    select(id = CpfCnpjCredor, title = Credor) %>%
    filter(!is.na(id)) %>%
    mutate(group = "PJ_PRIVADO") %>%
    filter(!duplicated(id)) %>%
    left_join(select(vertices, id), by ='id')
  
  v_unidade_gestora <- empenhos %>%
    select(id = CnpjUnidadeGestora, title = UnidadeGestora) %>%
    filter(!is.na(id)) %>%
    mutate(group = "PJ_PUBLICO")%>%
    filter(!duplicated(id)) %>%
    left_join(select(vertices,id), by = 'id')
  
  vertices <- vertices %>%
    bind_rows(v_cnpj_credor) %>%
    bind_rows(v_unidade_gestora)
  
  # Tabela "sancionados" ---------------------------------------------------------------------------------------
  
  sancionados <- data$sancionados
  
  sancionados <- sancionados %>% mutate(TipoPessoa = case_when(
    str_detect(TipoPessoa, 'J')~"PJ_PRIVADO",
    str_detect(TipoPessoa,'F')~"PF",
    T~"PJ"
  )) %>% mutate(CpfCnpjSancionado = as.character( CpfCnpjSancionado ))
  
  # v_empresa_sancionada <- sancionados %>%
  #   select(id = CpfCnpjSancionado, title = NomeOrgaoSancionador) %>%
  #   filter(!is.na(id)) %>%
  #   mutate(group = "PJ_PRIVADO") %>%
  #   filter(!duplicated(id)) %>%
  #   left_join(select(vertices, id), by ='id')
  # 
  # vertices <- vertices %>%
  #   bind_rows(v_empresa_sancionada)
  
  # a_sancionados <- sancionados %>%
  #   mutate(start = NA, end = NA) %>%
  #   select(from = CnpjUnidadeGestora,
  #          to = C,
  #          start, 
  #          end,
  #          valor = TotalValorPago)
  #Verificacao se o cnpj informado e sancionado e mudanca da coloracao para vermelho
  vertices <- vertices %>%
    mutate(role = case_when(
      id %in% sancionados$CpfCnpjSancionado ~ 'vermelho',
      group == 'TEL' ~ 'telefones'
    ))
  
  # v_cnpj_sancionado <- sancionados %>%
  #   select(id = CPF.OU.CNPJ.DO.SANCIONADO, title= NOME.INFORMADO.PELO.ORGAO.SANCIONADOR) %>%
  #   filter(!is.na(id)) %>%
  #   mutate(group = 'PJ_PRIVADO')%>%
  #   filter(!duplicated(id))%>%
  #   left_join(select(vertices, id), by='id')
  # 
  # vertices <- vertices %>%
  #   bind_rows(v_cnpj_sancionado)
  # 
  # Worksheet "9_Socio_Parentesco" 
  
  # socio_parentesco <- socio_parentesco%>%
  #   mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
  #          CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11),
  #          NUM_CNPJ_EMPRESA1 = str_pad(NUM_CNPJ_EMPRESA1, pad = '0', side = 'left', width = 14),
  #          NUM_CNPJ_EMPRESA2 = str_pad(NUM_CNPJ_EMPRESA2, pad = '0', side = 'left', width = 14))
  # 
  # v_socio_parentesco <- socio_parentesco %>%
  #   filter(nchar(CPF1) == 11) %>%
  #   select(id = CPF1, title = NOME_CPF1) %>%
  #   filter(!is.na(id)) %>%
  #   mutate(group = 'PF',
  #          role = 'socio')
  # 
  # v_socio_parentesco <- socio_parentesco %>%
  #   filter(nchar(CPF2) == 11) %>%
  #   select(id = CPF2, title = NOME_CPF2) %>%
  #   filter(!is.na(id)) %>%
  #   mutate(group = 'PF',
  #          role = 'socio') %>%
  #   bind_rows(v_socio_parentesco)%>%
  #   filter(!duplicated(id))%>%
  #   left_join(select(vertices, id), by='id')
  # 
  # a_socio_parentesco <- socio_parentesco %>%
  #   mutate(start = NA, end = NA) %>%
  #   filter(!is.na(CPF1)) %>%
  #   filter(!is.na(CPF2)) %>%
  #   filter(!(CPF1 == CPF2)) %>% 
  #   select(from = CPF1,
  #          to = CPF2,
  #          start,end,
  #          role = RELACAO) %>%
  #   mutate(type = 'parentesco',
  #          role = tolower(role))
  # 
  # vertices <- vertices %>%
  #   bind_rows(v_socio_parentesco) %>% 
  #   arrange(id) %>% 
  #   filter(!duplicated(id))
  
  
  # Consolidação dos vértices e das arestas do grafo --------------------------------------------------------
  
  # neste momento, podemos verificar se algum PJ inicialmente definido como privado era, na realidade
  # publico, e eliminar as diplicatas que estavam considerando-os como PJ provados:
  vertices <- vertices %>% 
    arrange(match(group, c('PF', 'PJ_PUBLICO', 'PJ_PRIVADO'))) %>% 
    filter(!duplicated(id)) # <----- neste filtro prevalecerao os PJs publicos sobre os PJs privados
  
  
  # consolida as arestas e remove eventuais duplicatas e arestas quebradas
  arestas <- a_socio %>% 
    bind_rows(a_parente) %>% 
    #bind_rows(a_parente_servidor) %>% 
    bind_rows(a_tel_empresa) %>%
    #bind_rows(a_socio_parentesco) %>%
    bind_rows(a_vinculo_servidor_pub) %>%
    bind_rows(a_vinculo_socio_servidor)%>%
    bind_rows(a_empenho)
  
  arestas <- arestas %>%
    filter(from %in% vertices$id, to %in% vertices$id) %>% 
    mutate(id = 1:nrow(arestas))%>%
    unique()
  
  # remove eventuais vertices isolados
  vertices <- vertices %>%
    filter(id %in% arestas$from | id %in% arestas$to)
  
  list(
    edges = arestas,
    
    nodes = vertices,
    
    data = list(
      pessoa_juridica = cnpjs,
      pessoa_fisica = NULL, # quebra-galho
      empenho = empenhos,
      parentesco = parentesco,
      socio = socios,
      sancionado = sancionados
    )
  )
  
}