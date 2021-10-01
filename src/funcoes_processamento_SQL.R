FixDataFrameEncoding <- function(df, destEnconding = "UTF-8") {
  
  library(stringi)
  
  numCols <- ncol(df)
  df <- data.frame(df)
  for (col in 1:numCols)
  {
    colClass <- class(df[, col])
    
    if(length(intersect(colClass,c('factor', 'character')))>0){
      
      sourceEncoding <- stri_enc_detect(paste(df[, col], collapse = " "))[[1]]$Encoding
      sourceEncoding <- sourceEncoding[sourceEncoding %in% iconvlist()][1]
      
      needsConversion <- ifelse(!is.na(sourceEncoding), sourceEncoding != destEnconding, F)
      
      if("character" %in% colClass){
        if(needsConversion){
          df[, col] <- iconv(df[, col],
                             from = sourceEncoding,
                             to = destEnconding)
        }
      }
      
      if("factor" %in% colClass){
        if(needsConversion){
          levels(df[, col]) <- iconv(levels(df[, col]),
                                     from = sourceEncoding,
                                     to = destEnconding)
        }
      }
    }
  }
  return(as.data.frame(df))}

load_connection_sql <- function(cnpj){
  library(odbc)
  library(DBI)
  library(data.table)
  library(dplyr)
  library(stringr)
  
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "TCERJVM48",
                   Trusted_Connection = "True",
                   database ="DGI_CONSULTA" )
  
  DBI::dbBegin(con)
  
  ### criando a tabela tempor?ria #CNPJ_RAIZ
  dbExecute(con, "CREATE TABLE #CNPJ_RAIZ (CNPJ VARCHAR(20))", immediate = TRUE)
  
  if(cnpj=="")
    dbExecute(con, "INSERT INTO #CNPJ_RAIZ VALUES ('09060537'),
                              ('02812740'),
                              ('25195543'),
                              ('07028841'),
                              ('05769219')")
  print(cnpj)
  selected_nodes<-NULL
  if(cnpj!="" | str_detect(cnpj,";")){
    cnpj<-str_squish(cnpj)
    cnpj<-strsplit(cnpj,";")
    cnpj<-unlist(cnpj)
    
    
    script_cnpj<- paste0("INSERT INTO #CNPJ_RAIZ VALUES ('",cnpj[1],"')")
    
    if(length(cnpj)>1){
      for (i in 2:length(cnpj)){
        script_cnpj<-paste0(script_cnpj,",('",cnpj[i],"')")
      }
    } 
    
    dbExecute(con, script_cnpj)
    #selected_nodes <- dbGetQuery(con,"SELECT CNPJ FROM #CNPJ_RAIZ")
    print(cnpj)
    
  }
  
  
  #dbExecute(con, teste)
  
  
  ### criando a tabela tempor?ria #CNPJ
  dbExecute(con,"CREATE TABLE #CNPJ (CNPJ VARCHAR(20))",immediate = TRUE)
  
  dbExecute(con,"INSERT INTO #CNPJ SELECT NUM_CNPJ
               FROM DGI_CONSULTA.IMPORTADO.CNPJ
               WHERE NUM_CNPJ_RAIZ IN (SELECT CNPJ FROM #CNPJ_RAIZ)")
  
  
  ###  1 - CNPJ - (CADASTRO NACIONAL DE PESSOA JURIDICA) / CNPJ - RELACAO DE MATRIZ E FILIAL
  #Armazenando a consulta sql
  
  cnpj_sql<- paste0("SELECT 'PJ' AS CONSULTA,
				A.NUM_CNPJ,
				A.NOME,
				CASE
							WHEN A.IND_MATRIZ_FILIAL = 1 THEN 'Matriz' ELSE 'Filial'
        END AS IND_MATRIZ_FILIAL,
				A.DATA_ABERTURA_ESTABELECIMENTO, 
				A.situacao_descricao SITUACAO_CADASTRAL,
				A.DATA_SITUACAO_CADASTRAL,
				C.DESCR AS ATIVIDADE_ECONOMICA,
				D.DESCR AS NATUREZA_JURIDICA,
				E.DESCR AS PORTE_EMPRESA,
				A.DESCR_EMAIL,
				A.TIPO_LOGRADOURO,
				A.DESCR_LOGRADOURO,
				A.NUM_LOGRADOURO,
				A.DESCR_COMPLEMENTO_LOGRADOURO,
				A.NOME_BAIRRO,
				A.NUM_CEP,
				A.NOME_MUNICIPIO,
				A.SIGLA_UF,
				A.NUM_DDD1,
				A.NUM_TELEFONE1,
				A.NUM_DDD2,
				A.NUM_TELEFONE2,
				A.NUM_DDD_FAX,
				A.NUM_FAX,
				A.VALOR_CAPITAL_SOCIAL

FROM DGI_CONSULTA.IMPORTADO.CNPJ A INNER JOIN IMPORTADO.ATIVIDADE_ECONOMICA C
                ON A.COD_ATIVIDADE_ECON_PRINCIPAL = C.COD
                               INNER JOIN IMPORTADO.NATUREZA_JURIDICA D
                ON A.COD_NATUREZA_JURIDICA = D.COD
                               INNER JOIN IMPORTADO.PORTE_EMPRESA E
                ON A.COD_PORTE_EMPRESA = E.COD
WHERE A.NUM_CNPJ IN (SELECT CNPJ FROM #CNPJ)
ORDER BY A.NUM_CNPJ,
                A.IND_MATRIZ_FILIAL,
                DATA_ABERTURA_ESTABELECIMENTO")
  
  ### 2 - CNPJ - TELEFONES
  
  #Armazenando a consulta sql
  telefones_sql<-paste0("
SELECT 'TELEFONE' AS CONSULTA,
                CONCAT(A.NUM_DDD1, A.NUM_TELEFONE1) AS TELEFONE,
                A.NUM_CNPJ,
                A.NOME

FROM DGI_CONSULTA.IMPORTADO.CNPJ A
WHERE A.NUM_CNPJ IN (SELECT CNPJ FROM #CNPJ) AND NOT A.NUM_TELEFONE1 IS NULL

UNION

SELECT 'TELEFONE' AS CONSULTA,
                CONCAT(A.NUM_DDD2, A.NUM_TELEFONE2) AS TELEFONE,
                A.NUM_CNPJ,
                A.NOME
FROM DGI_CONSULTA.IMPORTADO.CNPJ A
WHERE A.NUM_CNPJ IN (SELECT CNPJ FROM #CNPJ) AND NOT A.NUM_TELEFONE2 IS NULL

UNION

SELECT 'TELEFONE' AS CONSULTA,
                CONCAT(A.NUM_DDD_FAX, A.NUM_FAX) AS TELEFONE,
                A.NUM_CNPJ,
                A.NOME

FROM DGI_CONSULTA.IMPORTADO.CNPJ A
WHERE A.NUM_CNPJ IN (SELECT CNPJ FROM #CNPJ) AND NOT A.NUM_FAX IS NULL

ORDER BY TELEFONE,
                A.NUM_CNPJ")
  
  ###  3 - CNPJ - SOCIO(S), REPRESENTANTE(S) E PARTICIPACOES SOCIETARIAS
  
  
  #Armazenando a consulta sql
  socio_sql<- paste0("
SELECT 'SOCIO' AS CONSULTA,
                A.NUM_CNPJ_EMPRESA,
                A.NOME AS NOME_SOCIO,
                A.NUM_CPF,
                B.NOME AS NOME_EMPRESA,
                C.DESCR AS QUALIFICACAO_SOCIO,
                A.DATA_ENTRADA_SOCIEDADE,
                A.DATA_DE_EXCLUSAO_NA_SOCIEDADE,
                A.VALOR_PERCENTUA_CAPITAL_SOCIAL

FROM DGI_CONSULTA.IMPORTADO.SOCIO A INNER JOIN DGI_CONSULTA.IMPORTADO.CNPJ B
                ON A.NUM_CNPJ_EMPRESA = B.NUM_CNPJ INNER JOIN DGI_CONSULTA.IMPORTADO.QUALIFICACAO_SOCIO_RESP_REPRES C
                ON A.COD_QUALIFICACAO_SOCIO = C.COD
WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ)

ORDER BY A.NUM_CPF,
                A.NOME")
  
  ### 8 - CPF - RELACAO PARENTESCO
  
  parentesco_sql<- paste0("
SELECT DISTINCT 'PARENTESCO' AS CONSULTA,
                A.CPF1,
                B.NOME AS NOME1,
                A.RELACAO,
                D.NOME AS NOME2,
                A.CPF2

FROM DGI_CONSULTA.IMPORTADO.RELACAO_PF_PF A LEFT JOIN DGI_CONSULTA.IMPORTADO.CPF B
                ON A.CPF1 = B.NUM_CPF LEFT JOIN DGI_CONSULTA.IMPORTADO.RELACAO_PF_PF C
                ON A.CPF1 = C.CPF1 INNER JOIN DGI_CONSULTA.IMPORTADO.CPF D
                ON A.CPF2 = D.NUM_CPF

WHERE A.CPF1 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

UNION

SELECT DISTINCT 'PARENTESCO' AS CONSULTA,
                A.CPF1,
                B.NOME AS NOME1,
                A.RELACAO,
                D.NOME AS NOME2,
                A.CPF2

FROM DGI_CONSULTA.IMPORTADO.RELACAO_PF_PF A LEFT JOIN DGI_CONSULTA.IMPORTADO.CPF B
                ON A.CPF1 = B.NUM_CPF LEFT JOIN DGI_CONSULTA.IMPORTADO.RELACAO_PF_PF C
                ON A.CPF1 = C.CPF1 INNER JOIN DGI_CONSULTA.IMPORTADO.CPF D
                ON A.CPF2 = D.NUM_CPF

WHERE A.CPF2 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

UNION

SELECT DISTINCT 'PARENTESCO' AS CONSULTA,
                A.CPF1,
                B.NOME AS NOME1,
                A.RELACAO,
                D.NOME AS NOME2,
                A.CPF2

FROM DGI_CONSULTA.IMPORTADO.PARENTESCO_RELACAO_PF_PF A LEFT JOIN DGI_CONSULTA.IMPORTADO.CPF B
                ON A.CPF1 = B.NUM_CPF LEFT JOIN DGI_CONSULTA.IMPORTADO.PARENTESCO_RELACAO_PF_PF C
                ON A.CPF1 = C.CPF1 INNER JOIN DGI_CONSULTA.IMPORTADO.CPF D
                ON A.CPF2 = D.NUM_CPF

WHERE A.CPF1 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

UNION

SELECT DISTINCT 'PARENTESCO' AS CONSULTA,
                A.CPF1,
                B.NOME AS NOME1,
                A.RELACAO,
                D.NOME AS NOME2,
                A.CPF2

FROM DGI_CONSULTA.IMPORTADO.PARENTESCO_RELACAO_PF_PF A LEFT JOIN DGI_CONSULTA.IMPORTADO.CPF B
                ON A.CPF1 = B.NUM_CPF LEFT JOIN DGI_CONSULTA.IMPORTADO.PARENTESCO_RELACAO_PF_PF C
                ON A.CPF1 = C.CPF1 INNER JOIN DGI_CONSULTA.IMPORTADO.CPF D
                ON A.CPF2 = D.NUM_CPF

WHERE A.CPF2 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

ORDER BY A.CPF1,
                A.CPF2")
  
  ## 17 - CPF - FUNCIONARIOS EMPREGADOS NA ADMINISTRACAO PUBLICA 
  
  #passagem da consulta 
  funcionariosNaAdmPublica_sql <- paste0("
SELECT DISTINCT 'FUNCIONÁRIO ORG PUB' AS CONSULTA,
  A.CO_CPF,
  B.NOME AS EMPREGADO,
  A.CO_CNPJ_CEI,
  C.NOME AS EMPREGADOR,
  C.NOME_MUNICIPIO,
  C.SIGLA_UF,
  D.DS_TIPO_VINCULO AS TIPO,
  A.DA_ADMISSAO_RAIS_DMA,
  A.DA_DESLIGAMENTO_RAIS_DM
FROM importado.EMPREGADOs A INNER JOIN importado.CPF B
  ON A.CO_CPF = B.NUM_CPF INNER JOIN importado.CNPJ C
  ON A.CO_CNPJ_CEI = C.NUM_CNPJ INNER JOIN importado.CD_TIPO_VINCULO D 
  ON A.CO_TIPO_VINCULO_RAIS = D.CD_TIPO_VINCULO
WHERE C.COD_NATUREZA_JURIDICA IN (1090, 1996, 2020, 1287, 1295, 1309, 1317, 1325, 1333, 1341, 1015,
  1023, 1031, 1040, 1058, 1066, 1074, 1082, 1104, 1112, 1120, 1139, 1147, 1155, 1163, 1171, 1180,
  1201, 1210, 1236, 1244, 1252, 1260, 1279, 2011, 2038) AND
  CO_CPF IN (SELECT A.CO_CPF
    FROM importado.EMPREGADOs A INNER JOIN importado.CD_CBO_OCUPACAO B
      ON A.CO_CBO_RAIS = B.CD_CBO_OCUPACAO LEFT JOIN importado.CNPJ C
      ON A.CO_CNPJ_CEI = C.NUM_CNPJ
    WHERE A.CO_CNPJ_CEI IN (SELECT CNPJ FROM #CNPJ) AND B.TIPO = 'Ocupação'
    UNION
    SELECT B.NUM_CPF
    FROM importado.CAGED A INNER JOIN importado.CPF B
      ON A.CPF = B.NUM_CPF INNER JOIN importado.CNPJ C
      ON A.CNPJ = C.NUM_CNPJ LEFT JOIN importado.CD_CBO_OCUPACAO D
      ON A.CBO_2002_OCUPACAO = D.CD_CBO_OCUPACAO
    WHERE A.CNPJ IN (SELECT A.NUM_CNPJ_EMPRESA
      FROM importado.SOCIO A  
      WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ)))
")
  
  dbExecute(con,"WITH
QRY_SOCIO AS (SELECT 'SOCIO' AS CONSULTA,
  A.NUM_CNPJ_EMPRESA,
  B.NOME AS NOME_EMPRESA,
  A.NUM_CPF AS CPF_SOCIO,
  A.NOME AS NOME_SOCIO,
  C.DESCR AS QUALIFICACAO_SOCIO,
  A.DATA_ENTRADA_SOCIEDADE,
  A.DATA_DE_EXCLUSAO_NA_SOCIEDADE,
  A.VALOR_PERCENTUA_CAPITAL_SOCIAL
FROM importado.SOCIO A INNER JOIN importado.CNPJ B
  ON A.NUM_CNPJ_EMPRESA = B.NUM_CNPJ INNER JOIN importado.QUALIFICACAO_SOCIO_RESP_REPRES C
  ON A.COD_QUALIFICACAO_SOCIO = C.COD
WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

SELECT * INTO #TEMP_SOCIO
FROM QRY_SOCIO", immediate = TRUE)
  
  
  dbExecute(con,"SELECT DISTINCT * INTO #TEMP_CPF_SOCIO
FROM (SELECT CPF_SOCIO AS CPF
FROM #TEMP_SOCIO) AS TEMP_CPF_SOCIO

SELECT * FROM #TEMP_SOCIO
ORDER BY CPF_SOCIO
", immediate = TRUE)
  
  # 18 - CPF - SÓCIOS EMPREGADOS NA ADMINISTRAÇÃO PÚBLICA
  
  #passagem da consulta
  
  cpfSociosNaAdmPublica_sql <- paste0("
SELECT DISTINCT 'SOCIO ORG PUB' AS CONSULTA,
	A.CO_CPF,
	B.NOME AS EMPREGADO,
	A.CO_CNPJ_CEI,
	C.NOME AS EMPREGADOR,
	C.NOME_MUNICIPIO,
	C.SIGLA_UF,
	D.DS_TIPO_VINCULO AS TIPO,
	A.DA_ADMISSAO_RAIS_DMA,
	A.DA_DESLIGAMENTO_RAIS_DM
FROM importado.EMPREGADOS A INNER JOIN importado.CPF B
	ON A.CO_CPF = B.NUM_CPF INNER JOIN importado.CNPJ C
	ON A.CO_CNPJ_CEI = C.NUM_CNPJ INNER JOIN importado.CD_TIPO_VINCULO D
	ON A.CO_TIPO_VINCULO_RAIS = D.CD_TIPO_VINCULO
WHERE C.COD_NATUREZA_JURIDICA IN (1090, 1996, 2020, 1287, 1295, 1309, 1317, 1325, 1333, 1341, 1015,
	1023, 1031, 1040, 1058, 1066, 1074, 1082, 1104, 1112, 1120, 1139, 1147, 1155, 1163, 1171, 1180,
	1201, 1210, 1236, 1244, 1252, 1260, 1279, 2011, 2038) AND

	CO_CPF IN (SELECT CPF FROM #TEMP_CPF_SOCIO)    
ORDER BY A.CO_CPF,
	A.CO_CNPJ_CEI,
	A.DA_ADMISSAO_RAIS_DMA,
	A.DA_DESLIGAMENTO_RAIS_DM")
  
  ###   9 - CNPJ - RELACAO SOCIO-PARENTESCO
  
  SocioParentesco_sql<- paste0("
      WITH
        CONSULTA_SOCIO_PARENTE_RELACAO_PFPF AS (SELECT DISTINCT A.CPF1,
            B.NOME AS NOME_CPF1,
            C.NUM_CNPJ_EMPRESA AS NUM_CNPJ_EMPRESA1,
            E.DESCR AS QUALIFICACAO1,
            D.NOME AS NOME_EMPRESA1,
            C.DATA_ENTRADA_SOCIEDADE AS DATA_ENTRADA_SOCIEDADE1,
            C.DATA_DE_EXCLUSAO_NA_SOCIEDADE AS DATA_EXCLUSAO_SOCIEDADE1,
            A.RELACAO,
            A.CPF2
            
            FROM DGI_CONSULTA.IMPORTADO.RELACAO_PF_PF A INNER JOIN DGI_CONSULTA.IMPORTADO.CPF B
              ON A.CPF1 = B.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.SOCIO C
              ON A.CPF1 = C.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.CNPJ D
              ON C.NUM_CNPJ_EMPRESA = D.NUM_CNPJ INNER JOIN DGI_CONSULTA.IMPORTADO.QUALIFICACAO_SOCIO_RESP_REPRES E
              ON C.COD_QUALIFICACAO_SOCIO = E.COD
                WHERE CPF1 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                               WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

            UNION

              SELECT DISTINCT A.CPF1,
                               B.NOME AS NOME_CPF1,
                               C.NUM_CNPJ_EMPRESA AS NUM_CNPJ_EMPRESA1,
                               E.DESCR AS QUALIFICACAO1,
                               D.NOME AS NOME_EMPRESA1,
                               C.DATA_ENTRADA_SOCIEDADE AS DATA_ENTRADA_SOCIEDADE1,
                               C.DATA_DE_EXCLUSAO_NA_SOCIEDADE AS DATA_EXCLUSAO_SOCIEDADE1,
                               A.RELACAO,
                               A.CPF2

            FROM DGI_CONSULTA.IMPORTADO.PARENTESCO_RELACAO_PF_PF A INNER JOIN DGI_CONSULTA.IMPORTADO.CPF B
              ON A.CPF1 = B.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.SOCIO C
              ON A.CPF1 = C.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.CNPJ D
              ON C.NUM_CNPJ_EMPRESA = D.NUM_CNPJ INNER JOIN DGI_CONSULTA.IMPORTADO.QUALIFICACAO_SOCIO_RESP_REPRES E
              ON C.COD_QUALIFICACAO_SOCIO = E.COD
                WHERE CPF1 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                               WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))),


        CONSULTA_SOCIO_PARENTE_PARENTESCO_RELACAO_PFPF AS (SELECT DISTINCT A.CPF1,
            RELACAO,
            A.CPF2,
            B.NOME AS NOME_CPF2,
            D.DESCR AS QUALIFICACAO2,
            E.NUM_CNPJ AS NUM_CNPJ_EMPRESA2,
            E.NOME AS NOME_EMPRESA2,
            C.DATA_ENTRADA_SOCIEDADE AS DATA_ENTRADA_SOCIEDADE2,
            C.DATA_DE_EXCLUSAO_NA_SOCIEDADE AS DATA_EXCLUSAO_SOCIEDADE2
                FROM DGI_CONSULTA.IMPORTADO.RELACAO_PF_PF A INNER JOIN DGI_CONSULTA.IMPORTADO.CPF B
                    ON A.CPF2 = B.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.SOCIO C
                    ON A.CPF2 = C.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.QUALIFICACAO_SOCIO_RESP_REPRES D
                    ON C.COD_QUALIFICACAO_SOCIO = D.COD LEFT JOIN DGI_CONSULTA.IMPORTADO.CNPJ E
                    ON C.NUM_CNPJ_EMPRESA = E.NUM_CNPJ
                WHERE CPF2 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                               WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ))

            UNION
              SELECT DISTINCT A.CPF1,
                               RELACAO,
                               A.CPF2,
                               B.NOME AS NOME_CPF2,
                               D.DESCR AS QUALIFICACAO2,
                               E.NUM_CNPJ AS NUM_CNPJ_EMPRESA2,
                               E.NOME AS NOME_EMPRESA2,
                               C.DATA_ENTRADA_SOCIEDADE AS DATA_ENTRADA_SOCIEDADE2,
                               C.DATA_DE_EXCLUSAO_NA_SOCIEDADE AS DATA_EXCLUSAO_SOCIEDADE2

                FROM DGI_CONSULTA.IMPORTADO.PARENTESCO_RELACAO_PF_PF A INNER JOIN DGI_CONSULTA.IMPORTADO.CPF B
                               ON A.CPF2 = B.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.SOCIO C
                               ON A.CPF2 = C.NUM_CPF INNER JOIN DGI_CONSULTA.IMPORTADO.QUALIFICACAO_SOCIO_RESP_REPRES D
                               ON C.COD_QUALIFICACAO_SOCIO = D.COD LEFT JOIN DGI_CONSULTA.IMPORTADO.CNPJ E
                               ON C.NUM_CNPJ_EMPRESA = E.NUM_CNPJ
                WHERE CPF2 IN (SELECT A.NUM_CPF FROM DGI_CONSULTA.IMPORTADO.SOCIO A
                               WHERE A.NUM_CNPJ_EMPRESA IN (SELECT CNPJ FROM #CNPJ)))



              SELECT DISTINCT 'SOCIO-PARENTESCO' AS CONSULTA,
                               PR.CPF1,
                               PR.NOME_CPF1,
                               PR.QUALIFICACAO1,
                               PR.NUM_CNPJ_EMPRESA1,
                               PR.NOME_EMPRESA1,
                               PR.DATA_ENTRADA_SOCIEDADE1,
                               PR.DATA_EXCLUSAO_SOCIEDADE1,
                               PR.CPF2,
                               PR.RELACAO,
                               PA.NOME_CPF2,
                               PA.QUALIFICACAO2,
                               PA.NUM_CNPJ_EMPRESA2,
                               PA.NOME_EMPRESA2,
                               PA.DATA_ENTRADA_SOCIEDADE2,
                               PA.DATA_EXCLUSAO_SOCIEDADE2

              FROM CONSULTA_SOCIO_PARENTE_RELACAO_PFPF PR INNER JOIN CONSULTA_SOCIO_PARENTE_PARENTESCO_RELACAO_PFPF PA
                               ON PR.CPF1 = PA.CPF1 AND PR.CPF2 = PA.CPF2

              WHERE PR.NUM_CNPJ_EMPRESA1 <> PA.NUM_CNPJ_EMPRESA2 AND
                               PR.NUM_CNPJ_EMPRESA1 IN (SELECT CNPJ FROM #CNPJ) 
                                  
                                  AND
                                  
                               PA.NUM_CNPJ_EMPRESA2 IN (SELECT CNPJ FROM #CNPJ)

              ORDER BY PR.NOME_CPF1,
                               PR.NOME_EMPRESA1,
                               NOME_CPF2,
                               PA.NOME_EMPRESA2")
  
  
  # executando a consulta
  cnpjs<- dbGetQuery(con, cnpj_sql)
  telefones <- dbGetQuery(con, telefones_sql)
  socios <- dbGetQuery(con, socio_sql)
  parentesco <- dbGetQuery(con, parentesco_sql)
  func_na_adm_publica <- dbGetQuery(con, funcionariosNaAdmPublica_sql)
  socio_org_publico <- dbGetQuery(con, cpfSociosNaAdmPublica_sql)
  #socio_parentesco <- dbGetQuery(con, SocioParentesco_sql)
  
  # fecha a conex?o
  DBI::dbDisconnect(con) 
  
  con2 <- dbConnect(odbc(),
                    Driver = "SQL Server",
                    Server = "TCERJVM48",
                    Trusted_Connection = "True",
                    database ="IRIS_EMPENHO" )
  
  dbExecute(con2, "CREATE TABLE #CNPJ_RAIZ (CNPJ VARCHAR(20))", immediate = TRUE)
  
  if(cnpj==""){
    dbExecute(con2, "INSERT INTO #CNPJ_RAIZ VALUES ('09060537'),
                              ('02812740'),
                              ('25195543'),
                              ('07028841'),
                              ('05769219')")}
  # print(cnpj)
  # selected_nodes<-NULL
  if(cnpj!="" | str_detect(cnpj,";")){
    cnpj<-str_squish(cnpj)
    # cnpj<-strsplit(cnpj,";")
    # cnpj<-unlist(cnpj)
    
    #Extrair de 8 em 8 caracteres a partir do ;
    
    script_empenho<- paste0("INSERT INTO #CNPJ_RAIZ VALUES ('",cnpj[1],"')")
    
    if(length(cnpj)>1){
      for (i in 2:length(cnpj)){
        script_empenho<-paste0(script_empenho,",('",cnpj[i],"')")
      }
    } 
    
    dbExecute(con2, script_empenho)
    #selected_nodes <- dbGetQuery(con,"SELECT CNPJ FROM #CNPJ_RAIZ")
    
  }
  
  empenhos_sql<-"SELECT DISTINCT 'EMPENHOS' AS CONSULTA,
  B.ENTE AS Ente,
A.ID_UG AS IdUnidadeGestora,
A.UNIDADE AS UnidadeGestora,
B.NU_CGC AS CnpjUnidadeGestora,
A.NU_EMPENHO AS NumeroEmpenho,
A.TP_EMPENHO AS TipoEmpenho,
A.DT_EMPENHO AS DataEmpenho,
A.[VL EMPENHADO] AS ValorEmpenhado,
A.ANULACAOEMPENHO AS ValorAnulacaoEmpenho,
A.[VL LIQUIDADO] AS ValorLiquidado,
A.[VALOR ANULACAO LIQUIDACAO] AS ValorAnulacaoLiquidacao,
A.[VALOR SUBEMPENHO] AS ValorSubempenho,
A.[VL PAGO] AS ValorPago,
A.[VALOR RETENÇÃO] AS ValorRetencao,
A.NU_LICITACAO AS NumeroLicitacao,
A.HISTORICO AS HistoricoEmpenho,
A.CREDOR AS Credor,
A.CPF_CNPJ_CREDOR AS CpfCnpjCredor,
A.ELEMENTOTCE AS ElementoDespesaTCE,
A.DE_FONTE_TCE AS FonteRecursoTCE,
A.NU_PROJ_ATIV As NumeroProjetoAtividade,
A.DE_PROJATIV AS ProjetoAtividade,
A.FUNCAO AS Funcao,
A.SUBFUNCAO AS Subfuncao,
A.DE_PROGRAMA AS ProgramaTrabalho
FROM IRISEMPENHOMUNICIPIO AS A LEFT JOIN IRISUNIDADEGESTORA AS B
ON A.ID_UG = B.CD_UNIDADE JOIN #CNPJ_RAIZ AS C ON CPF_CNPJ_CREDOR like CONCAT(C.CNPJ,'%')"
  
  # executando a consulta
  empenhos<-dbGetQuery(con2,empenhos_sql)
  
  # fecha a conex?o
  DBI::dbDisconnect(con2) 
  
  #Consertando as acentuacoes
  parentesco <- FixDataFrameEncoding(parentesco)
  empenhos <- FixDataFrameEncoding(empenhos)
  socio_org_publico <- FixDataFrameEncoding(socio_org_publico)
  cnpjs<- FixDataFrameEncoding(cnpjs)
  func_na_adm_publica<- FixDataFrameEncoding(func_na_adm_publica)
  #socio_parentesco <- FixDataFrameEncoding(socio_parentesco)
  
  # # tabela_cnpj ###################################################################################
  cnpjs <- cnpjs%>% mutate(TEL1 = paste0(NUM_DDD1, NUM_TELEFONE1), TEL2 = paste0(NUM_DDD2, NUM_TELEFONE2),
                           TEL1 = str_pad(TEL1, pad = '0', side = "left", width = 10),
                           TEL2 = str_pad(TEL2, pad = '0', side = "left", width = 10))
  
  v_cnpj <- cnpjs %>%
    select(id = NUM_CNPJ, title = NOME) %>%
    filter(nchar(id)==14, !duplicated(id)) %>%
    mutate(group = 'PJ')
  
  # v_telefones <- cnpj %>%
  #   select(id = TEL1, title = TEL1) %>%
  #   filter(nchar(id)<=11) %>%
  #   filter(!is.na(id))
  
  v_telefones <- cnpjs %>%
    select(id = TEL1, title = TEL1) %>%
    filter(nchar(id)<=11) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'TEL',
           role = 'telefones')%>%
    filter(!duplicated(id))
  
  a_tel_cnpj <- cnpjs %>%
    mutate(start = NA, end = NA) %>%
    select(from = NUM_CNPJ,
           to = TEL1,
           start, end)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(role = 'telefones',
           type = 'telefone_empresa')
  
  
  vertices <- v_cnpj %>%
    bind_rows(v_telefones)
  
  # # telefones ###################################################################################
  ### ordenando a tabela pelos telefones
  ###  em ordem crescente para comparaÃ§Ã£o par a par posterior
  #telefones<-telefones[order(telefones$TELEFONE)]
  
  # AlocaÃ§Ã£o na memÃ³ria do vetor da coluna de verificaÃ§Ã£o com o 
  # mesmo tamanho das listas da tabela_telefones
  # v<- rep(NA,length(telefones$TELEFONE))
  # 
  # 
  # #  marca como iguais onde nÃºmeros de telefones iguais estÃ£o associados a duas
  # # raizes de CNPJs diferentes
  # # raiz do CNPJ = (substring)primeiros 8 digitos
  # for (i in 2:length(telefones$TELEFONE)) {
  #   
  #   if(
  #     (telefones$TELEFONE[i-1] == telefones$TELEFONE[i])
  #     &
  #     (substr(telefones$NUM_CNPJ[i-1],1 ,8)!= substr(telefones$NUM_CNPJ[i],1,8))
  #   ){
  #     v[i-1]<-"iguais"
  #     v[i]<-"iguais"
  #   }
  # }
  # 
  # #passagem do vetor para lista que sera anexada ao final da tabela_telefones
  # verificacao<- as.list(v)
  # 
  # telefones <- telefones%>% mutate(verificacao = verificacao)
  
  v_tel <- telefones %>%
    filter(nchar(TELEFONE)<=11) %>%
    select(id = TELEFONE, title = TELEFONE) %>%
    filter(!is.na(id)) %>%
    mutate(group = 'TEL',
           role = 'telefones')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  v_cnpj <- telefones %>%
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
    bind_rows(v_cnpj)
  
  # # socios ###################################################################################
  
  socios <- socios %>% mutate(NUM_CNPJ_EMPRESA = str_pad(NUM_CNPJ_EMPRESA, pad = '0', side = 'left', width = 14)) %>% 
    mutate(NUM_CPF = case_when(
      nchar(NUM_CPF) > 11 ~ str_pad(NUM_CPF, pad = '0', side = 'left', width = 14),
      T ~ str_pad(NUM_CPF, pad = '0', side = 'left', width = 11)))
  
  a_socio <- socios %>% 
    select(from = NUM_CPF,
           to = NUM_CNPJ_EMPRESA,
           start = DATA_ENTRADA_SOCIEDADE,
           end = DATA_DE_EXCLUSAO_NA_SOCIEDADE) %>% 
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>% 
    mutate(role = 'socio',
           type = 'sociedade')%>%
    unique()
  
  v_socio_pf <- socios %>% 
    select(id = NUM_CPF, title = NOME_SOCIO) %>% 
    filter(nchar(id)==11) %>% 
    mutate(group = 'PF') %>% 
    inner_join(select(a_socio, from:to), by=c('id'='from')) %>% 
    #  left_join(select(ws_listados, id, level), by=c('to'='id')) %>% 
    select(-to) %>% 
    filter(!duplicated(id))
  
  v_socio_pj <- socios %>% 
    select(id = NUM_CPF, title = NOME_SOCIO) %>% 
    #left_join(select(ws_listados, id, level), by='id') %>% 
    filter(nchar(id)==14, !duplicated(id)) %>% 
    mutate(group = 'PJ_PRIVADO')
  
  vertices <- vertices %>%
    bind_rows(v_socio_pf) %>% 
    bind_rows(v_socio_pj) %>% 
    arrange(id) %>% 
    filter(!duplicated(id))
  
  # # parentesco ###################################################################################
  # Encoding(pessoaFisica$NOME_MAE) <- "latin1"
  # 
  # Encoding(PARENTESCO_RELACAO$RELACAO) <- "latin1"
  # Encoding(PARENTESCO_RELACAO$FONTE) <- "latin1"
  
  parentesco <- parentesco %>% mutate(CPF1 = str_pad(CPF1, pad = '0', side = 'left', width = 11),
                                      CPF2 = str_pad(CPF2, pad = '0', side = 'left', width = 11))%>%
    mutate(RELACAO = case_when(
      str_detect(RELACAO,'AVÓ')~'AVÓ/AVÔ',
      str_detect(RELACAO,'AVÕ')~'AVÓ/AVÔ',
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
  
  # # func_na_adm_publica ###################################################################################
  func_na_adm_publica <- func_na_adm_publica %>%
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
    mutate(group = 'PJ_PUBLICO')%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  a_vinculo_servidor_pub <- func_na_adm_publica %>%
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
  
  
  # # socio_org_publico ###################################################################################
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
    mutate(type = 'vinculo_empregaticio')%>%
    filter(!is.na(from), !is.na(to))
  
  vertices <- vertices %>%
    bind_rows(v_socio_servidor) %>%
    bind_rows(v_socio_org_publico)
  
  # # tabela_empenhos ###################################################################################
  # company_name_to_id <- empenhos$CnpjUnidadeGestora %>%
  #   `names<-`(empenhos$UnidadeGestora)
  
  empenhos <- empenhos %>% mutate(CpfCnpjCredor= str_pad(CpfCnpjCredor, pad = '0', side = 'left', width = 14))%>%
    mutate(CpfCnpjCredor = case_when(
      nchar(CpfCnpjCredor) > 11 ~ str_pad(CpfCnpjCredor, pad = '0', side = 'left', width = 14),
      T ~ str_pad(CpfCnpjCredor, pad = '0', side = 'left', width = 11)))%>%
    mutate(DataEmpenho = as.character(DataEmpenho), ValorPago= as.character(ValorPago))
  
  a_empenho<- empenhos %>%
    mutate(start=NA, end=NA)%>%
    select(from = CnpjUnidadeGestora,
           to = CpfCnpjCredor,
           start, end,
           role = ValorPago)%>%
    filter(!is.na(from)) %>%
    filter(!is.na(to)) %>%
    mutate(type = 'empenho')%>%
    filter(!is.na(from), !is.na(to))
  
  v_cnpj_credor<- empenhos %>%
    select(id = CpfCnpjCredor, title = Credor) %>%
    filter(!is.na(id)) %>%
    mutate(group = "PJ_PRIVADO")%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices, id), by='id')
  
  v_unidade_gestora <- empenhos %>%
    select(id = CnpjUnidadeGestora, title = UnidadeGestora)%>%
    filter(!is.na(id))%>%
    mutate(group = "PJ_PUBLICO")%>%
    filter(!duplicated(id))%>%
    left_join(select(vertices,id), by = 'id')
  
  vertices <- vertices %>%
    bind_rows(v_cnpj_credor) %>%
    bind_rows(v_unidade_gestora)
  
  # Worksheet "9_Socio_Parentesco" ############################################################################
  
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
    bind_rows(a_vinculo_socio_servidor) %>%
    bind_rows(a_empenho)%>%
    filter(from %in% vertices$id, to %in% vertices$id) %>% 
    unique()
  
  # remove eventuais vertices isolados
  vertices <- vertices %>% 
    filter(id %in% arestas$from | id %in% arestas$to)
  
  list(
    edges = arestas,
    
    nodes = vertices
  )
  
}