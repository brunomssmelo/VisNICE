library(fuzzyjoin)

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

DBI::dbBegin(con)

cnpj_sql <- read_file("./src/sql/ConsultaAbaCnpj.sql")
cnpj_sancionados <- read_file("./src/sql/ConsultaAbaSancionados.sql")

cnpj <- dbGetQuery(con, stri_enc_tonative(cnpj_sql))
sancionados <- dbGetQuery(con, stri_enc_tonative(cnpj_sancionados))

dbDisconnect(con)
sancionados <- Corrige_Codificacao_Dataframe(sancionados)
cnpj <- Corrige_Codificacao_Dataframe(cnpj)

sancionados <- sancionados %>%
  select(OrgaoSancionador)

teste <- sancionados %>%
  # head(10) %>%
  # distinct() %>%
  stringdist_join(df_cnpj_publico, by = c(OrgaoSancionador = "NOME_EMPRESARIAL"),  method = "osa", mode = "inner", max_dist = 2) %>%
  distinct() %>% select(CNPJ, OrgaoSancionador)