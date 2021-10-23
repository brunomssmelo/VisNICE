Corrige_Codificacao_Dataframe <- function(df, codificacao_destino = "UTF-8") {
  
  library(stringi)
  
  num_col <- ncol(df)
  df <- data.frame(df)
  for (col in 1:num_col)
  {
    classe_col <- class(df[, col])
    
    if(length(intersect(classe_col,c('factor', 'character')))>0){
      
      codificacao_origem <- stri_enc_detect(paste(df[, col], collapse = " "))[[1]]$Encoding
      codificacao_origem <- codificacao_origem[codificacao_origem %in% iconvlist()][1]
      
      necessario_converter <- ifelse(!is.na(codificacao_origem), codificacao_origem != codificacao_destino, F)
      
      if("character" %in% classe_col){
        if(necessario_converter){
          df[, col] <- iconv(df[, col],
                             from = codificacao_origem,
                             to = codificacao_destino)
        }
      }
      
      if("factor" %in% classe_col){
        if(necessario_converter){
          levels(df[, col]) <- iconv(levels(df[, col]),
                                     from = codificacao_origem,
                                     to = codificacao_destino)
        }
      }
    }
  }
  return(as.data.frame(df))}