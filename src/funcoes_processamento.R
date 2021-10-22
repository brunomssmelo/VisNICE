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