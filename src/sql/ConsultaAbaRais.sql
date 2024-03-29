-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

 SELECT DISTINCT 'RAIS CONTAGEM' AS CONSULTA,
    A.NUM_CNPJ,
    A.NOME,
    B.ANO_RAIS,
    COUNT(B.CO_CPF) AS QTDE

  FROM DGI_CONSULTA.IMPORTADO.CNPJ A INNER JOIN DGI_CONSULTA.IMPORTADO.EMPREGADOS B
                ON A.NUM_CNPJ = B.CO_CNPJ_CEI

  WHERE A.NUM_CNPJ IN (SELECT CNPJ FROM #CNPJ)

  GROUP BY A.NUM_CNPJ,
    A.NOME,
    B.ANO_RAIS

  ORDER BY A.NUM_CNPJ,
                B.ANO_RAIS