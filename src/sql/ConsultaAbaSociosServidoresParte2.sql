-- QUERY PRINCIPAL

SELECT DISTINCT
	'SOCIO ORG PUB' AS CONSULTA,
	A.CO_CPF,
	B.NOME AS EMPREGADO,
	A.CO_CNPJ_CEI,
	C.NOME AS EMPREGADOR,
	C.NOME_MUNICIPIO,
	C.SIGLA_UF,
	D.DS_TIPO_VINCULO AS TIPO,
	A.DA_ADMISSAO_RAIS_DMA,
	A.DA_DESLIGAMENTO_RAIS_DM
FROM
	importado.EMPREGADOS A
	INNER JOIN importado.CPF B
		ON A.CO_CPF = B.NUM_CPF
	INNER JOIN importado.CNPJ C
		ON A.CO_CNPJ_CEI = C.NUM_CNPJ
	INNER JOIN importado.CD_TIPO_VINCULO D
		ON A.CO_TIPO_VINCULO_RAIS = D.CD_TIPO_VINCULO
WHERE
	C.COD_NATUREZA_JURIDICA IN (1090, 1996, 2020, 1287, 1295, 1309, 1317, 1325, 1333, 1341, 1015,
	1023, 1031, 1040, 1058, 1066, 1074, 1082, 1104, 1112, 1120, 1139, 1147, 1155, 1163, 1171, 1180,
	1201, 1210, 1236, 1244, 1252, 1260, 1279, 2011, 2038)
	AND	CO_CPF IN (SELECT CPF FROM #TEMP_CPF_SOCIO)    
ORDER BY
	A.CO_CPF,
	A.CO_CNPJ_CEI,
	A.DA_ADMISSAO_RAIS_DMA,
	A.DA_DESLIGAMENTO_RAIS_DM
