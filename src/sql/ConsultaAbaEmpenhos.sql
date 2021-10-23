-- !preview conn=DBI::dbConnect(RSQLite::SQLite())
SELECT DISTINCT 'EMPENHOS' AS CONSULTA
,[ESFERA] AS Esfera
,[ID_UNIDADE] AS IdUnidadeGestora
,[CPF_CNPJ_CREDOR] AS CpfCnpjCredor
,[CREDOR] AS Credor
,[Id_contrato] AS IdContrato
,[NumeroLicitacao] AS NumeroLicitacao
,[Ente] AS Ente
,[unidade] AS UnidadeGestora
,[CnpjUnidadeGestora] AS CnpjUnidadeGestora
,[total_valorEmpenho] AS TotalValorEmpenho
,[total_valorpagamento] AS TotalValorPagamento
,[total_ValorAnulacaoEmpenho] AS TotalValorAnulacaoEmpenho
,[total_ValorLiquidado] AS TotalValorLiquidado
,[total_ValorAnulacaoLiquidacao] AS TotalValorAnulacaoLiquidacao
,[total_ValorSubempenho] AS TotalValorSubempenho
,[total_ValorPago] AS TotalValorPago
,[total_ValorRetencao] AS TotalValorRetencao
,[total_valorEmpenhado] AS TotalValorEmpenhado
,[QUANTIDADE EMPENHOS] AS QntdEmpenhos
,[DATA INICIO] AS DataInicio
,[DATA FIM] AS DataFim
,[DT_ANOMES INICIO] AS DataAnoMesInicio
,[DT_ANOMES FIM] AS DataAnoMesFim
,[ano INICIO] AS AnoInicio
,[ano FIM] AS AnoFim
FROM [IRIS_EMPENHO].[dbo].[AGRUPAMENTO_EMPENHOS] AS A
	JOIN #CNPJ_RAIZ AS C 
		ON A.[CPF_CNPJ_CREDOR] LIKE CONCAT(C.CNPJ,'%')

