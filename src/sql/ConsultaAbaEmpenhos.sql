SELECT DISTINCT
	'EMPENHOS' AS CONSULTA,
	B.[ENTE] AS Ente,
	A.[ID_UG] AS IdUnidadeGestora,
	A.[Unidade] AS UnidadeGestora,
	B.[NU_CGC] AS CnpjUnidadeGestora,
	A.[NU_EMPENHO] AS NumeroEmpenho,
	A.[TP_EMPENHO] AS TipoEmpenho,
	A.[DT_EMPENHO] AS DataEmpenho,
	A.[Vl Empenhado] AS ValorEmpenhado,
	A.[AnulacaoEmpenho] AS ValorAnulacaoEmpenho,
	A.[Vl Liquidado] AS ValorLiquidado,
	A.[Valor Anulação Liquidação] AS ValorAnulacaoLiquidacao,
	A.[Valor SubEmpenho] AS ValorSubempenho,
	A.[Vl Pago] AS ValorPago,
	A.[Valor Retenção] AS ValorRetencao,
	A.[NU_LICITACAO] AS NumeroLicitacao,
	A.[HISTORICO] AS HistoricoEmpenho,
	A.[CREDOR] AS Credor,
	A.[CPF_CNPJ_CREDOR] AS CpfCnpjCredor,
	A.[ELEMENTOTCE] AS ElementoDespesaTCE,
	A.[DE_FONTE_TCE] AS FonteRecursoTCE,
	A.[NU_PROJ_ATIV] As NumeroProjetoAtividade,
	A.[DE_PROJATIV] AS ProjetoAtividade,
	A.[FUNCAO] AS Funcao,
	A.[SUBFUNCAO] AS Subfuncao,
	A.[DE_PROGRAMA] AS ProgramaTrabalho
FROM
	[IRIS_EMPENHO].[dbo].[IrisEmpenhoMunicipio] AS A 
	LEFT JOIN [IRIS_EMPENHO].[dbo].[IrisUnidadeGestora] AS B
		ON A.[ID_UG] = B.[CD_UNIDADE] 
	JOIN #CNPJ_RAIZ AS C 
		ON A.[CPF_CNPJ_CREDOR] LIKE CONCAT(C.CNPJ,'%')
