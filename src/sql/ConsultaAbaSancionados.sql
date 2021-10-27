SELECT DISTINCT 'SANÇÕES' AS CONSULTA
  , [TIPO DE PESSOA] AS TipoPessoa
  , [CPF OU CNPJ DO SANCIONADO formatado] AS CpfCnpjSancionado
  , [NOME INFORMADO PELO ÓRGÃO SANCIONADOR] AS NomeOrgaoSancionador
  , [NOME FANTASIA - CADASTRO RECEITA] AS NomeFantasia
  , [NÚMERO DO PROCESSO] AS NumeroProcesso
  , [TIPO SANÇÃO] AS TipoSancao
  , [DATA INÍCIO SANÇÃO] AS DataInicioSancao
  , [DATA FINAL SANÇÃO] AS DataFimSancao
  , [ÓRGÃO SANCIONADOR] AS OrgaoSancionador
  , [ORIGEM INFORMAÇÕES] AS OrigemInfo
  , [DATA PUBLICAÇÃO] AS DataPublicacao
  , [PUBLICAÇÃO] AS Publicacao
  , [DETALHAMENTO] AS Detalhamento
  , [ABRAGÊNCIA DEFINIDA EM DECISÃO JUDICIAL] AS AbrangenciaDecisaoJudicial
  , [FUNDAMENTAÇÃO LEGAL] AS FundamentacaoLegal
  , [DESCRIÇÃO DA FUNDAMENTAÇÃO LEGAL] AS DescricaoFundamentacaoLegal
  , [OBSERVAÇÕES] AS Observacoes
  , [ESFERA] AS Esfera
  , [Unidade] AS Unidade
  , [NUMERO EMPENHO] AS NumeroEmpenho
  , [TIPO EMPENHO] AS TipoEmpenho
  , [DATA EMPENHO] AS DataEmpenho
  , [Valor  Empenho] AS ValorEmpenho
  , [Vl Pago] AS ValorPago
  , [CREDOR] AS Credor
  , [CNPJ] AS CnpjSancao
FROM
  [IRIS_EMPENHO].[dbo].[resultadoTipologia_ceisPessoaJuridicaMunicipios]
  JOIN #CNPJ_RAIZ AS C ON [CPF OU CNPJ DO SANCIONADO] like CONCAT(C.CNPJ,'%')
