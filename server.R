require(tidyverse)
require(readr)
require(readxl)
require(writexl)
require(fastDummies)
require(igraph)
require(shiny)
require(shinyWidgets)
require(dashboardthemes)
require(shinyjs)
require(shinybusy)
require(visNetwork)

# https://www.statsandr.com/blog/how-to-embed-a-shiny-app-in-blogdown/

source('./src/funcoes_processamento.R', encoding = "UTF-8")
source('./src/funcoes_processamento_SQL.R', encoding = "UTF-8")
source('./src/funcoes_grafos.R', encoding = "UTF-8")

# data <- load_xlsx_nice('./dados/CruzamentoDados_0762020_SCE.xlsx')
# 
# data_nice$a_socio = data$a_socio
# data_nice$a_parente = data$a_parente
# 
# data_nice$v_parente = data$v_parente
# data_nice$v_empresa = data$v_empresa
# data_nice$v_socio_pj = data$v_socio_pj
# data_nice$v_socio_pf = data$v_socio_pf

shinyServer(function(input, output, session){
  
  sample_data_rds <- read_rds('./dados/sample_data.rds')
  
  selected_nodes <- reactiveVal(NULL)
  ego_radius <- reactiveVal(1)
  sample_data <- reactiveVal(sample_data_rds)
  is_sample_data <- reactiveVal(TRUE)
  
  # As datas iniciais e finais do filtro de sócios coincidirão com as datas
  # mínimas e máximas dos vínculos societários presentes em toda a base de dados
  filter_start_date <- reactiveVal(get_role_min_date(sample_data_rds, 'socio'))
  filter_end_date <- reactiveVal(get_role_max_date(sample_data_rds, 'socio'))
  
  # datas mínimas e máximas dos vínculos societários presentes em toda a base de
  # dados
  data_start_date <- reactiveVal(get_role_min_date(sample_data_rds, 'socio'))
  data_end_date <- reactiveVal(get_role_max_date(sample_data_rds, 'socio'))
  
  # As datas iniciais e finais do filtro de servidores coincidirão com as datas
  # mínimas e máximas dos vínculos empregatícios de servidores presentes em toda
  # a base de dados
  filter_start_date_serv <- reactiveVal(get_role_min_date(sample_data_rds, 'servidor'))
  filter_end_date_serv <- reactiveVal(get_role_max_date(sample_data_rds, 'servidor'))
  
  # datas mínimas e máximas dos vínculos empregatícios de servidores presentes
  # em toda a base de dados
  data_start_date_serv <- reactiveVal(get_role_min_date(sample_data_rds, 'servidor'))
  data_end_date_serv <- reactiveVal(get_role_max_date(sample_data_rds, 'servidor'))
  
  source("./src/server/proxy_viz_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/proxy_data_source_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/proxy_graph_structure_server.R", local = TRUE, encoding = "UTF-8")
})
