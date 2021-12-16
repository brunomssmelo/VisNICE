require(tidyverse)
require(readr)
require(stringi)
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
require(shinydashboard)
require(dplyr)

# https://www.statsandr.com/blog/how-to-embed-a-shiny-app-in-blogdown/

source('./src/funcoes_carregamento.R', encoding = "UTF-8")
source('./src/funcoes_processamento.R', encoding = "UTF-8")
source('./src/funcoes_grafos.R', encoding = "UTF-8")

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
  select_cnpj<- reactiveVal(NULL)
  nos_selecionados <- reactiveVal(NULL)
  cnpj <- reactiveVal(NULL)
  
  # As datas iniciais e finais do filtro de sócios coincidirão com as datas
  # mínimas e máximas dos vínculos societários presentes em toda a base de dados
  filter_start_date <- reactiveVal(get_role_min_date(sample_data_rds, 'sociedade'))
  filter_end_date <- reactiveVal(get_role_max_date(sample_data_rds, 'sociedade'))
  #nos_selecionados <- reactiveVal(destaca_elementos(sample_data_rds,'PF'))
  # datas mínimas e máximas dos vínculos societários presentes em toda a base de
  # dados
  data_start_date <- reactiveVal(get_role_min_date(sample_data_rds, 'sociedade'))
  data_end_date <- reactiveVal(get_role_max_date(sample_data_rds, 'sociedade'))
  
  # As datas iniciais e finais do filtro de servidores coincidirão com as datas
  # mínimas e máximas dos vínculos empregatícios de servidores presentes em toda
  # a base de dados
  filter_start_date_serv <- reactiveVal(get_role_min_date(sample_data_rds, 'vinculo_empregaticio'))
  filter_end_date_serv <- reactiveVal(get_role_max_date(sample_data_rds, 'vinculo_empregaticio'))
  
  # datas mínimas e máximas dos vínculos empregatícios de servidores presentes
  # em toda a base de dados
  data_start_date_serv <- reactiveVal(get_role_min_date(sample_data_rds, 'vinculo_empregaticio'))
  data_end_date_serv <- reactiveVal(get_role_max_date(sample_data_rds, 'vinculo_empregaticio'))
  
  # datas mínimas e máximas dos empenhos presentes
  # em toda a base de dados
  data_start_date_emp <- reactiveVal(get_role_min_date(sample_data_rds, 'empenho'))
  data_end_date_emp <- reactiveVal(get_role_max_date(sample_data_rds, 'empenho'))
  
  # As datas iniciais e finais do filtro de empenhos coincidirão com as datas
  # mínimas e máximas dos empenhos presentes em toda
  # a base de dados
  filter_start_date_emp <- reactiveVal(get_role_min_date(sample_data_rds, 'empenho'))
  filter_end_date_emp <- reactiveVal(get_role_max_date(sample_data_rds, 'empenho'))
  
  source("./src/server/proxy_viz_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/proxy_data_source_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/proxy_graph_structure_server.R", local = TRUE, encoding = "UTF-8")
})
