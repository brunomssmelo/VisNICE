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
  
  sample_data_rds <- read_rds('./dados/sample_data_Copia.rds')
  
  selected_nodes <- reactiveVal(NULL)
  ego_radius <- reactiveVal(1)
  sample_data <- reactiveVal(sample_data_rds)
  is_sample_data <- reactiveVal(TRUE)
  filter_start_date <- reactiveVal(min(sample_data_rds$a_socio$start, na.rm = T)[1])
  filter_start_date_serv <- reactiveVal(min(sample_data_rds$a_vinculo_servidor$start, na.rm = T)[1])
  filter_end_date <- reactiveVal(max(sample_data_rds$a_socio$end, na.rm = T)[1])
  filter_end_date_serv <- reactiveVal(max(sample_data_rds$a_vinculo_servidor$end, na.rm = T)[1])
  data_start_date <- reactiveVal(min(sample_data_rds$a_socio$start, na.rm = T)[1])
  data_start_date_serv <- reactiveVal(min(sample_data_rds$a_vinculo_servidor$start, na.rm = T)[1])
  data_end_date <- reactiveVal(max(sample_data_rds$a_socio$end, na.rm = T)[1])
  data_end_date_serv <- reactiveVal(max(sample_data_rds$a_vinculo_servidor$end, na.rm = T)[1])
  
  source("./src/server/proxy_viz_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/proxy_data_source_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/proxy_graph_structure_server.R", local = TRUE, encoding = "UTF-8")
})
