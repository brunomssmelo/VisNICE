require(igraph)

check_filter_interval <- function(edge_data, filter_start, filter_end, filter_type){
  
  edge_data <- edge_data %>%
    mutate(
      start = if_else(is.na(start), true = as.character(filter_start), false = start),
      end = if_else(is.na(end),
                    true = as.character(filter_end), false = end),
      end = if_else((end=='Tempo indefinido'),
                    true = as.character(filter_end), false = end)) %>%
    mutate(
      ok = case_when(
        ((type == filter_type) & (parse_date(start)>filter_end)) ~ F,
        ((type == filter_type) & (parse_date(end)<filter_start)) ~ F,
        T ~ T))
  
  edge_data$ok
}

get_role_max_date <- function(graph_data, edge_type){
  graph_data$edges %>% 
    filter(str_detect(type, edge_type)) %>%
    `$`(end) %>% max(na.rm = T)
}

get_role_min_date <- function(graph_data, edge_type){
  graph_data$edges %>% 
    filter(str_detect(type, edge_type)) %>%
    `$`(start) %>% min(na.rm = T)
}

ego_graph <- function(graph, order, center_nodes){
  
  ego_graph <- make_ego_graph(graph, order = order,
                              nodes = center_nodes,
                              mode = "all")
  
  # first convert the list of igraphs to list of data.frames
  ego_graph <- lapply(ego_graph, igraph::as_data_frame)
  
  # then combine all the data.frames in the list into one data.frame
  ego_graph <- do.call(rbind, ego_graph)
  
  # then make a graph out of the one combined data.frame
  ego_graph <- graph_from_data_frame(ego_graph)
  
  # ego_nodes <- unique(rownames(igraph::as_data_frame(ego_graph, what = 'vertices')))
  # 
  # ego_nodes
}

build_source_graph <- function(graph_data){
  
  nodes <- graph_data$nodes
  
  edges <- graph_data$edges
  
  data <- graph_data$data
  
  edges <- edges %>%
    mutate(start = as.character(start),
           end = as.character(end)) %>%
    mutate(end = case_when(
      !is.na(start) ~ if_else(is.na(end), 'Tempo indefinido', end),
      T ~ end)) %>% # adicionar  no titulo
    mutate(title = case_when(
      !is.na(start) & type != 'empenho' ~ paste0("<p>", role, ":", start, " à ", end, "</p>"),
      T ~ role))%>%
    mutate(color = case_when(
      type == 'sociedade' ~ 'blue',
      type == 'vinculo_empregaticio' ~ 'purple',
      type == 'parentesco' ~ 'red',
      type == 'telefone_empresa' ~ 'E5C039',
      type == 'empenho' ~ 'black',
      T ~ 'black'))
  
  nodes <- nodes %>%
    mutate(title = case_when(
      role == 'vermelho'~ paste('<p style=color:red;><strong>Empresa_Sancionada: ', title, "</strong></p>"),
      T ~ title
    ))

  graph <- graph_from_data_frame(d = edges,
                                 directed = TRUE,
                                 vertices = nodes)
  
  # Nós centrais serão, a priori, as PJ solicitadas
  # center_nodes <- nodes %>%
  # filter(level==0, str_detect(group, 'PJ')) %>%
  # select(id) %>%
  # unlist()
  filtro<- case_when(str_detect(nodes$group,'PJ_PRIVADO')~nodes$id)
  
  filtro<-filtro[!is.na(filtro)]
  
  center_nodes <- nodes %>%
    filter(id %in% filtro, str_detect(group, 'PJ')) %>%
    select(id) %>%
    unlist()
  
  
  #center_nodes <- filter(nodes, type == 0) %>% select(id) %>% unlist() #<---- Não
  
  ledges <- data.frame(color = c("blue", "red", "purple", "E5C039", "black"),
                       label = c("sócio", "parente", "vinc_servidor", "telefones", "empenhos"), arrows =c("to","to", "to", "to","to"))
  
  list(graph = graph, ledges = ledges, center_nodes = center_nodes, data = data)
}


