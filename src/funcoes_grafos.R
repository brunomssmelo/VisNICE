require(igraph)

get_role_max_date <- function(graph_data, edge_role){
  graph_data$edges %>% 
    filter(str_detect(role, edge_role)) %>%
    `$`(end) %>% max(na.rm = T)
}

get_role_min_date <- function(graph_data, edge_role){
  graph_data$edges %>% 
    filter(str_detect(role, edge_role)) %>%
    `$`(end) %>% min(na.rm = T)
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
  
  # nodes <- nodes %>% 
  #   dummy_cols(select_columns = 'role') %>% 
  #   select(-role) %>% 
  #   group_by(id, title, group) %>%
  #   summarise(
  #     type = min(type),
  #     role_parente = sum(role_parente) > 0,
  #     role_socio = sum(role_socio) > 0,
  #     role_empresa = sum(role_empresa) > 0,
  #     role_servidor = sum(role_servidor) > 0,
  #     role_orgao_publico = sum(role_orgao_publico) > 0
  #   ) %>%
  #   ungroup() %>%
  #   filter(!duplicated(id))%>%
  #   unique()
  
 #nodes2 <- nodes %>% aggregate(nodes$title ~ nodes$id, FUN = max)
    #group_by(nodes$id, nodes$title, nodes$group)%>%summarise_each(funs(max))
  
  
  edges <- edges %>%
    mutate(start = as.character(start),
           end = as.character(end)) %>% 
    mutate(end = case_when(
      !is.na(start) ~ if_else(is.na(end), 'Tempo indefinido', end),
      T ~ end)) %>% 
    mutate(title = case_when(
      !is.na(start) ~ paste0("<p>", role, ":", start, " à ", end, "</p>"),
      T ~ role)) %>% 
    mutate(color = case_when(
      type == 'sociedade' ~ 'blue',
      type == 'vinculo_empregaticio' ~ 'purple',
      type == 'parentesco' ~ 'red',
      type == 'telefone_empresa' ~ 'E5C039',
      T ~ 'black'))

  graph <- graph_from_data_frame(d = edges,
                                 directed = TRUE,
                                 vertices = nodes)

  # Nós centrais serão, a priori, as PJ solicitadas
  center_nodes <- nodes %>% 
    filter(level == 0, str_detect(group, 'PJ')) %>%
    select(id) %>%
    unlist()
  
  # center_nodes <- filter(nodes, type == 0) %>% select(id) %>% unlist() <---- Não

  ledges <- data.frame(color = c("blue", "red", "purple", "E5C039"),
                       label = c("sócio", "parente", "vinc_servidor", "telefones"), arrows =c("to", "to", "to","to"))
  
  list(graph = graph, ledges = ledges, center_nodes = center_nodes)
}
