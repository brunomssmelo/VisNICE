require(igraph)

ego_graph_nodes <- function(graph, order, center_nodes){
  
  graph_nodes <- igraph::as_data_frame(graph, what = 'vertices') %>% 
    rename(id = name)
  
  ego_graph <- make_ego_graph(graph, order = order,
                              nodes = center_nodes,
                              mode = "all")
  # print(ego_graph)
  # first convert the list of igrpahs to list of data.frames
  ego_graph <- lapply(ego_graph, igraph::as_data_frame)
  # then combine all the data.frames in the list into one data.frame
  ego_graph <- do.call(rbind, ego_graph)
  # then make a graph out of the one combined data.frame
  ego_graph <- graph_from_data_frame(ego_graph)

  unique(rownames(igraph::as_data_frame(ego_graph, what = 'vertices')))
}

build_source_graph <- function(graph_data){
  
  v_empresa <- graph_data$v_empresa
  v_socio_pj <- graph_data$v_socio_pj
  v_socio_pf <- graph_data$v_socio_pf
  v_parente <- graph_data$v_parente
  v_servidor <- graph_data$v_servidor
  v_orgao_publico <- graph_data$v_orgao_publico
  v_empresa_socio <- graph_data$v_empresa_socio
  v_telefones <- graph_data$v_telefones
  v_cnpj <- graph_data$v_cnpj
  
  a_socio <- graph_data$a_socio
  a_parente <- graph_data$a_parente
  a_vinculo_servidor <- graph_data$a_vinculo_servidor
  a_tel_empresa <- graph_data$a_tel_empresa
  
  nodes <- v_empresa %>%
    bind_rows(v_socio_pj) %>%
    bind_rows(v_socio_pf) %>% 
    bind_rows(v_parente) %>%
    bind_rows(v_servidor)%>%
    bind_rows(v_orgao_publico)%>%
    bind_rows(v_empresa_socio)%>%
    bind_rows(v_telefones)%>%
    bind_rows(v_cnpj)%>%
    dummy_cols(select_columns = 'role') %>% 
    select(-role) %>% 
    group_by(id, title, group) %>%
    summarise(
      type = min(type),
      role_parente = sum(role_parente) > 0,
      role_socio = sum(role_socio) > 0,
      role_empresa = sum(role_empresa) > 0,
      role_servidor = sum(role_servidor) > 0,
      role_orgao_publico = sum(role_orgao_publico) > 0
      # role_telefones = sum(role_telefones) > 0
    ) %>%
    ungroup() %>%
    filter(!duplicated(id))%>%
    unique()
  
 #nodes2 <- nodes %>% aggregate(nodes$title ~ nodes$id, FUN = max)
    #group_by(nodes$id, nodes$title, nodes$group)%>%summarise_each(funs(max))
    
  edges <- a_socio %>% 
    mutate(start = as.character(start),
           end = as.character(end),
           end = if_else(is.na(end), 'Tempo indefinido', end)) %>% 
    mutate(title = paste0("<p>", role, ":", start, " à ", end, "</p>"),
           color = 'blue') %>%
  unique()
  
  edges_parent <- a_vinculo_servidor %>%
    mutate(start = as.character(start),
           end = as.character(end),
           end = if_else(is.na(end), 'Tempo indefinido', end)) %>%
    mutate(title = paste0("<p>", role, ":", start, " à ", end, "</p>"),
           color = 'purple') %>%
    unique()
  
  edges_tel <- a_tel_empresa %>%
    mutate(color = '#E5C039')%>%
    unique()

  edges <- a_parente %>%
    mutate(title = role,
           color = 'red') %>%
    bind_rows(edges, edges_parent, edges_tel) %>%
    unique()
  
  graph <- graph_from_data_frame(d = edges,
                                 directed = TRUE,
                                 vertices = nodes)
  
  # center_nodes <- filter(nodes, role_empresa) %>% select(id) %>% unlist()
  center_nodes <- filter(nodes, type == 0) %>% select(id) %>% unlist()
  
  ledges <- data.frame(color = c("blue", "red", "purple", "#E5C039"),
                       label = c("sócio", "parente", "vinc_servidor", "telefones"), arrows =c("to", "to", "to", "to"))
  
  list(graph = graph, ledges = ledges, center_nodes = center_nodes)
}
