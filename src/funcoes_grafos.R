require(igraph)

ego_graph_nodes <- function(graph, order, center_nodes){
  
  graph_nodes <- igraph::as_data_frame(graph, what = 'vertices') %>% 
    rename(id = name)
  
  ego_graph <- make_ego_graph(graph, order = order,
                              nodes = center_nodes,
                              mode = "all")
  
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
  
  a_socio <- graph_data$a_socio
  a_parente <- graph_data$a_parente
  
  nodes <- v_empresa %>%
    bind_rows(v_socio_pj) %>%
    bind_rows(v_socio_pf) %>% 
    bind_rows(v_parente) %>%
    dummy_cols(select_columns = 'role') %>% 
    select(-role) %>% 
    group_by(id, title, group) %>%
    summarise(
      type = min(type),
      role_parente = sum(role_parente) > 0,
      role_socio = sum(role_socio) > 0,
      role_empresa = sum(role_empresa) > 0
    ) %>%
    ungroup() %>%
    unique()
  
  edges <- a_socio %>% 
    mutate(start = as.character(start),
           end = as.character(end),
           end = if_else(is.na(end), '...', end)) %>% 
    mutate(title = paste0("<p>", role, ":", start, " à ", end, "</p>"),
           color = 'blue')
  
  edges <- a_parente %>%
    mutate(title = role,
           color = 'red') %>%
    bind_rows(edges) %>%
    unique()
  
  graph <- graph_from_data_frame(d = edges,
                                 directed = TRUE,
                                 vertices = nodes)
  
  # center_nodes <- filter(nodes, role_empresa) %>% select(id) %>% unlist()
  center_nodes <- filter(nodes, type == 0) %>% select(id) %>% unlist()
  
  ledges <- data.frame(color = c("blue", "red"),
                       label = c("sócio", "parente"), arrows =c("to", "to"))
  
  list(graph = graph, ledges = ledges, center_nodes = center_nodes)
}