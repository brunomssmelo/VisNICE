data_path <- './dados/dados_grafos_sql.xlsx'

sample_data <- load_data(data_path)

# sample_data$edges <- sample_data$edges %>%
#   mutate(from = `str_sub<-`(from, 2, 4, value = '***')) %>%
#   mutate(to = `str_sub<-`(to, 2, 4, value = '***'))
# 
# sample_data$nodes <- sample_data$nodes %>%
#   mutate(id = `str_sub<-`(id, 2, 4, value = '***')) %>% 
#   mutate(title = `str_sub<-`(title, 1, as.integer(0.05*nchar(title)), value = '***'),
#          title = `str_sub<-`(title, -as.integer(0.05*nchar(title)), -1, value = '***'))

saveRDS(sample_data, './dados/sample_data.rds')