

x <- data_main %>%
  select(SEXO, DIALISIS) %>%
  table() %>%
  fisher.test()

x
