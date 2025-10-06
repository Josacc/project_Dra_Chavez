# Tipo de complicación asociada vs Tipo de diálisis



# First analysis ----------------------------------------------------------

# Tabla de contingencia
# Variables: tipo de complicación & diálisis
table_cont_tipocomplicacion_dialisis <-
  data_main %>%
  select(DIALISIS, COMPLICACION_ACTUAL) %>%
  table() %>%
  as.data.frame.matrix() %>% ##
  mutate(across(everything(), as.character)) %>%
  map2_df(
    .y = (
      data_main %>%
        select(DIALISIS, COMPLICACION_ACTUAL) %>%
        table() %>%
        prop.table() %>%
        round(4) %>%
        `*`(100) %>%
        as.data.frame.matrix() %>%
        mutate(across(everything(), ~ str_c(" ", "(", .x, "%", ")")))
    ),
    ~str_c(.x, .y),
    .id = "colname"
  ) %>%
  datatable(
    rownames = c("AUTOMATIZADA", "MANUAL"),
    class    = "compact stripe cell-border hover row-border",
    options  = list(
      ordering     = FALSE,
      dom          = "t",
      columnDefs   = list(list(targets = c(1:6), className = "dt-center" )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().node()).css({'font-size': '12px'});",
        "}"
      )
    )
  )



# Plot: treemap-plot
# Frecuencia de Tipo de complicaciones & Tipo de diálisis
plot_treemap_complicacion_dialisis <-
  data_main %>%
  count(DIALISIS, COMPLICACION_ACTUAL, name = 'Conteo', sort = TRUE) %>%
  transmute(
    Grupo = fct_inorder(
      str_c(str_c("Complicación: ", COMPLICACION_ACTUAL), str_c("Diálisis: ", DIALISIS), sep = '\n\n')
    ),
    Conteo
  ) %>%
  filter(!is.na(Grupo)) %>%
  mutate(Porcentaje = (Conteo / sum(Conteo)) * 100) %>%
  ggplot(
    aes(
      area  = Porcentaje,
      fill  = Porcentaje,
      label = str_c(Grupo, str_c(round(Porcentaje, digits = 2), '%'), sep = '\n\n')
    )
  ) +
  geom_treemap(start = 'topleft', show.legend = FALSE) +
  geom_treemap_text(colour = "white", place = "centre", size = 10, start = 'topleft') +
  scale_fill_gradient(
    low    = "#56b0f6",
    high   = "#132a42"
  )



# Test: χ2 de Pearson
# Variables: tipo de complicación & tipo de diálisis
set.seed(1)
chi2_tipodialisis_tipocomplicacion <-
  data_main %>%
  select(DIALISIS, COMPLICACION_ACTUAL) %>%
  table() %>%
  chisq.test()


table_chi2_tipodialisis_tipocomplicacion <-
  tibble(
    !!chi2_tipodialisis_tipocomplicacion$method := c(
      str_c(names(chi2_tipodialisis_tipocomplicacion$statistic), " = ", round(chi2_tipodialisis_tipocomplicacion$statistic, 3)),
      str_c(names(chi2_tipodialisis_tipocomplicacion$parameter), " = ", round(chi2_tipodialisis_tipocomplicacion$parameter, 3)),
      str_c("p-value",                                           " = ", round(chi2_tipodialisis_tipocomplicacion$p.value, 3))
    )
  ) %>%
  datatable(
    rownames = FALSE,
    options  = list(ordering = FALSE, dom = "t", columnDefs = list(list(targets = 0, className = "dt-center" ))), width = "400px"
  )



# Plot: bar-plot
# Frequencia de complicaciones por tipo de diálisis
plot_freq_dialisis <-
  data_main %>%
  ggplot(aes(N_COMPLICACION)) +
  geom_bar(aes(fill = DIALISIS), position = 'dodge') +
  scale_fill_jama(name = "Tipo de diálisis") +
  scale_x_continuous(breaks = c(0, 1, 2, 3), labels = c('NINGUNA', 'PRIMERA', 'SEGUNDA', 'TERCERA')) +
  scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  labs(x = 'Número de complicaciones', y = 'Frecuencia') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  )


# Second analysis ---------------------------------------------------------


# Plot 4
data_main %>%
  ggplot() +
  geom_boxplot(aes(DIALISIS, N_COMPLICACION, fill = DIALISIS)) +
  scale_fill_manual(name = 'Tipo de diálisis', values = c('#b81466', '#252850')) +
  ggtitle('Comparación, sexo vs tipo de diálisis') +
  ylab('Numero de complicaciones') +
  xlab('Tipo de diálisis') +
  theme_light()


# Test 1: t-test
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     TIPO DE DIÁLISIS

data_main %>%
  t.test(formula = N_COMPLICACION ~ DIALISIS, data = ., var.equal = TRUE) %>%
  tidy() %>%
  select(statistic, parameter, p.value, method) %>%
  rename(df         = parameter) %>%
  mutate(method     = 't_test') %>%
  mutate(conf.level = 0.95) %>%
  mutate(across(where(is_double), ~ round(.x, 3)))


# Test 2: Anova one-way and DBCA
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     TIPO DE DIALISIS
# Factores de bloque:    SEXO & TIPO DE CUIDADOR
(
  data_main %>%
    aov(N_COMPLICACION ~ DIALISIS + SEXO + as.character(CUIDADOR), data = .) %>%
    summary()
)[[1]] %>%
  `row.names<-`(c('Tipo de diálisis', 'Sexo', 'Tipo de cuidador', 'Residuals'))


