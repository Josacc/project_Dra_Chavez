# Tipo de complicación asociada vs Sexo



# First analysis ----------------------------------------------------------

# Tabla de contingencia
# Variables: tipo de complicación & sexo
table_cont_tipocomplicacion_sexo <-
  data_main %>%
  select(SEXO, COMPLICACION_ACTUAL) %>%
  mutate(
    SEXO = str_replace_all(
      SEXO, c(
        '^F$' = 'MUJER',
        '^M$' = 'HOMBRE'
      )
    )
  ) %>%
  table() %>%
  as.data.frame.matrix() %>%
  mutate(across(everything(), as.character)) %>%
  map2_df(
    .y = (
      data_main %>%
        select(SEXO, COMPLICACION_ACTUAL) %>%
        mutate(
          SEXO = str_replace_all(
            SEXO, c(
              '^F$' = 'MUJER',
              '^M$' = 'HOMBRE'
            )
          )
        ) %>%
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
    rownames = c("HOMBRE", "MUJER"),
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



# Test: χ2 de Pearson
# Variables: tipo de complicación & sexo
chi2_tipocomplicacion_sexo <-
  data_main %>%
  select(SEXO, COMPLICACION_ACTUAL) %>%
  table() %>%
  chisq.test()


table_chi2_tipocomplicacion_sexo <-
  tibble(
    !!chi2_tipocomplicacion_sexo$method := c(
      str_c(names(chi2_tipocomplicacion_sexo$statistic), " = ", round(chi2_tipocomplicacion_sexo$statistic, 3)),
      str_c(names(chi2_tipocomplicacion_sexo$parameter), " = ", round(chi2_tipocomplicacion_sexo$parameter, 3)),
      str_c("p-value",                                   " = ", round(chi2_tipocomplicacion_sexo$p.value, 3))
    )
  ) %>%
  datatable(
    rownames = FALSE,
    options  = list(ordering = FALSE, dom = "t", columnDefs = list(list(targets = 0, className = "dt-center" ))),
    width    = "400px"
  )



# Plot: treemap-plot
# Treemap de estructura y distribución de valores del tipo de complicación & sexo
plot_treemap_tipocomplicacion_sexo <-
  data_main %>%
  count(SEXO, COMPLICACION_ACTUAL, name = 'Conteo', sort = TRUE) %>%
  mutate(
    SEXO = str_replace_all(
      SEXO, c(
        '^F$' = 'MUJER',
        '^M$' = 'HOMBRE'
      )
    )
  ) %>%
  transmute(
    Grupo = fct_inorder(
      str_c(str_c("Complicación: ", COMPLICACION_ACTUAL), str_c("Sexo: ", SEXO), sep = '\n\n')
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
    high   = "#132a42",
    name   = 'Frecuencia',
    breaks = c(4, 8, 12, 16),
    labels = scales::percent(c(4, 8, 12, 16), scale = 1)
  ) +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal"
  )



# Second analysis ---------------------------------------------------------


# Plot 1
data_main %>%
  ggplot(aes(N_COMPLICACION)) +
  geom_bar(aes(fill = SEXO), position = 'dodge') +
  facet_grid(col = vars(DIALISIS)) +
  scale_fill_manual(name = 'Sexo', values = c('#b81466', '#252850')) +
  scale_x_continuous(breaks = c(0, 1, 2, 3), labels = c('NINGUNA', 'PRIMERA', 'SEGUNDA', 'TERCERA')) +
  scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  ggtitle('Número de complicaciones por sexo') +
  labs(x = '', y = 'Conteo') +
  theme_light()


# Plot 2
data_main %>%
  ggplot() +
  geom_boxplot(aes(SEXO, N_COMPLICACION, fill = SEXO)) +
  scale_fill_manual(name = 'Sexo', values = c('#b81466', '#252850')) +
  ggtitle('Comparación, sexo vs complicaciones') +
  ylab('Numero de complicaciones') +
  xlab('Sexo') +
  theme_light()


# Test 1: t-test
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     SEXO

data_main %>%
  t.test(formula = N_COMPLICACION ~ SEXO, data = ., var.equal = TRUE) %>%
  tidy() %>%
  select(statistic, parameter, p.value, method) %>%
  rename(df         = parameter) %>%
  mutate(method     = 't_test') %>%
  mutate(conf.level = 0.95) %>%
  mutate(across(where(is_double), ~ round(.x, 3)))
