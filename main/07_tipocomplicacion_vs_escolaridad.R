# Tipo de complicación asociada vs Escolaridad



# First analysis ----------------------------------------------------------

# Plot: treemap-plot
# Treemap de estructura y distribución de valores por tipo de complicación & escolaridad
plot_treemap_tipocomplicacion_escolaridad <-
  data_main %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  count(ESCOLARIDAD, COMPLICACION_ACTUAL, name = 'Conteo', sort = TRUE) %>%
  transmute(Grupo = fct_inorder(str_c(COMPLICACION_ACTUAL, ESCOLARIDAD, sep = '\n\n')), Conteo) %>%
  filter(!is.na(Grupo)) %>%
  mutate(Porcentaje = (Conteo / sum(Conteo)) * 100) %>%
  ggplot(
    aes(
      area  = Porcentaje,
      fill  = Porcentaje,
      label = str_c(Grupo, str_c(round(Porcentaje, digits = 2), '%'), sep = '\n\n')
    )
  ) +
  geom_treemap(start = 'topleft') +
  geom_treemap_text(colour = "white", place = "centre", size = 10, start = 'topleft') +
  scale_fill_gradient(
    low    = "#56b0f6",
    high   = "#132a42",
    name   = 'Frecuencia',
    breaks = c(6, 12, 18, 24),
    labels = scales::percent(c(6, 12, 18, 24), scale = 1)
  )



# Test: χ2 de Pearson
# Variables: tipo de complicación & escolaridad
chi2_tipocomplicacion_escolaridad <-
  data_main %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  select(ESCOLARIDAD, COMPLICACION_ACTUAL) %>%
  table() %>%
  chisq.test()


table_chi2_tipocomplicacion_escolaridad <-
  tibble(
    !!chi2_tipocomplicacion_escolaridad$method := c(
      str_c(names(chi2_tipocomplicacion_escolaridad$statistic), " = ", round(chi2_tipocomplicacion_escolaridad$statistic, 3)),
      str_c(names(chi2_tipocomplicacion_escolaridad$parameter), " = ", round(chi2_tipocomplicacion_escolaridad$parameter, 3)),
      str_c("p-value",                                          " = ", round(chi2_tipocomplicacion_escolaridad$p.value, 3))
    )
  ) %>%
  datatable(
    rownames = FALSE,
    options  = list(ordering = FALSE, dom = "t", columnDefs = list(list(targets = 0, className = "dt-center" ))), width = "400px"
  )



# Second analysis ---------------------------------------------------------

# Plot 11
data_main %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  ggplot() +
  geom_bar(aes(N_COMPLICACION, fill = ESCOLARIDAD), position = 'dodge') +
  scale_fill_manual(name = 'Escolaridad', values = c('#3671a0', '#57b0f6')) +
  scale_x_continuous(breaks = c(0, 1, 2, 3), labels = c('NINGUNA', 'PRIMERA', 'SEGUNDA', 'TERCERA')) +
  ggtitle('Número de complicaciones por escolaridad') +
  labs(x = '', y = 'Conteo') +
  theme_light()


# Tests 1: Anova one-way
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     AÑOS DE TERAPIA
# Factores de bloque:    TIPO DE DIALISIS Y TIPO DE CUIDADOR
(
  data_main %>%
    mutate(
      ESCOLARIDAD = str_replace_all(
        ESCOLARIDAD, c(
          '^(SECUNDARIA)'   = 'MEDIA',
          '^(PREPARATORIA)' = 'MEDIA',
          '^(LICENCIATURA)' = 'SUPERIOR'
        )
      )
    ) %>%
    aov(N_COMPLICACION ~ ESCOLARIDAD, data = .) %>%
    summary()
)[[1]] %>%
  `row.names<-`(c('Escolaridad', 'Residuals'))

