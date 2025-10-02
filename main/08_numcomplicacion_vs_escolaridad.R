# Cantidad de complicaciones vs Escolaridad



# First analysis ----------------------------------------------------------


# Test: χ2 de Pearson
# Variables: número de complicaciones & escolaridad
chi2_numcomplicacion_escolaridad <-
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
  select(N_COMPLICACION, ESCOLARIDAD) %>%
  table() %>%
  chisq.test()


table_chi2_numcomplicacion_escolaridad <-
  tibble(
    !!chi2_numcomplicacion_escolaridad$method := c(
      str_c(names(chi2_numcomplicacion_escolaridad$statistic), " = ", round(chi2_numcomplicacion_escolaridad$statistic, 3)),
      str_c(names(chi2_numcomplicacion_escolaridad$parameter), " = ", round(chi2_numcomplicacion_escolaridad$parameter, 3)),
      str_c("p-value",                                         " = ", round(chi2_numcomplicacion_escolaridad$p.value, 3))
    )
  ) %>%
  datatable(
    rownames = FALSE,
    options  = list(ordering = FALSE, dom = "t", columnDefs = list(list(targets = 0, className = "dt-center" ))), width = "400px"
  )


# Plot: plot-mosaic
# Mosaico de frecuencia combinada: tiempo de terapia & numero de complicaciones
plot_mosaic_numcomplicaciones_escolaridad <-
  data_main %>%
  mutate(N_COMPLICACION = as.character(N_COMPLICACION)) %>%
  mutate(
    N_COMPLICACION = str_replace_all(
      N_COMPLICACION, c(
        '0' = 'NINGUNA',
        '1' = 'PRIMERA',
        '2' = 'SEGUNDA',
        '3' = 'TERCERA'
      )
    )
  ) %>%
  mutate(
    N_COMPLICACION = factor(N_COMPLICACION, levels = c('NINGUNA', 'PRIMERA', 'SEGUNDA', 'TERCERA'))
  ) %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  mutate(
    ESCOLARIDAD = factor(ESCOLARIDAD, levels = c('MEDIA', 'SUPERIOR'))
  ) %>%
  ggplot() +
  geom_mosaic(aes(x = product(ESCOLARIDAD, N_COMPLICACION), fill = ESCOLARIDAD), show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = 'Número de complicaciones asociadas', y = 'Escolaridad') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 7),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 30)
  )


# Plot: treemap-plot
# Treemap de estructura y distribución de valores por número de complicaciones & educación
plot_treemap_numcomplicaciones_escolaridad <-
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
  mutate(N_COMPLICACION = as.character(N_COMPLICACION)) %>%
  mutate(
    N_COMPLICACION = str_replace_all(
      N_COMPLICACION, c(
        '0' = 'NINGUNA',
        '1' = 'PRIMERA',
        '2' = 'SEGUNDA',
        '3' = 'TERCERA'
      )
    )
  ) %>%
  count(ESCOLARIDAD, N_COMPLICACION, name = 'Conteo', sort = TRUE) %>%
  transmute(
    Grupo = fct_inorder(
      str_c(str_c("Escolaridad: ", ESCOLARIDAD), str_c("Número de complicación: ", N_COMPLICACION), sep = '\n\n')
    ),
    Conteo
  ) %>%
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
    breaks = c(9, 18, 27),
    labels = scales::percent(c(9, 18, 27), scale = 1)
  )
