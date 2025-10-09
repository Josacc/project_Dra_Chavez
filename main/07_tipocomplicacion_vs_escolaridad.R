# Tipo de complicación asociada vs Escolaridad



# First analysis ----------------------------------------------------------

# Tabla de contingencia
# Variables: tipo de complicación & escolaridad
table_cont_tipocomplicacion_escolaridad <-
  data_main %>%
  select(ESCOLARIDAD, COMPLICACION_ACTUAL) %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  table() %>%
  as.data.frame.matrix() %>%
  mutate(across(everything(), as.character)) %>%
  map2_df(
    .y = (
      data_main %>%
        select(ESCOLARIDAD, COMPLICACION_ACTUAL) %>%
        mutate(
          ESCOLARIDAD = str_replace_all(
            ESCOLARIDAD, c(
              '^(SECUNDARIA)'   = 'MEDIA',
              '^(PREPARATORIA)' = 'MEDIA',
              '^(LICENCIATURA)' = 'SUPERIOR'
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
    rownames = c("MEDIA", "SUPERIOR"),
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
  transmute(
    Grupo = fct_inorder(
      str_c(str_c("Complicación: ", COMPLICACION_ACTUAL), str_c("Escolaridad: ", ESCOLARIDAD), sep = '\n\n')
    ),
    Conteo
  ) %>%
  filter(!is.na(Grupo)) %>%
  mutate(Porcentaje = (Conteo / sum(Conteo)) * 100) %>%
  filter(Porcentaje > 6) %>%
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
# Variables: tipo de complicación & escolaridad
set.seed(1)
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

