# Número de complicaciones vs Años de terapia



# First analysis ----------------------------------------------------------

# Tabla de contingencia
# Variables: tiempo de terapia & número de complicaciones
table_cont_tiempoterapia_numcomplicaciones <-
  data_main %>%
  select(N_COMPLICACION, ANOS_TERAPIA) %>%
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
    N_COMPLICACION = factor(N_COMPLICACION, levels = c('TERCERA', 'SEGUNDA', 'PRIMERA', 'NINGUNA'))
  ) %>%
  mutate(ANOS_TERAPIA = fct(as.character(ANOS_TERAPIA), levels = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "17"))) %>%
  table() %>%
  as.data.frame.matrix() %>%
  mutate(across(everything(), as.character)) %>%
  map2_df(
    .y = (
      data_main %>%
        select(N_COMPLICACION, ANOS_TERAPIA) %>%
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
          N_COMPLICACION = factor(N_COMPLICACION, levels = c('TERCERA', 'SEGUNDA', 'PRIMERA', 'NINGUNA'))
        ) %>%
        mutate(ANOS_TERAPIA = fct(as.character(ANOS_TERAPIA), levels = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "17"))) %>%
        table() %>%
        prop.table() %>%
        round(3) %>%
        `*`(100) %>%
        as.data.frame.matrix() %>%
        mutate(across(everything(), ~ str_c(" ", "(", .x, "%", ")")))
    ),
    ~str_c(.x, .y),
    .id = "colname"
  ) %>%
  datatable(
    rownames = c('TERCERA', 'SEGUNDA', 'PRIMERA', 'NINGUNA'),
    class    = "compact stripe cell-border hover row-border",
    options  = list(
      ordering     = FALSE,
      dom          = "t",
      columnDefs   = list(list(targets = c(1:10), className = "dt-center" )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().node()).css({'font-size': '12px'});",
        "}"
      )
    )
  )



# Plot: mosaic
# Mosaico de frecuencia combinada: tiempo de terapia & número de complicaciones
plot_mosaic_timpoterapia_complicaciones <-
  data_main %>%
  mutate(ANOS_TERAPIA = fct(as.character(ANOS_TERAPIA), levels = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "17"))) %>%
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
  filter(!is.na(ANOS_TERAPIA)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(N_COMPLICACION, ANOS_TERAPIA), fill = N_COMPLICACION), show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = 'Tiempo de terapia sustitutiva (años)', y = 'Número de complicaciones asociadas') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 7),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 30)
  )



# Test: χ2 de Pearson
# Variables: tiempo de terapia & número de complicaciones
set.seed(1)
chi2_tiempo_complicacion <-
  data_main %>%
  select(ANOS_TERAPIA, N_COMPLICACION) %>%
  table() %>%
  chisq.test()


table_chi2_tiempo_complicacion <-
  tibble(
    !!chi2_tiempo_complicacion$method := c(
      str_c(names(chi2_tiempo_complicacion$statistic), " = ", round(chi2_tiempo_complicacion$statistic, 3)),
      str_c(names(chi2_tiempo_complicacion$parameter), " = ", round(chi2_tiempo_complicacion$parameter, 3)),
      str_c("p-value",                                     " = ", round(chi2_tiempo_complicacion$p.value, 3))
    )
  ) %>%
  datatable(
    rownames = FALSE,
    options  = list(ordering = FALSE, dom = "t", columnDefs = list(list(targets = 0, className = "dt-center" ))), width = "400px"
  )



# Second analysis ---------------------------------------------------------

# Plot: box-plot
# Tiempo de terapia vs Cantidad de complicaciones
# plot_t_terapia_vs_complicacion <-
  data_main %>%
  mutate(ANOS_TERAPIA = as.character(ANOS_TERAPIA)) %>%
  mutate(ANOS_TERAPIA = fct(ANOS_TERAPIA, levels = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "17"))) %>%
  filter(!is.na(ANOS_TERAPIA)) %>%
  ggplot(aes(ANOS_TERAPIA, N_COMPLICACION, fill = ANOS_TERAPIA)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  scale_y_continuous(breaks = c(0, 1, 2, 3), labels = c("cero", "una", "dos", "tres")) +
  ggtitle('Tiempo de terapia vs Cantidad de complicaciones') +
  labs(x = 'Tiempo de terapia sustitutiva (años)', y = 'Cantidad de complicaciones asociadas') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 7),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 30)
  )



# Test: ANOVA one-way
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     AÑOS DE TERAPIA
anova_t_terapia_vs_complicacion <-
  (
    data_main %>%
      aov(N_COMPLICACION ~ as.character(ANOS_TERAPIA), data = .) %>%
      summary()
  )[[1]] %>%
  `row.names<-`(c('Años de terapia', 'Residuals')) %>%
  round(3) %>%
  as_tibble(rownames = NA) %>%
  datatable(options = list(ordering = FALSE, dom = "t"))



# Thrird analysis ----------------------------------------------------------

# Plot
data_main %>%
  ggplot(aes(as.character(N_COMPLICACION), ANOS_TERAPIA, fill = as.character(N_COMPLICACION))) +
  geom_boxplot(na.rm = TRUE, show.legend = FALSE) +
  scale_fill_lancet() +
  scale_x_discrete(breaks = c("0", "1", "2", "3") , labels = c("cero", "una", "dos", "tres")) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  ggtitle('Cantidad de complicaciones vs tiempo de terapia') +
  labs(x = 'Cantidad de complicaciones asociadas', y = 'Tiempo de terapia sustitutiva (años)') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 7),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 30)
  )


# Sexo: Masculino & Femenino
# Plot
data_main_2 %>%
  ggplot(aes(ANOS_TERAPIA, N_COMPLICACION, fill = N_COMPLICACION)) +
  geom_boxplot(na.rm = TRUE, show.legend = FALSE) +
  scale_fill_lancet() +
  facet_grid(cols = vars(SEXO)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  ggtitle('Complicaciones vs años de terapia') +
  labs(x = 'Años de terapia', y = '', fill = 'Complicación') +
  theme_light()


# Tipo de diálisis: Manual & Automatizada
# Plot
data_main %>%
  ggplot(aes(as.character(N_COMPLICACION), ANOS_TERAPIA, fill = as.character(N_COMPLICACION))) +
  geom_boxplot(na.rm = TRUE, show.legend = FALSE) +
  scale_fill_lancet() +
  scale_x_discrete(breaks = c("0", "1", "2", "3") , labels = c("cero", "una", "dos", "tres")) +
  facet_grid(cols = vars(DIALISIS)) +
  ggtitle('Cantidad de complicaciones vs tiempo de terapia') +
  labs(x = 'Cantidad de complicaciones asociadas', y = 'Tiempo de terapia sustitutiva (años)') +
  theme_light() +
  theme(
    axis.title.x = element_text(vjust = -1)
  )

data_main %>%
  mutate(ANOS_TERAPIA = as.character(ANOS_TERAPIA)) %>%
  mutate(ANOS_TERAPIA = fct(ANOS_TERAPIA, levels = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "17"))) %>%
  filter(!is.na(ANOS_TERAPIA)) %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  ggplot(aes(ANOS_TERAPIA, N_COMPLICACION, fill = ANOS_TERAPIA)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(cols = vars(ESCOLARIDAD)) +
  scale_fill_npg()


# Test: ANOVA one-way by Variable
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     AÑOS DE TERAPIA

aov_by_variable <- function(variable) {

  map(
    data_main %>% distinct(.data[[variable]]) %>% pull() %>% levels(),
    .f = ~ (
      data_main %>%
        filter(.data[[variable]] == .x) %>%
        aov(N_COMPLICACION ~ as.character(ANOS_TERAPIA), data = .) %>%
        summary()
    )[[1]] %>%
      `row.names<-`(c('Años de terapia', 'Residuals'))
  )

}

aov_by_variable("DIALISIS"); aov_by_variable("SEXO"); aov_by_variable("ESCOLARIDAD")



# Tests: Anova one-way and DBCA
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     AÑOS DE TERAPIA
# Factores de bloque:    TIPO DE DIALISIS Y TIPO DE CUIDADOR
anova_t_terapia_vs_complicacion_bloque <- (
  data_main %>%
    aov(N_COMPLICACION ~ (as.character(ANOS_TERAPIA) + DIALISIS + as.character(CUIDADOR)), data = .) %>%
    summary()
)[[1]] %>%
  `row.names<-`(c('Años de terapia', 'Tipo de diálisis', 'Tipo de cuidador', 'Residuals'))
