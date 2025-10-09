# Tipo de complicación & Número de complicación


# Plot: mosaic
plot_mosaic_tipocomplicacion_numcomplicacion <-
  data_table_analysis %>%
  filter(!is.na(COMPLICACION_ACTUAL)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(N_COMPLICACION, COMPLICACION_ACTUAL), fill = N_COMPLICACION), show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = 'Tipo de complicación',  y = 'Número de complicación') +
  theme_minimal() +
  scale_x_productlist(
    labels = c(
      "FUGA POR ORIFICIO DE SALIDA" = "FOS",
      "HERNIA",
      "MIGRACIÓN DE CATETER"        = "MC",
      "OBSTRUCCIÓN DE CATETER"      = "OBSTRUCCIÓN \nDE CATETER",
      "PERITONITIS",
      "TUNELITIS"
    )
  ) +
  theme(
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 7),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 30)
  )



# Plot: treemap
plot_treemap_numcomplicaciones_tipocomplicacion <-
  data_table_analysis %>%
  count(COMPLICACION_ACTUAL, N_COMPLICACION, name = 'Conteo', sort = TRUE) %>%
  transmute(
    Grupo = fct_inorder(
      str_c(str_c("Complicación: ", COMPLICACION_ACTUAL), str_c("N. complicación: ", N_COMPLICACION), sep = '\n\n')
    ),
    Conteo
  ) %>%
  mutate(Porcentaje = (Conteo / sum(Conteo)) * 100) %>%
  filter(Porcentaje > 5) %>%
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
