

# Plot: mosaic
# Mosaico de frecuencia combinada: tiempo de terapia & número de complicaciones
plot_mosaic_timpoterapia_complicaciones <-
  data_table_analysis %>%
  filter(!is.na(COMPLICACION_ACTUAL)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(COMPLICACION_ACTUAL, CUIDADOR), fill = COMPLICACION_ACTUAL), show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = 'Tipo de cuidador', y = 'Tipo de complicación') +
  theme_minimal() +
  theme(
    plot.title = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 7),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 30)
  )
