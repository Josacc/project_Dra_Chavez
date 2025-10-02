# Tipo de complicaión prévia vs Tipo de complicación actual



# First analysis ----------------------------------------------------------

# Plot: alluvial
# Diagrama aluvial de transición de las complicaciones asociadas.
plot_alluvial_tipocomplicacion <-
  data_main %>%
  count(COMPLICACION_PREVIA, COMPLICACION_ACTUAL, name = 'Conteo') %>%
  ggplot(aes(axis1 = COMPLICACION_PREVIA, axis2 = COMPLICACION_ACTUAL, y = Conteo)) +
  geom_alluvium(aes(fill = COMPLICACION_PREVIA)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = str_wrap(after_stat(stratum), width = 15)), size = 2) +
  scale_x_discrete(limits = c("Complicación previa", "Complicación actual"), expand = c(0.15, 0.05)) +
  labs(fill = 'Tipo de complicación') +
  theme_void() +
  theme(
    axis.text.x     = element_text(size = 10, vjust = 5),
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.text     = element_text(size = 6),
    legend.title    = element_text(size = 10)
  )



# Second analysis ---------------------------------------------------------

# Diális manual
# Plot 8

  data_main %>%
  filter(DIALISIS == 'MANUAL') %>%
  count(COMPLICACION_PREVIA, COMPLICACION_ACTUAL, name = 'Conteo') %>%
  ggplot(aes(axis1 = COMPLICACION_PREVIA, axis2 = COMPLICACION_ACTUAL, y = Conteo)) +
  geom_alluvium(aes(fill = COMPLICACION_PREVIA)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = str_wrap(after_stat(stratum), width = 15))) +
  scale_x_discrete(limits = c("Complicación previa", "Complicación actual"), expand = c(0.15, 0.05)) +
  labs(fill = 'Tipo de complicación') +
  theme_void() +
  theme(
    axis.text.x     = element_text(size = 12, vjust = 5),
    legend.position = "bottom",
    legend.box      = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1))



# Diálisis automatizada
# Plot 9
data_main %>%
  filter(DIALISIS == 'AUTOMATIZADA') %>%
  count(COMPLICACION_PREVIA, COMPLICACION_ACTUAL, name = 'Conteo') %>%
  ggplot(aes(axis1 = COMPLICACION_PREVIA, axis2 = COMPLICACION_ACTUAL, y = Conteo)) +
  geom_alluvium(aes(fill = COMPLICACION_PREVIA)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = str_wrap(after_stat(stratum), width = 15))) +
  scale_x_discrete(limits = c("Complicación previa", "Complicación actual"),
                   expand = c(0.15, 0.05)) +
  labs(fill = 'Complicación') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 12, vjust = 5)
  )
