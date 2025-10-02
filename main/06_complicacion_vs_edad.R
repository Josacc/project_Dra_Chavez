# Cantidad de complicaciones vs Edad


# Plot 6
data_main_2 %>%
  ggplot(aes(N_COMPLICACION, EDAD, fill = N_COMPLICACION)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  ggtitle('Complicaciones vs edad') +
  labs(x = '', y = 'Edad', fill = 'Complicación') +
  theme_light()

# Tests 1: Anova one-way
# Variable de respuesta: NÚMERO DE COMPLICACIONES
# Factor de estudio:     EDAD
(
  data_main %>%
    aov(N_COMPLICACION ~ as.character(EDAD), data = .) %>%
    summary()
)[[1]] %>%
  `row.names<-`(c('Edad', 'Residuals'))
