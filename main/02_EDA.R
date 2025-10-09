# Exploratory data analysis


data_raw <- read_xlsx('data_source/base_datos.xlsx')


data_main <-
  data_raw %>%
  select(-c(NOMBRE, NSS, AGREGADO)) %>%
  rename(
    c(
      ID                  = `# DE PACIENTE`,
      SEXO                = `F/M`,
      ANOS_TERAPIA        = `AÑOS DE TERAPIA`,
      DIALISIS            = `CARACTERISTICAS DE LA DIALISIS`,
      COMPLICACION_PREVIA = `COMPLICACIONES PREVIAS`,
      COMPLICACION_ACTUAL = `COMPLICACION ACTUAL`,
      CUIDADOR            = `PERFIL DEL CUIDADOR`,
      N_COMPLICACION      = `NUMERO Y TIPO DE INGRESOS POR COMPLICACION`
    )
  ) %>%
  mutate(
    N_COMPLICACION = str_replace_all(
      N_COMPLICACION, c(
        'NINGUNA' = '0',
        'PRIMERA' = '1',
        'SEGUNDA' = '2',
        'TERCERA' = '3'
      )
    )
  ) %>%
  mutate(N_COMPLICACION = as.double(N_COMPLICACION)) %>%
  mutate(ESCOLARIDAD    = str_replace(ESCOLARIDAD, pattern = 'SECUDNARIA', replacement = 'SECUNDARIA')) %>%
  mutate(SEXO           = str_replace(SEXO, pattern = 'A', replacement = 'F')) %>%
  mutate(across(where(is_character), ~ fct(.x, levels = sort(unique(.x)))))



data_main_2 <-
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
  )



data_table_analysis <-
  data_raw %>%
  select(-c(NOMBRE, NSS, AGREGADO)) %>%
  rename(
    c(
      ID                  = `# DE PACIENTE`,
      SEXO                = `F/M`,
      ANOS_TERAPIA        = `AÑOS DE TERAPIA`,
      DIALISIS            = `CARACTERISTICAS DE LA DIALISIS`,
      COMPLICACION_PREVIA = `COMPLICACIONES PREVIAS`,
      COMPLICACION_ACTUAL = `COMPLICACION ACTUAL`,
      CUIDADOR            = `PERFIL DEL CUIDADOR`,
      N_COMPLICACION      = `NUMERO Y TIPO DE INGRESOS POR COMPLICACION`
    )
  ) %>%
  mutate(ESCOLARIDAD    = str_replace(ESCOLARIDAD, pattern = 'SECUDNARIA', replacement = 'SECUNDARIA')) %>%
  mutate(SEXO           = str_replace(SEXO, pattern = 'A', replacement = 'F')) %>%
  mutate(
    ESCOLARIDAD = str_replace_all(
      ESCOLARIDAD, c(
        '^(SECUNDARIA)'   = 'MEDIA',
        '^(PREPARATORIA)' = 'MEDIA',
        '^(LICENCIATURA)' = 'SUPERIOR'
      )
    )
  ) %>%
  mutate(CUIDADOR = as.character(CUIDADOR)) %>%
  mutate(across(where(is_character), ~ fct(.x, levels = sort(unique(.x))))) %>%
  mutate(ANOS_TERAPIA = fct(as.character(ANOS_TERAPIA), levels = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "17")))
