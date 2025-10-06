# Table analysis


data_table_analysis <- data_raw %>%
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


set.seed(1)
list_chi2_tests <-
  map(
    c("COMPLICACION_ACTUAL", "N_COMPLICACION"),
    .f = \(complicacion)(
      data_table_analysis %>%
        keep(is.factor) %>%
        select(-c(COMPLICACION_PREVIA, all_of(complicacion))) %>%
        map2(
          .y = data_table_analysis %>% select(all_of(complicacion)),
          ~ chisq.test(table(.x, .y))
        )
    )
  ) %>%
  `names<-`(c("COMPLICACION_ACTUAL", "N_COMPLICACION"))


list_anova_tests <-
  map(
    c("COMPLICACION_ACTUAL", "N_COMPLICACION"),
    .f = \(complicacion) data_table_analysis %>%
      aov(EDAD ~ pluck(., complicacion), data = .) %>%
      summary() %>%
      pluck(1)
  ) %>%
  `names<-`(c("COMPLICACION_ACTUAL", "N_COMPLICACION"))


table_all_analysis <-
  data_table_analysis %>%
  keep(is.factor) %>%
  select(-COMPLICACION_PREVIA) %>%
  names() %>%
  sort() %>%
  rep(2) %>%
  tibble() %>%
  `names<-`("Variable") %>%
  mutate(`Factor de estudio` = c(rep("COMPLICACION_ACTUAL", 8), rep("N_COMPLICACION", 8))) %>%
  mutate(
    `Valor p` = map2_dbl(
      Variable,
      `Factor de estudio`,
      \(variable, factor_est) pluck(list_chi2_tests, factor_est, variable, "p.value", .default = NA_real_)
    ) %>%
      round(digits = 4)
  ) %>%
  filter(!(is.na(`Valor p`) | (Variable == "COMPLICACION_ACTUAL" & `Factor de estudio` == "N_COMPLICACION"))) %>%
  add_row(
    tibble(Variable = rep("EDAD", 2), `Factor de estudio` = c("COMPLICACION_ACTUAL", "N_COMPLICACION")) %>%
      mutate(
        `Valor p` = map_dbl(
          `Factor de estudio`,
          \(factor_est) pluck(list_anova_tests, factor_est, "Pr(>F)", 1, .default = NA_real_)
        ) %>%
          round(4)
      )
  ) %>%
  mutate(
    `Factor de estudio` = str_replace_all(
      `Factor de estudio`, c(
        '^(COMPLICACION_ACTUAL)' = 'TIPO DE COMPLICACIÓN',
        '^(N_COMPLICACION)'      = 'NÚMERO DE COMPLICACIÓN'
      )
    )
  ) %>%
  mutate(
    Variable = str_replace_all(
      Variable, c(
        "^ANOS_TERAPIA"   = "TIEMPO DE TERAPIA",
        "^N_COMPLICACION" = 'NÚMERO DE COMPLICACIÓN'
      )
    )
  ) %>%
  datatable(
    rownames = FALSE,
    options  = list(
      pageLength   = -1,
      ordering     = FALSE,
      dom          = "t",
      columnDefs   = list(list(targets = "_all", className = "dt-left" )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().node()).css({'font-size': '12px'});",
        "}"
      )
    )
  )
