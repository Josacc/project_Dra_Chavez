# Table analysis



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
    )
  ) %>%
  filter(!(is.na(`Valor p`) | (Variable == "COMPLICACION_ACTUAL" & `Factor de estudio` == "N_COMPLICACION"))) %>%
  add_row(
    tibble(Variable = rep("EDAD", 2), `Factor de estudio` = c("COMPLICACION_ACTUAL", "N_COMPLICACION")) %>%
      mutate(
        `Valor p` = map_dbl(
          `Factor de estudio`,
          \(factor_est) pluck(list_anova_tests, factor_est, "Pr(>F)", 1, .default = NA_real_)
        )
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
  mutate(`Valor p` = round(`Valor p`, 3)) %>%
  mutate(significa = if_else(`Valor p`<0.05, true = " *", false = "")) %>%
  mutate(`Valor p` = if_else(`Valor p`< 0.001, true = "< 0.001", false = as.character(`Valor p`))) %>%
  mutate(`Valor p` = str_c(`Valor p`, significa)) %>%
  select(-significa) %>%
  datatable(
    rownames = FALSE,
    class    = "hover",
    options  = list(
      pageLength   = -1,
      ordering     = FALSE,
      dom          = "t",
      columnDefs   = list(
        list(targets = "_all", className = "dt-left" ),
        list(targets = 1, width = '350px')
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().node()).css({'font-size': '12px'});",
        "}"
      )
    )
  )
