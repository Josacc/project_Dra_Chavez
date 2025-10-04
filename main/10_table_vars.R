# Tabla de variables, caracteristicas y sus valores.


table_vars <-
  tribble(
    ~ Variable,              ~ Tipo,                ~ Valores,
    "SEXO"                  , "Categórica nominal" , "HOMBRE, MUJER",
    "EDAD"                  , "Continua (años)"    , "valores entre 18 y 92 años",
    "OCUPACIÓN"             , "Categórica nominal" , "PENSIONADO, TRABAJADOR",
    "ESCOLARIDAD"           , "Categórica nominal" , "MEDIA, SUPERIOR",
    "AÑOS DE TERAPIA"       , "Categórica ordinal" , "1, 2, 3, 4, 5, 6, 7, 8, 9, 17",
    "TIPO DE DÍALISIS"      , "Categórica nominal" , "AUTOMATIZADA, MANUAL",
    "TIPO DE COMPLICACIÓN"  , "Categórica nominal" , "FUGA POR ORIFICIO DE SALIDA, HERNIA, MIGRACIÓN DE CATETER, OBSTRUCCIÓN DE CATETER, PERITONITIS, TUNELITIS",
    "NÚMERO DE COMPLICAIÓN" , "Categórica ordinal" , "NINGUNA, PRIMERA, SEGUNDA, TERCERA"

  ) %>%
  datatable(
    rownames = FALSE,
    class    = "compact stripe cell-border hover row-border",
    options  = list(
      ordering     = FALSE,
      dom          = "t",
      columnDefs   = list(list(targets = "_all", className = "dt-left" )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().node()).css({'font-size': '14px'});",
        "}"
      )
    )
  )
