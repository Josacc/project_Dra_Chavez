# Tabla de variables, caracteristicas y sus valores.


table_vars <-
  tribble(
    ~ Variable,               ~ Tipo,                ~ Valores,
    "SEXO"                   , "Categórica nominal" , c("HOMBRE", " MUJER"),
    "EDAD"                   , "Continua (años)"    , "valores entre 18 y 92 años",
    "OCUPACIÓN"              , "Categórica nominal" , c("PENSIONADO", " TRABAJADOR"),
    "ESCOLARIDAD"            , "Categórica nominal" , c("MEDIA", " SUPERIOR"),
    "AÑOS DE TERAPIA"        , "Categórica ordinal" , c("1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9", " 17"),
    "TIPO DE DÍALISIS"       , "Categórica nominal" , c("AUTOMATIZADA", " MANUAL"),
    "CUIDADOR"               , "Categórica nominal" , c("Pareja hombre escolaridad media (1)", " Pareja mujer escolaridad media (2)",
    " Pareja hombre escolaridad superior (3)", " Pareja mujer escolaridad superior (4)", " Familiar directo hombre escolaridad media (5)",
    " Familiar directo mujer escolaridad media (6)", " Familiar directo hombre escolaridad superior (7)", " Familiar directo mujer escolaridad media (8)", "\n Otro (9)"),
    "TIPO DE COMPLICACIÓN"   , "Categórica nominal" , "FUGA POR ORIFICIO DE SALIDA, HERNIA, MIGRACIÓN DE CATETER, OBSTRUCCIÓN DE CATETER, PERITONITIS, TUNELITIS",
    "NÚMERO DE COMPLICACIÓN" , "Categórica ordinal" , "NINGUNA, PRIMERA, SEGUNDA, TERCERA"

  ) %>%
  datatable(
    rownames = FALSE,
    class    = "compact stripe cell-border hover row-border",
    options  = list(
      ordering     = FALSE,
      dom          = "t",
      columnDefs   = list(
        list(targets = "_all", className = "dt-left" ),
        list(targets = 2, width = '350px')
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().node()).css({'font-size': '12px'});",
        "}"
      )
    )
  )
