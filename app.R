# ============================================================
# DASHBOARD TERRITORIAL INDUSTRIAL — ENTRE RÍOS
# app.R — Aplicación Shiny completa
# ============================================================  

# ===== LIBRERÍAS =====
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(plotly)
library(scales)

# ===== CONSTANTES =====
MESES_ES <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

INFRA_LABELS <- c(
  infra_alumbrado    = "Alumbrado público",
  infra_cerco        = "Cerco perimetral",
  infra_energia      = "Energía eléctrica",
  infra_gas          = "Gas natural",
  infra_accesos      = "Accesos pavimentados",
  infra_desagues     = "Desagüe pluvial",
  infra_agua         = "Agua potable",
  infra_conectividad = "Conectividad",
  infra_efluentes    = "Trat. de efluentes",
  infra_seguridad    = "Seguridad"
)

# Paleta para cociente de localización (4 categorías interpretables)
LQ_BINS   <- c(0, 0.75, 1.25, 2.0, Inf)
LQ_COLORS <- c("#ea7b6f", "#f1f1f1", "#bfe8df", "#35d4bc")

# ===== CARGA DE DATOS (una sola vez al inicio) =====

empleo <- readr::read_tsv(
  "data/empleo_reducido.txt",
  col_types = cols(
    anio                     = col_integer(),
    mes                      = col_integer(),
    nombre_departamento_indec = col_character(),
    sector_nombre            = col_character(),
    puestos_sector_privado   = col_double()
  )
)

parques <- readr::read_tsv("data/parques_para_shiny.txt", show_col_types = FALSE) |>
  dplyr::mutate(
    n_servicios = rowSums(
      across(any_of(names(INFRA_LABELS)), function(x) as.integer(!is.na(x) & as.numeric(x) == 1)),
      na.rm = TRUE
    )
  )

puertos <- readr::read_csv("data/puertos_er.csv",
  col_types = cols(puerto = col_character(), lat = col_double(),
                   lon = col_double(), ubicacion = col_character()))

PUERTOS_DESC <- c(
  "Puerto Concepción del Uruguay" = "Principal salida de cítricos y granos del norte entrerriano sobre el río Uruguay",
  "Puerto Gualeguaychú"           = "600 m de muelle sobre el río homónimo; opera granos y carga general con acceso al Uruguay",
  "Puerto Colón"                  = "Puerto natural sobre el Uruguay; conecta la región citrícola entrerriana con la Hidrovía",
  "Puerto Diamante"               = "Sobre el Paraná junto al túnel subfluvial; embarque de granos y subproductos agroindustriales",
  "Puerto Victoria"               = "Puerto del Paraná en los bajos del río; embarque de productos forestales y arroceros",
  "Puerto Ibicuy"                 = "En el delta entrerriano; exporta más de 150 mil toneladas de madera en rollizos por año",
  "Puerto Concordia"              = "Norte provincial sobre el Uruguay; opera granos con infraestructura de aguas profundas",
  "Puerto La Paz"                 = "Puerto costanero del Paraná; tráfico de carga general y productos agroindustriales",
  "Puerto Rosario"                = "Mayor hub exportador de granos de Argentina; concentra el 66 % de las exportaciones cerealeras nacionales",
  "Puerto Fray Bentos"            = "Terminal uruguaya especializada en celulosa (1,3 M t/año); principal destino de la madera entrerriana al exterior"
)

# Polígonos de departamentos de Entre Ríos
# pxdptodatosok: shapefile INDEC de departamentos, columna codpcia identifica la provincia
deptos_er <- sf::st_read("data/geo/pxdptodatosok.shp", quiet = TRUE) |>
  dplyr::filter(codpcia == "30") |>
  # Normalizar nombre para join: la columna en el shapefile se llama "departamen"
  dplyr::rename(nombre_depto = departamen)

# Polígono provincial (para clipping de rutas)
prov_er <- sf::st_read("data/geo/provinciaPolygon.shp", quiet = TRUE) |>
  dplyr::filter(in1 == "30")

# Rutas recortadas a Entre Ríos
rutas_nac  <- sf::st_read("data/geo/vial_nacionalLine.shp",  quiet = TRUE) |>
  sf::st_filter(prov_er)
rutas_prov <- sf::st_read("data/geo/vial_provincialLine.shp", quiet = TRUE) |>
  sf::st_filter(prov_er)

# Valores para selectores
ANIOS    <- sort(unique(empleo$anio), decreasing = TRUE)
SECTORES <- sort(unique(empleo$sector_nombre))
DPTOS_PARQUES <- c("Todos" = "", sort(unique(parques$departamento)))

# Función para extraer valores únicos de columnas de listas separadas por coma
lista_unica <- function(col) {
  col |> na.omit() |> strsplit(",") |> unlist() |> trimws() |>
    (\(x) x[nchar(x) > 0])() |> stringr::str_to_title() |> unique() |> sort()
}
RUBROS_PARQUES   <- lista_unica(parques$rubros_instalados)
EMPRESAS_PARQUES <- lista_unica(parques$empresas_instaladas)

# Meses con datos reales por año (evita mostrar meses sin relevamiento)
MESES_POR_ANIO <- empleo |>
  distinct(anio, mes) |>
  arrange(anio, mes)

meses_de_anio <- function(a) {
  MESES_POR_ANIO$mes[MESES_POR_ANIO$anio == as.integer(a)]
}

# Datos preprocesados para heatmap (estático)
rubros_long <- parques |>
  select(nombre_parque, departamento, rubros_instalados) |>
  filter(!is.na(rubros_instalados), rubros_instalados != "") |>
  mutate(rubro_list = strsplit(rubros_instalados, ",")) |>
  tidyr::unnest(rubro_list) |>
  mutate(rubro = trimws(rubro_list)) |>
  filter(nchar(rubro) > 0) |>
  count(departamento, rubro, name = "frecuencia")

# ===== FUNCIONES AUXILIARES =====

mes_nombre <- function(m) MESES_ES[as.integer(m)]

interpret_lq <- function(lq) {
  dplyr::case_when(
    is.na(lq)  ~ "Sin datos",
    lq < 0.75  ~ "Especialización baja",
    lq < 1.25  ~ "Cercana al promedio provincial",
    lq < 2.0   ~ "Especialización alta",
    TRUE       ~ "Especialización muy alta"
  )
}

# Cálculo del cociente de localización sectorial por departamento
calc_lq <- function(df, sector_sel) {
  total_depto <- df |>
    group_by(nombre_departamento_indec) |>
    summarise(total = sum(puestos_sector_privado, na.rm = TRUE), .groups = "drop")

  sector_depto <- df |>
    filter(sector_nombre == sector_sel) |>
    group_by(nombre_departamento_indec) |>
    summarise(sector = sum(puestos_sector_privado, na.rm = TRUE), .groups = "drop")

  sector_prov <- sum(sector_depto$sector, na.rm = TRUE)
  total_prov  <- sum(df$puestos_sector_privado, na.rm = TRUE)

  total_depto |>
    left_join(sector_depto, by = "nombre_departamento_indec") |>
    replace_na(list(sector = 0)) |>
    mutate(
      lq = ifelse(total > 0 & total_prov > 0 & sector_prov > 0,
                  (sector / total) / (sector_prov / total_prov),
                  NA_real_),
      interpretacion = interpret_lq(lq),
      valor = lq
    )
}

# Trunca una cadena separada por comas a n elementos + "…"
trunc_lista <- function(txt, n = 5) {
  if (is.na(txt) || nchar(trimws(txt)) == 0) return("—")
  items <- trimws(strsplit(txt, ",")[[1]])
  items <- items[nchar(items) > 0]
  if (length(items) <= n) paste(items, collapse = ", ")
  else paste(c(items[1:n], "…"), collapse = ", ")
}

# HTML del estado de infraestructura (2 columnas)
infra_html <- function(park_row) {
  items <- mapply(function(col, label) {
    val <- park_row[[col]]
    on  <- !is.na(val) && as.numeric(val) == 1
    dot <- if (on) "service-dot" else "service-dot off"
    sprintf('<div style="display:flex;align-items:center;gap:3px;"><span class="%s"></span><span>%s</span></div>', dot, label)
  }, names(INFRA_LABELS), INFRA_LABELS, SIMPLIFY = TRUE)
  paste0(
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:0.1rem 0.4rem;font-size:11.5px;">',
    paste(items, collapse = ""),
    '</div>'
  )
}

# Popup de hover para parques (info básica)
label_parque <- function(nombre, dept) {
  sprintf("<b>%s</b><br><span style='color:#64748b;font-size:12px;'>%s</span>",
          nombre, dept)
}

# Popup de click para parques (información completa)
popup_parque <- function(row) {
  promo_html <- ""
  if (!is.na(row$tiene_promocion) && as.logical(row$tiene_promocion)) {
    promo_html <- '<div style="margin:0.3rem 0 0.5rem;"><span style="background:#fef9c3;color:#854d0e;border-radius:4px;padding:2px 8px;font-size:12px;font-weight:600;">&#9733; Con promociones impositivas</span></div>'
  }

  link_html <- ""
  if (!is.na(row$enlace) && nchar(trimws(row$enlace)) > 0) {
    link_html <- sprintf('<div style="margin-top:0.6rem;"><a href="%s" target="_blank" class="popup-link">Ver ficha completa &rarr;</a></div>', row$enlace)
  }

  stat_items <- list()
  if (!is.na(row$cant_empresas))
    stat_items <- c(stat_items, sprintf('<div><div class="popup-label">Empresas</div><div style="font-size:13px;font-weight:600;color:#1e293b;">%s</div></div>', row$cant_empresas))
  if (!is.na(row$cant_personas_empleadas))
    stat_items <- c(stat_items, sprintf('<div><div class="popup-label">Empleados</div><div style="font-size:13px;font-weight:600;color:#1e293b;">%s</div></div>', row$cant_personas_empleadas))
  if (!is.na(row$sup_total_ha))
    stat_items <- c(stat_items, sprintf('<div><div class="popup-label">Superficie</div><div style="font-size:13px;font-weight:600;color:#1e293b;">%s ha</div></div>', row$sup_total_ha))
  if (!is.na(row$pct_sup_disponible))
    stat_items <- c(stat_items, sprintf('<div><div class="popup-label">Área disp.</div><div style="font-size:13px;font-weight:600;color:#1e293b;">%.1f%%</div></div>', as.numeric(row$pct_sup_disponible)))

  stats_grid <- if (length(stat_items) > 0)
    paste0('<div style="display:grid;grid-template-columns:1fr 1fr;gap:0.4rem 0.6rem;margin:0.4rem 0 0.5rem;">',
           paste(stat_items, collapse = ""), '</div>')
  else ""

  paste0(
    '<div style="min-width:260px;max-width:320px;font-family:Inter,sans-serif;font-size:13px;line-height:1.5;">',
    sprintf('<div class="popup-title">%s</div>', row$nombre_parque),
    sprintf('<div style="color:#64748b;font-size:12px;margin-bottom:0.3rem;">%s</div>', row$departamento),
    promo_html, stats_grid,
    '<div class="popup-section">',
    '<div style="font-size:10px;font-weight:600;color:#94a3b8;text-transform:uppercase;letter-spacing:.05em;margin-bottom:.2rem;">Sectores instalados</div>',
    sprintf('<div style="font-size:12px;color:#374151;">%s</div>', trunc_lista(row$rubros_instalados, 5)),
    '</div>',
    '<div class="popup-section">',
    '<div style="font-size:10px;font-weight:600;color:#94a3b8;text-transform:uppercase;letter-spacing:.05em;margin-bottom:.2rem;">Empresas instaladas</div>',
    sprintf('<div style="font-size:12px;color:#374151;">%s</div>', trunc_lista(row$empresas_instaladas, 5)),
    '</div>',
    '<div class="popup-section">',
    '<div style="font-size:10px;font-weight:600;color:#94a3b8;text-transform:uppercase;letter-spacing:.05em;margin-bottom:.2rem;">Infraestructura</div>',
    infra_html(row),
    '</div>',
    link_html,
    '</div>'
  )
}

# Tooltip HTML para polígonos de empleo (hover)
tooltip_empleo <- function(depto, valor, modo, sector = NULL, anio, mes) {
  periodo <- paste0("<small style='color:#94a3b8;font-size:11px;'>", mes_nombre(mes), " ", anio, "</small>")
  if (modo == "cantidad") {
    val_fmt <- if (is.na(valor) || valor == 0) "Sin datos"
               else format(round(valor), big.mark = ".", scientific = FALSE)
    paste0("<b>", depto, "</b><br>",
           "<span style='color:#475569;'>Puestos registrados: <b>", val_fmt, "</b></span><br>",
           periodo)
  } else {
    if (is.na(valor)) {
      paste0("<b>", depto, "</b><br>",
             "<span style='color:#94a3b8;'>Sin datos para este sector</span><br>",
             periodo)
    } else {
      pct_diff <- round((valor - 1) * 100)
      color    <- if (pct_diff > 0) "#166534" else if (pct_diff < 0) "#374151" else "#475569"
      frase    <- if (pct_diff > 0)
        paste0("Concentra un <b>", pct_diff, "% más</b> que el promedio provincial")
      else if (pct_diff < 0)
        paste0("Concentra un <b>", abs(pct_diff), "% menos</b> que el promedio provincial")
      else
        "Concentra exactamente el <b>promedio provincial</b>"
      paste0("<b>", depto, "</b><br>",
             "<span style='color:#64748b;font-size:11px;'>LQ: <b style='color:", color, ";'>",
             sprintf("%.2f", valor), "</b></span><br>",
             "<span style='color:", color, ";font-size:12px;'>", frase, "</span><br>",
             periodo)
    }
  }
}

# ===== IDENTIDAD VISUAL =====
color_fondo     <- "#181818"
color_detalle   <- "#505050"
color_texto     <- "white"
color_principal <- "#2AA198"
colores <- list(fondo = color_fondo, detalle = color_detalle,
                texto = color_texto, principal = color_principal)
tema <- bs_theme(bg = color_fondo, fg = color_texto, primary = color_principal)
options(spinner.type = 8, spinner.color = color_principal)

# ===== COORDENADAS ENTRE RÍOS =====
ER_BOUNDS <- list(lng1 = -60.85, lat1 = -34.1, lng2 = -57.4, lat2 = -29.9)

# ===== INTERFAZ DE USUARIO =====
ui <- fluidPage(
  theme = tema,
  # CSS y fuentes
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(HTML("
      $(document).on('shiny:idle', function() {
        $('#loading-screen').fadeOut(400);
      });
      $(document).on('shiny:connected', function() {
        function syncBtns(val) {
          $('.seg-btn[data-target]').removeClass('seg-active');
          $('.seg-btn[data-target=\"' + val + '\"]').addClass('seg-active');
        }
        function syncPromoBtns(checked) {
          $('.seg-promo-btn[data-promo=\"true\"]').toggleClass('seg-active', checked);
          $('.seg-promo-btn[data-promo=\"false\"]').toggleClass('seg-active', !checked);
        }
        // Toggle empleo: click → radio oculto → Shiny
        $(document).on('click', '.seg-btn[data-target]', function() {
          var val = $(this).data('target');
          $('#modo_empleo input[value=\"' + val + '\"]').prop('checked', true).trigger('change');
          syncBtns(val);
        });
        // Toggle promociones: click → radio oculto → Shiny (igual que empleo)
        $(document).on('click', '.seg-promo-btn', function() {
          var val = $(this).attr('data-promo');
          $('#solo_promo input[value=\"' + val + '\"]').prop('checked', true).trigger('change');
          syncPromoBtns(val === 'true');
        });
        // updateRadioButtons desde el servidor → sincroniza botones visuales
        $(document).on('change', '#modo_empleo input[type=radio]', function() {
          syncBtns($(this).val());
        });
        // updateRadioButtons desde el servidor → sincroniza botones de promo
        $(document).on('change', '#solo_promo input[type=radio]', function() {
          syncPromoBtns($(this).val() === 'true');
        });
        // Estado inicial
        var init = $('#modo_empleo input:checked').val() || 'cantidad';
        syncBtns(init);
      });
    "))
  ),

  # Estilos bslib: botones y selectores
  tags$style(paste0("
    .action-button {
      font-size: 75%; padding: 3px; padding-left: 8px; padding-right: 8px;
      color: ", colores$detalle, ";
      border: 1px solid ", colores$detalle, ";
    }
    .action-button:hover, .action-button:active, .action-button:focus {
      color: ", colores$fondo, ";
      border: 1px solid ", colores$principal, ";
      background-color: ", colores$principal, ";
    }")),
  tags$style(paste0("
    .selectize-input {
      background-color: #252525 !important;
      max-height: 58px; overflow-y: hidden;
    }")),

  # Pantalla de carga
  tags$div(id = "loading-screen",
    tags$div(class = "spinner"),
    tags$div(class = "loading-text", "Cargando monitor de producción…")
  ),

  # Encabezado
  tags$div(class = "app-header",
    tags$div(class = "app-wrapper",
      tags$div(class = "header-content",
        tags$div(class = "header-text",
          tags$h1("Monitor de actores productivos"),
          tags$p(class = "lead",
            "Versión de prueba — Entre Ríos. Distribución del empleo privado registrado, ",
            "localización de parques industriales y especialización sectorial por departamento."
          )
        ),
        tags$div(class = "header-logo",
          tags$img(src = "ISE logo.png", alt = "Instituto Sociedad y Economía")
        )
      )
    )
  ),

  # Contenido principal
  tags$div(class = "app-wrapper",

    # ── SECCIÓN 1: EMPLEO ─────────────────────────────────────
    tags$div(class = "section-container", id = "sec-empleo",
      tags$h2(class = "section-title", "Distribución de los puestos de trabajo"),
      tags$p(class = "section-desc",
        "Visualizá el empleo privado registrado por departamento, seleccionando uno o varios sectores, ",
        "o calculando la especialización relativa de un sector respecto al total provincial."
      ),

      # Toggle de modo
      tags$div(class = "mode-toggle",
        # Radio Shiny oculto: mantiene input$modo_empleo y updateRadioButtons funcionales
        tags$div(style = "display:none;",
          radioButtons("modo_empleo", label = NULL,
            choices  = c("Cantidad de puestos de trabajo" = "cantidad",
                         "Especialización relativa"       = "especializacion"),
            selected = "cantidad",
            inline   = TRUE
          )
        ),
        # Botones visuales custom
        tags$div(class = "segmented-control",
          tags$button(type = "button", class = "seg-btn seg-active",
            `data-target` = "cantidad", "Cantidad de puestos de trabajo"),
          tags$button(type = "button", class = "seg-btn",
            `data-target` = "especializacion", "Especialización relativa")
        )
      ),
      tags$p(class = "filter-help",
        tags$b("Cantidad de puestos:"), " total de empleos privados registrados por departamento para el período seleccionado. ",
        tags$b("Especialización relativa:"), " cociente de localización (LQ) que indica si un departamento concentra más o menos ese sector respecto al promedio provincial."
      ),

      fluidRow(
        # Panel de filtros
        column(4,
          tags$div(class = "filter-panel",
            uiOutput("filtros_empleo_ui"),
            uiOutput("cards_empleo_ui"),
            tags$hr(),
            actionButton("reset_empleo", "Limpiar filtros", class = "btn-reset")
          )
        ),
        # Mapa
        column(8,
          tags$div(class = "map-container",
            leafletOutput("mapa_empleo", height = "520px")
          )
        )
      )
    ),

    # ── SECCIÓN 2: PARQUES INDUSTRIALES ───────────────────────
    tags$div(class = "section-container", id = "sec-parques",
      tags$h2(class = "section-title", "Parques Industriales"),
      tags$p(class = "section-desc",
        "Localización, infraestructura disponible y características de los 38 parques industriales de Entre Ríos. ",
        "El color de cada parque indica cuánto se aleja del promedio provincial de servicios. ",
        "Los parques con corona dorada tienen promociones impositivas."
      ),

      fluidRow(
        column(4,
          tags$div(class = "filter-panel",
            # Filtro por departamento
            selectInput("filtro_dpto_parque", "Departamento",
              choices  = DPTOS_PARQUES,
              selected = ""
            ),
            tags$p(class = "filter-help", "Filtrá para ver únicamente los parques del departamento seleccionado. Dejá en 'Todos' para ver el mapa completo."),

            # Toggle solo con promociones
            tags$div(style = "display:none;",
              radioButtons("solo_promo", label = NULL,
                choices  = c("Todos" = "false", "Solo con promoción" = "true"),
                selected = "false",
                inline   = TRUE
              )
            ),
            tags$label(class = "control-label", "Promoción impositiva"),
            tags$div(class = "segmented-control",
              tags$button(type = "button", class = "seg-btn seg-promo-btn seg-active",
                `data-promo` = "false", "Todos"),
              tags$button(type = "button", class = "seg-btn seg-promo-btn",
                `data-promo` = "true", "Solo con promoción")
            ),
            tags$p(class = "filter-help", "Activá para mostrar únicamente los parques que cuentan con beneficios de promoción impositiva municipal o provincial. En el mapa se identifican con una corona dorada."),

            # Filtro por servicio de infraestructura (multi-select)
            selectizeInput("servicio_infra", "Servicios de infraestructura",
              choices  = setNames(names(INFRA_LABELS), INFRA_LABELS),
              selected = "infra_alumbrado",
              multiple = TRUE,
              options  = list(placeholder = "Todos los servicios")
            ),
            tags$p(class = "filter-help", "Mostrá solo los parques que cuentan con todos los servicios seleccionados. Sin selección se muestran todos los parques."),

            # Filtro por rubros instalados
            selectizeInput("filtro_rubros", "Rubros instalados",
              choices  = RUBROS_PARQUES,
              selected = NULL,
              multiple = TRUE,
              options  = list(placeholder = "Todos los rubros")
            ),
            tags$p(class = "filter-help", "Mostrá solo los parques que tienen al menos uno de los rubros seleccionados. Sin selección se muestran todos."),

            # Filtro por empresas instaladas
            selectizeInput("filtro_empresas", "Empresas instaladas",
              choices  = EMPRESAS_PARQUES,
              selected = NULL,
              multiple = TRUE,
              options  = list(placeholder = "Todas las empresas")
            ),
            tags$p(class = "filter-help", "Buscá una empresa para ver en qué parques opera. Sin selección se muestran todos."),

            # Leyenda
            uiOutput("leyenda_parques_ui"),

            tags$hr(),
            actionButton("reset_parques", "Limpiar filtros", class = "btn-reset")
          )
        ),
        column(8,
          tags$div(class = "map-container",
            leafletOutput("mapa_parques", height = "540px")
          )
        )
      )
    ),

    # ── SECCIÓN 3: MAPA DE CALOR ───────────────────────────────
    tags$div(class = "section-container", id = "sec-heatmap",
      tags$h2(class = "section-title", "Sectores industriales por departamento"),
      tags$p(class = "section-desc",
        "Frecuencia de aparición de cada sector industrial en los parques de cada departamento. ",
        "El color más intenso indica mayor presencia de ese sector."
      ),
      tags$p(class = "heatmap-note",
        "Datos basados en los rubros declarados por cada parque industrial."
      ),
      plotlyOutput("heatmap_sectores", height = "500px")
    ),

    # ── PIE DE PÁGINA ─────────────────────────────────────────
    tags$div(class = "app-footer",
      "Fuente: Ministerio de Producción de Entre Ríos · INDEC · datos.produccion.gob.ar · IGN"
    )
  )
)

# ===== SERVIDOR =====
server <- function(input, output, session) {

  # ── UI REACTIVA: FILTROS DE EMPLEO ──────────────────────────
  output$filtros_empleo_ui <- renderUI({
    if (input$modo_empleo == "cantidad") {
      meses_ini_emp <- meses_de_anio(max(ANIOS))
      tagList(
        selectInput("anio_emp", "Año",
          choices = setNames(ANIOS, ANIOS), selected = max(ANIOS)),
        tags$p(class = "filter-help", "Año del relevamiento de empleo privado registrado."),
        selectInput("mes_emp", "Mes",
          choices  = setNames(meses_ini_emp, MESES_ES[meses_ini_emp]),
          selected = meses_ini_emp[1]),
        tags$p(class = "filter-help", "Mes del período a analizar. Solo se muestran los meses con datos disponibles para el año seleccionado."),
        selectizeInput("sectores_emp", "Sector(es)",
          choices  = SECTORES,
          multiple = TRUE,
          options  = list(placeholder = "Todos los sectores combinados")),
        tags$p(class = "filter-help",
          "Filtrá por una o varias industrias. Sin selección, se muestran todos los sectores sumados. Con varios sectores seleccionados, se acumulan sus puestos de trabajo.")
      )
    } else {
      meses_ini_esp <- meses_de_anio(max(ANIOS))
      tagList(
        selectInput("anio_esp", "Año",
          choices = setNames(ANIOS, ANIOS), selected = max(ANIOS)),
        tags$p(class = "filter-help", "Año del relevamiento de empleo privado registrado."),
        selectInput("mes_esp", "Mes",
          choices  = setNames(meses_ini_esp, MESES_ES[meses_ini_esp]),
          selected = meses_ini_esp[1]),
        tags$p(class = "filter-help", "Mes del período a analizar. Solo se muestran los meses con datos disponibles para el año seleccionado."),
        selectInput("sector_esp", "Sector",
          choices  = SECTORES,
          selected = SECTORES[1]),
        tags$p(class = "filter-help",
          "Sector a analizar. El mapa muestra si cada departamento está más o menos especializado en este sector respecto al promedio provincial: LQ > 1 indica mayor concentración relativa, LQ < 1 indica menor concentración.")
      )
    }
  })

  # ── DATOS REACTIVOS: EMPLEO PARA MAPA ───────────────────────
  empleo_mapa_data <- reactive({
    if (input$modo_empleo == "cantidad") {
      req(input$anio_emp, input$mes_emp)
      df <- empleo |>
        filter(anio == input$anio_emp, mes == input$mes_emp)

      if (!is.null(input$sectores_emp) && length(input$sectores_emp) > 0) {
        df <- df |> filter(sector_nombre %in% input$sectores_emp)
      }

      df |>
        group_by(nombre_departamento_indec) |>
        summarise(valor = sum(puestos_sector_privado, na.rm = TRUE), .groups = "drop") |>
        mutate(anio = input$anio_emp, mes = input$mes_emp, modo = "cantidad",
               sector = paste(input$sectores_emp, collapse = ", "))

    } else {
      req(input$anio_esp, input$mes_esp, input$sector_esp)
      df <- empleo |>
        filter(anio == input$anio_esp, mes == input$mes_esp)

      lq_df <- calc_lq(df, input$sector_esp)

      lq_df |>
        rename(nombre_departamento_indec = nombre_departamento_indec) |>
        mutate(anio = input$anio_esp, mes = input$mes_esp, modo = "especializacion",
               sector = input$sector_esp)
    }
  })

  # ── TARJETAS DE INDICADORES ──────────────────────────────────
  output$cards_empleo_ui <- renderUI({
    datos <- empleo_mapa_data()
    if (is.null(datos) || nrow(datos) == 0) return(NULL)

    if (input$modo_empleo == "cantidad") {
      total <- sum(datos$valor, na.rm = TRUE)
      top_depto <- datos |> filter(!is.na(valor)) |> slice_max(valor, n = 1)

      # Sector predominante: top sector por puestos en el período seleccionado
      df_sec <- empleo |>
        filter(anio == input$anio_emp, mes == input$mes_emp)
      if (!is.null(input$sectores_emp) && length(input$sectores_emp) > 0)
        df_sec <- df_sec |> filter(sector_nombre %in% input$sectores_emp)

      sec_ranking <- df_sec |>
        group_by(sector_nombre) |>
        summarise(puestos = sum(puestos_sector_privado, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(puestos))

      top_sector <- if (nrow(sec_ranking) > 0) sec_ranking$sector_nombre[1] else "—"
      pct_sector <- if (nrow(sec_ranking) > 0 && total > 0)
        sprintf("%.1f%% del total", sec_ranking$puestos[1] / total * 100)
      else "—"

      tagList(
        tags$br(),
        fluidRow(
          column(6,
            tags$div(class = "stat-card",
              tags$div(class = "stat-label", "Total provincial"),
              tags$div(class = "stat-value",
                format(total, big.mark = ".", decimal.mark = ",", scientific = FALSE)),
              tags$div(class = "stat-note", "puestos de trabajo")
            )
          ),
          column(6,
            tags$div(class = "stat-card",
              tags$div(class = "stat-label", "Sector predominante"),
              tags$div(class = "stat-value",
                style = "font-size:0.9rem;line-height:1.35;", top_sector),
              tags$div(class = "stat-note", pct_sector)
            )
          )
        ),
        if (nrow(top_depto) > 0)
          tags$div(class = "stat-card", style = "margin-top:.6rem;",
            tags$div(class = "stat-label", "Mayor concentración"),
            tags$div(class = "stat-value", style = "font-size:1.1rem;",
                     top_depto$nombre_departamento_indec[1]),
            tags$div(class = "stat-note",
              format(top_depto$valor[1], big.mark = ".", decimal.mark = ",", scientific = FALSE),
              " puestos")
          )
      )
    } else {
      datos_no_na <- datos |> filter(!is.na(valor))
      muy_altos <- sum(datos_no_na$valor > 2, na.rm = TRUE)
      tagList(
        tags$br(),
        tags$div(class = "stat-card",
          tags$div(class = "stat-label", "Deptamentos con esp. muy alta"),
          tags$div(class = "stat-value", muy_altos),
          tags$div(class = "stat-note", "Cociente de localización > 2")
        ),
        tags$div(class = "stat-card", style = "margin-top:.6rem;",
          tags$div(class = "stat-label", "Interpretación del indicador"),
          tags$div(style = "font-size:12px;color:#475569;margin-top:.3rem;line-height:1.7;",
            tags$div("< 0,75 — ", tags$b("Especialización baja")),
            tags$div("0,75 – 1,25 — ", tags$b("Cercana al promedio")),
            tags$div("1,25 – 2 — ", tags$b("Especialización alta")),
            tags$div("> 2 — ", tags$b("Especialización muy alta"))
          )
        )
      )
    }
  })

  # ── MAPA EMPLEO: RENDER INICIAL ──────────────────────────────
  output$mapa_empleo <- renderLeaflet({
    leaflet() |>
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
        attribution = "© OpenStreetMap | © CartoDB"
      ) |>
      fitBounds(
        lng1 = ER_BOUNDS$lng1, lat1 = ER_BOUNDS$lat1,
        lng2 = ER_BOUNDS$lng2, lat2 = ER_BOUNDS$lat2
      )
  })

  # ── MAPA EMPLEO: ACTUALIZACIÓN CON LEAFLETPROXY ──────────────
  observe({
    req(empleo_mapa_data())
    datos <- empleo_mapa_data()
    if (nrow(datos) == 0) return()
    modo  <- datos$modo[1]
    anio  <- datos$anio[1]
    mes   <- datos$mes[1]

    # Join con el shapefile de departamentos
    mapa_sf <- deptos_er |>
      left_join(datos, by = c("nombre_depto" = "nombre_departamento_indec"))

    if (modo == "cantidad") {
      max_val <- max(mapa_sf$valor, na.rm = TRUE)
      if (!is.finite(max_val)) max_val <- 1
      pal <- colorNumeric(
        palette  = c("#d73535", "#ea7b6f", "#f3b7ae", "#f1f1f1", "#bfe8df", "#87dfcd", "#35d4bc"),
        domain   = c(0, max(max_val, 1)),
        na.color = "#e2e8f0"
      )

      labels <- lapply(seq_len(nrow(mapa_sf)), function(i)
        htmltools::HTML(tooltip_empleo(
          mapa_sf$nombre_depto[i], mapa_sf$valor[i], "cantidad", NULL, anio, mes
        ))
      )

      leafletProxy("mapa_empleo", data = mapa_sf) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor    = ~pal(valor),
          fillOpacity  = 0.78,
          color        = "white",
          weight       = 1.2,
          smoothFactor = 1,
          label        = labels,
          labelOptions = labelOptions(
            style     = list("font-family" = "Inter, sans-serif", "font-size" = "13px",
                             "padding" = "6px 10px", "border-radius" = "6px"),
            direction = "auto",
            sticky    = FALSE
          ),
          highlightOptions = highlightOptions(
            weight      = 2.5,
            color       = "#2563eb",
            fillOpacity = 0.88,
            bringToFront = TRUE
          )
        )

    } else {
      # Modo especialización relativa — paleta por bins
      pal_lq <- colorBin(LQ_COLORS, bins = LQ_BINS, na.color = "#e2e8f0")

      labels <- lapply(seq_len(nrow(mapa_sf)), function(i)
        htmltools::HTML(tooltip_empleo(
          mapa_sf$nombre_depto[i], mapa_sf$valor[i], "especializacion", datos$sector[1], anio, mes
        ))
      )

      leafletProxy("mapa_empleo", data = mapa_sf) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor    = ~pal_lq(valor),
          fillOpacity  = 0.80,
          color        = "white",
          weight       = 1.2,
          smoothFactor = 1,
          label        = labels,
          labelOptions = labelOptions(
            style     = list("font-family" = "Inter, sans-serif", "font-size" = "13px",
                             "padding" = "6px 10px", "border-radius" = "6px"),
            direction = "auto",
            sticky    = FALSE
          ),
          highlightOptions = highlightOptions(
            weight       = 2.5,
            color        = "#2563eb",
            fillOpacity  = 0.9,
            bringToFront = TRUE
          )
        )
    }
  })

  # ── MESES DISPONIBLES SEGÚN AÑO ─────────────────────────────
  observeEvent(input$anio_emp, {
    req(input$anio_emp)
    meses_ok <- meses_de_anio(input$anio_emp)
    sel <- if (!is.null(input$mes_emp) && as.integer(input$mes_emp) %in% meses_ok)
      as.character(input$mes_emp) else as.character(meses_ok[1])
    updateSelectInput(session, "mes_emp",
      choices  = setNames(meses_ok, MESES_ES[meses_ok]),
      selected = sel)
  })

  observeEvent(input$anio_esp, {
    req(input$anio_esp)
    meses_ok <- meses_de_anio(input$anio_esp)
    sel <- if (!is.null(input$mes_esp) && as.integer(input$mes_esp) %in% meses_ok)
      as.character(input$mes_esp) else as.character(meses_ok[1])
    updateSelectInput(session, "mes_esp",
      choices  = setNames(meses_ok, MESES_ES[meses_ok]),
      selected = sel)
  })

  # ── RESET FILTROS EMPLEO ────────────────────────────────────
  observeEvent(input$reset_empleo, {
    updateRadioButtons(session, "modo_empleo", selected = "cantidad")
    # Los filtros dinámicos se resetean al recrearse el UI
  })

  # ── DATOS REACTIVOS: PARQUES FILTRADOS ──────────────────────
  parques_filtrados <- reactive({
    # Excluir parques sin coordenadas válidas (ej: Área Industrial Basavilbaso)
    df <- parques |> filter(!is.na(lat), !is.na(lon), !is.na(departamento))
    if (!is.null(input$filtro_dpto_parque) && input$filtro_dpto_parque != "") {
      df <- df |> filter(departamento == input$filtro_dpto_parque)
    }
    if (!is.null(input$solo_promo) && input$solo_promo == "true") {
      df <- df |> filter(tiene_promocion == TRUE)
    }
    if (length(input$servicio_infra) > 0) {
      for (col in input$servicio_infra) {
        df <- df |> filter(!is.na(.data[[col]]) & as.numeric(.data[[col]]) == 1)
      }
    }
    if (length(input$filtro_rubros) > 0) {
      sel <- tolower(input$filtro_rubros)
      df <- df |> filter(!is.na(rubros_instalados) & sapply(rubros_instalados, function(r) {
        any(tolower(trimws(unlist(strsplit(r, ",")))) %in% sel)
      }))
    }
    if (length(input$filtro_empresas) > 0) {
      sel <- tolower(input$filtro_empresas)
      df <- df |> filter(!is.na(empresas_instaladas) & sapply(empresas_instaladas, function(e) {
        any(tolower(trimws(unlist(strsplit(e, ",")))) %in% sel)
      }))
    }
    df
  })

  # ── MAPA PARQUES: RENDER INICIAL ────────────────────────────
  output$mapa_parques <- renderLeaflet({
    # Solo parques con coordenadas válidas para el mapa inicial
    parques_mapa <- parques |> filter(!is.na(lat), !is.na(lon), !is.na(departamento))

    # Paleta para desvío de servicios (divergente, centrada en 0)
    rng      <- range(parques_mapa$desvio_servicios, na.rm = TRUE)
    pal_desv <- colorNumeric(
      palette  = c("#003d5c", "#31497e", "#674f95", "#a14e9a", "#d44c8d", "#f9596f", "#ff7a47", "#ffa600"),
      domain   = c(min(rng) * 1.1, max(rng) * 1.1),
      na.color = "#d1d5db"
    )

    # Popups y labels
    popups_click  <- sapply(seq_len(nrow(parques_mapa)), function(i) popup_parque(parques_mapa[i, ]))
    labels_hover  <- lapply(
      seq_len(nrow(parques_mapa)),
      function(i) htmltools::HTML(label_parque(parques_mapa$nombre_parque[i], parques_mapa$departamento[i]))
    )

    # Parques con promoción (para marcador de corona dorada)
    parques_promo <- parques_mapa |> filter(tiene_promocion == TRUE)

    mapa <- leaflet() |>
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
        attribution = "© OpenStreetMap | © CartoDB"
      ) |>
      fitBounds(
        lng1 = ER_BOUNDS$lng1, lat1 = ER_BOUNDS$lat1,
        lng2 = ER_BOUNDS$lng2, lat2 = ER_BOUNDS$lat2
      ) |>
      # Límites departamentales
      addPolygons(
        data         = deptos_er,
        fillColor    = "transparent",
        fillOpacity  = 0,
        color        = "#94a3b8",
        weight       = 0.8,
        smoothFactor = 1,
        group        = "Departamentos",
        options      = pathOptions(interactive = FALSE)
      ) |>
      # Rutas nacionales
      addPolylines(
        data   = rutas_nac,
        color  = "#293033",
        weight = 1.5,
        opacity = 0.55,
        group  = "Rutas nacionales"
      ) |>
      # Rutas provinciales
      addPolylines(
        data   = rutas_prov,
        color  = "#d97706",
        weight = 1,
        opacity = 0.45,
        group  = "Rutas provinciales"
      ) |>
      # Puntos de parques industriales
      addCircleMarkers(
        data        = parques_mapa,
        lng         = ~lon, lat = ~lat,
        radius      = ~ifelse(is.na(radio_px), 6, pmax(radio_px, 6)),
        fillColor   = ~pal_desv(desvio_servicios),
        fillOpacity = 0.88,
        color       = "white",
        weight      = 1.8,
        stroke      = TRUE,
        popup       = popups_click,
        label       = labels_hover,
        labelOptions = labelOptions(
          style    = list("font-family" = "Inter, sans-serif", "font-size" = "13px"),
          direction = "auto"
        ),
        group       = "Parques"
      ) |>
      # Corona dorada para parques con promociones (no interactiva, no tapa el marker)
      addCircleMarkers(
        data        = parques_promo,
        lng         = ~lon, lat = ~lat,
        radius      = ~ifelse(is.na(radio_px), 6, pmax(radio_px, 6)) + 4.5,
        fillColor   = "transparent",
        fillOpacity = 0,
        color       = "#f59e0b",
        weight      = 2.5,
        stroke      = TRUE,
        options     = pathOptions(interactive = FALSE),
        group       = "Parques"
      ) |>
      # Puertos
      addCircleMarkers(
        data        = puertos,
        lng         = ~lon, lat = ~lat,
        radius      = 11,
        fillColor   = "#7c3aed",
        fillOpacity = 0.92,
        color       = "white",
        weight      = 2.5,
        popup       = sapply(seq_len(nrow(puertos)), function(i) paste0(
          '<div style="font-family:Inter,sans-serif;font-size:13px;min-width:220px;">',
          '<div class="popup-title">&#x2693; ', puertos$puerto[i], '</div>',
          '<div style="font-size:11px;color:#94a3b8;margin-bottom:5px;">', puertos$ubicacion[i], '</div>',
          '<div style="font-size:12px;color:#475569;line-height:1.55;">', PUERTOS_DESC[puertos$puerto[i]], '</div>',
          '</div>'
        )),
        label       = lapply(seq_len(nrow(puertos)), function(i)
          htmltools::HTML(paste0(
            "<b>", puertos$puerto[i], "</b>",
            "<br><small style='color:#64748b;'>", puertos$ubicacion[i], "</small>",
            "<br><span style='font-size:12px;color:#475569;'>", PUERTOS_DESC[puertos$puerto[i]], "</span>"
          ))
        ),
        group       = "Puertos"
      ) |>
      # Control de capas
      addLayersControl(
        overlayGroups = c("Departamentos", "Rutas nacionales", "Rutas provinciales", "Parques", "Puertos"),
        options       = layersControlOptions(collapsed = TRUE),
        position      = "topright"
      )

    mapa
  })

  # ── MAPA PARQUES: ACTUALIZACIÓN CON FILTROS ─────────────────
  observe({
    df <- parques_filtrados()
    proxy <- leafletProxy("mapa_parques")
    proxy |> clearGroup("Parques")

    if (nrow(df) == 0) return()

    rng      <- range(parques$desvio_servicios, na.rm = TRUE)
    pal_desv <- colorNumeric(
      palette  = c("#003d5c", "#31497e", "#674f95", "#a14e9a", "#d44c8d", "#f9596f", "#ff7a47", "#ffa600"),
      domain   = c(min(rng) * 1.1, max(rng) * 1.1),
      na.color = "#d1d5db"
    )

    popups_click <- sapply(seq_len(nrow(df)), function(i) popup_parque(df[i, ]))
    labels_hover <- lapply(
      seq_len(nrow(df)),
      function(i) htmltools::HTML(label_parque(df$nombre_parque[i], df$departamento[i]))
    )

    df_promo <- df |> filter(tiene_promocion == TRUE)

    proxy |>
      addCircleMarkers(
        data        = df,
        lng         = ~lon, lat = ~lat,
        radius      = ~ifelse(is.na(radio_px), 6, pmax(radio_px, 6)),
        fillColor   = ~pal_desv(desvio_servicios),
        fillOpacity = 0.88,
        color       = "white",
        weight      = 1.8,
        stroke      = TRUE,
        popup       = popups_click,
        label       = labels_hover,
        labelOptions = labelOptions(
          style    = list("font-family" = "Inter, sans-serif", "font-size" = "13px"),
          direction = "auto"
        ),
        group = "Parques"
      )

    if (nrow(df_promo) > 0) {
      proxy |>
        addCircleMarkers(
          data        = df_promo,
          lng         = ~lon, lat = ~lat,
          radius      = ~ifelse(is.na(radio_px), 6, pmax(radio_px, 6)) + 4.5,
          fillColor   = "transparent",
          fillOpacity = 0,
          color       = "#f59e0b",
          weight      = 2.5,
          stroke      = TRUE,
          options     = pathOptions(interactive = FALSE),
          group       = "Parques"
        )
    }
  })

  # ── LEYENDA PARQUES (panel lateral) ─────────────────────────
  output$leyenda_parques_ui <- renderUI({
    df  <- parques_filtrados()
    n   <- nrow(df)
    n_p <- sum(df$tiene_promocion == TRUE, na.rm = TRUE)
    tagList(
      tags$br(),
      tags$div(class = "custom-legend",
        tags$div(class = "legend-title", "Referencias"),
        tags$div(class = "legend-row",
          tags$div(class = "legend-dot", style = "background:#ffa600;"),
          "Muchos servicios (↑ promedio)"
        ),
        tags$div(class = "legend-row",
          tags$div(class = "legend-dot", style = "background:#e2e8f0;border:1.5px solid #94a3b8;"),
          "Promedio de servicios"
        ),
        tags$div(class = "legend-row",
          tags$div(class = "legend-dot", style = "background:#003d5c;"),
          "Pocos servicios (↓ promedio)"
        ),
        tags$div(class = "legend-row",
          tags$span(style = "display:inline-block;width:14px;height:14px;border-radius:50%;border:2.5px solid #f59e0b;background:transparent;flex-shrink:0;"),
          "Con promociones impositivas"
        ),
        tags$div(class = "legend-row",
          tags$div(class = "legend-dot", style = "background:#7c3aed;"),
          "Puerto"
        ),
        tags$hr(),
        tags$div(style = "font-size:12px;color:#64748b;",
          tags$b(n), " parques mostrados · ", tags$b(n_p), " con promociones"
        )
      )
    )
  })

  # ── RESET FILTROS PARQUES ───────────────────────────────────
  observeEvent(input$reset_parques, {
    updateSelectInput(session, "filtro_dpto_parque", selected = "")
    updateRadioButtons(session, "solo_promo", selected = "false")
    updateSelectizeInput(session, "servicio_infra",   selected = character(0))
    updateSelectizeInput(session, "filtro_rubros",    selected = character(0))
    updateSelectizeInput(session, "filtro_empresas",  selected = character(0))
  })

  # ── HEATMAP SECTORIAL ───────────────────────────────────────
  output$heatmap_sectores <- renderPlotly({
    # Ordenar sectores por frecuencia total (más comunes al inicio)
    sector_order <- rubros_long |>
      group_by(rubro) |>
      summarise(total = sum(frecuencia), .groups = "drop") |>
      arrange(desc(total)) |>
      pull(rubro)

    dept_order <- sort(unique(deptos_er$nombre_depto))

    df_plot <- expand.grid(
      departamento = dept_order,
      rubro        = sector_order,
      stringsAsFactors = FALSE
    ) |>
      left_join(rubros_long, by = c("departamento", "rubro")) |>
      replace_na(list(frecuencia = 0L)) |>
      mutate(
        rubro        = factor(rubro,        levels = sector_order),
        departamento = factor(departamento, levels = dept_order)
      )

    plot_ly(
      data = df_plot,
      x    = ~departamento,
      y    = ~rubro,
      z    = ~frecuencia,
      type = "heatmap",
      colorscale = list(
        c(0,    "#2a2a2a"),
        c(0.25, "#ff7a47"),
        c(0.5,  "#d44c8d"),
        c(0.75, "#674f95"),
        c(1,    "#003d5c")
      ),
      showscale  = TRUE,
      hovertemplate = paste0(
        "<b>Departamento:</b> %{x}<br>",
        "<b>Sector:</b> %{y}<br>",
        "<b>Parques con ese sector:</b> %{z}",
        "<extra></extra>"
      ),
      colorbar = list(
        title    = list(text = "Parques<br>con el sector", font = list(size = 12, color = color_texto)),
        tickfont = list(color = color_texto),
        len      = 0.6,
        thickness = 14,
        outlinewidth = 0
      )
    ) |>
      layout(
        xaxis = list(
          title      = "",
          tickangle  = -45,
          tickfont   = list(family = "Inter, sans-serif", size = 11, color = color_texto),
          showgrid   = FALSE,
          automargin = TRUE
        ),
        yaxis = list(
          title    = "",
          tickfont = list(family = "Inter, sans-serif", size = 11, color = color_texto),
          showgrid = FALSE,
          autorange = "reversed"
        ),
        margin      = list(l = 230, b = 130, t = 10, r = 20),
        paper_bgcolor = color_fondo,
        plot_bgcolor  = color_fondo,
        font = list(family = "Inter, sans-serif", color = color_texto)
      ) |>
      config(
        displaylogo   = FALSE,
        modeBarButtons = list(list("toImage")),
        toImageButtonOptions = list(format = "svg")
      )
  })
}

# ===== EJECUTAR APLICACIÓN =====
shinyApp(ui = ui, server = server)
