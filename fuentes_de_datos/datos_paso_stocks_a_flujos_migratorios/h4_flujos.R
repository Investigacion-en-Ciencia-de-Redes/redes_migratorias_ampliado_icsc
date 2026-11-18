# =============================================================================
# H4 (migración forzada) con FLUJOS estimados
#
# Reemplaza a la versión anterior de h4_flujos.R: incluye todo lo que hacía
# (parte A) y agrega el barrido de episodios (parte B) y los ejemplos
# adicionales (parte C).
#
#   A. Casos principales (Siria, Venezuela, Ucrania): tabla y tres figuras
#   B. Barrido de todos los países: saltos bruscos de emigración (exploratorio)
#   C. Ejemplos adicionales seleccionados: tabla con contexto y una figura
#      resumen con la proporción de salidas a países limítrofes en todos los casos
#
# Entradas:
#   flujos_long.csv      flujos estimados (method, year0, orig, dest, flow,
#                        year1, period, years). orig/dest = país de residencia
#                        anterior/nueva (no país de nacimiento como en stocks).
#   limitrofes_iso3.csv  pares de países con frontera terrestre (orig, dest en
#                        ISO3), derivado de GeoDataSource country-borders.
#   WPP (opcional)       para tasas de emigración por mil habitantes
#                        (WPP2024_Demographic_Indicators_Medium, CSV de
#                        https://population.un.org/wpp/). Si no está, se omite.
#
# Fuente de limitrofes_iso3.csv: GeoDataSource Country Borders
#   (https://github.com/geodatasource/country-borders), con los códigos ISO2
#   convertidos a ISO3. Contiene un par por fila (orig, dest) para cada par de
#   países con frontera terrestre.
#
# Paquetes: readr, dplyr, tidyr, ggplot2.
#
# Uso: ajustar las rutas de la sección "Configuración" a la estructura del
#   repositorio. Por defecto son relativas a la carpeta raíz del proyecto, que
#   puede indicarse con la variable de entorno H4_RAIZ, por ejemplo:
#   Sys.setenv(H4_RAIZ = "/ruta/al/repo"); source("h4_flujos.R")
#
# Salidas (en dir_out):
#   h4_tabla_principales.csv / .tex     tabla de los tres casos principales
#   h4_tabla_ejemplos.csv / .tex        tabla de ejemplos adicionales
#   h4_episodios_detectados.csv         barrido exploratorio (no va al paper)
#   fig_h4_emigracion.pdf/png           emigración anual por método
#   fig_h4_tipo_destino.pdf/png         composición de las salidas
#   fig_h4_principales_destinos.pdf/png destinos antes vs. durante la crisis
#   fig_h4_resumen_limitrofes.pdf/png   % a limítrofes en todos los casos
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
})

# ---------------------------- Configuración ---------------------------------
raiz            <- Sys.getenv("H4_RAIZ", ".")    # carpeta raíz del proyecto
ruta_flujos     <- file.path(raiz, "output_paper_red", "flujos_long.csv")
ruta_limitrofes <- file.path(raiz, "data", "limitrofes_iso3.csv")
ruta_wpp        <- file.path(raiz, "data", "WPP2024_Demographic_Indicators_Medium.gz")  # NULL para omitir
dir_out         <- file.path(raiz, "output_h4")
metodo_ref      <- "da_pb_closed"
metodos_rob     <- c("da_min_closed", "da_min_open", "sd_drop_neg", "sd_rev_neg")  # chequeo de robustez
umbral_mult     <- 3        # barrido: la emigración al menos se triplica...
umbral_anual    <- 50000    # ...y supera estas salidas por año
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
for (f in c(ruta_flujos, ruta_limitrofes)) {
  if (!file.exists(f)) stop("No se encontró ", f, ". Revisar las rutas de la sección Configuración.")
}

# Casos principales: país, nombre, año de inicio, período previo y de la crisis
casos <- tribble(
  ~pais, ~nombre,     ~inicio, ~pre,        ~crisis,
  "SYR", "Syria",     2011,    "2005-2010", "2010-2015",
  "VEN", "Venezuela", 2015,    "2010-2015", "2015-2020",
  "UKR", "Ukraine",   2022,    "2015-2020", "2020-2024"
)
# Región de los casos principales (subregión M49; para Venezuela, América del Sur)
region <- list(
  SYR = c("ARM","AZE","BHR","CYP","GEO","IRQ","ISR","JOR","KWT","LBN","OMN",
          "QAT","SAU","PSE","TUR","ARE","YEM"),
  VEN = c("ARG","BOL","BRA","CHL","COL","ECU","GUY","PRY","PER","SUR","URY"),
  UKR = c("BLR","BGR","CZE","HUN","POL","MDA","ROU","RUS","SVK")
)

# Ejemplos adicionales. pre = NA cuando la crisis cae en el primer período
# (los datos empiezan en 1990 y no hay período previo).
ejemplos <- tribble(
  ~pais, ~nombre,                    ~pre,        ~crisis,     ~contexto,                                          ~grupo,
  "IRQ", "Iraq",                     "2000-2005", "2005-2010", "Sectarian civil war (2006--2007)",                 "Supports H4",
  "SDN", "Sudan",                    "2015-2020", "2020-2024", "War between SAF and RSF (2023--)",                 "Supports H4",
  "SSD", "South Sudan",              "2005-2010", "2010-2015", "Civil war (2013--2018)",                           "Supports H4",
  "CAF", "Central African Republic", "2005-2010", "2010-2015", "S\\'el\\'eka coup and civil conflict (2013--)",    "Supports H4",
  "AFG", "Afghanistan",              "2015-2020", "2020-2024", "Taliban takeover (2021)",                          "Supports H4",
  "MMR", "Myanmar",                  "2010-2015", "2015-2020", "Military operations against the Rohingya (2017)",  "Supports H4",
  "RWA", "Rwanda",                   NA,          "1990-1995", "Civil war and genocide (1990--1994)",              "Supports H4",
  "SOM", "Somalia",                  NA,          "1990-1995", "State collapse and civil war (1991--)",            "Supports H4",
  "SLE", "Sierra Leone",             NA,          "1990-1995", "Civil war (1991--2002)",                           "Supports H4",
  "BDI", "Burundi",                  NA,          "1990-1995", "Civil war (1993--2005)",                           "Supports H4",
  "AZE", "Azerbaijan",               NA,          "1990-1995", "Nagorno-Karabakh war (1988--1994)",                "Supports H4",
  "BIH", "Bosnia and Herzegovina",   NA,          "1990-1995", "Bosnian war (1992--1995)",                         "Partial exception",
  "YEM", "Yemen",                    "2010-2015", "2015-2020", "Civil war and Saudi-led intervention (2015--)",    "Limited outflows"
)

# ---------------------------- Datos ------------------------------------------
flujos <- read_csv(ruta_flujos, show_col_types = FALSE)
pares_limitrofes <- read_csv(ruta_limitrofes, show_col_types = FALSE) |>
  transmute(clave = paste(orig, dest))
orden_periodos <- sort(unique(flujos$period))

flujos <- flujos |>
  mutate(limitrofe = paste(orig, dest) %in% pares_limitrofes$clave)

pob <- NULL
if (!is.null(ruta_wpp) && file.exists(ruta_wpp)) {
  pob <- read_csv(ruta_wpp, show_col_types = FALSE, guess_max = 1e5) |>
    filter(Variant == "Medium", !is.na(ISO3_code)) |>
    transmute(pais = ISO3_code, year0 = Time, pop = TPopulation1July * 1000)
}

# Indicadores de emigración para todos los países, períodos y métodos
emigracion <- flujos |>
  group_by(method, pais = orig, period, year0, years) |>
  summarise(emig = sum(flow), vecinos = sum(flow[limitrofe]), .groups = "drop") |>
  mutate(emig_anual = emig / years, pct_vecinos = 100 * vecinos / emig) |>
  arrange(method, pais, period) |>
  group_by(method, pais) |>
  mutate(emig_anual_prev = lag(emig_anual), pct_vecinos_prev = lag(pct_vecinos),
         multiplicador = emig_anual / emig_anual_prev) |>
  ungroup()
if (!is.null(pob)) {
  emigracion <- emigracion |> left_join(pob, by = c("pais", "year0")) |>
    mutate(tasa_emig_mil = 1000 * emig_anual / pop)
}

# Proporción a limítrofes según los métodos de robustez (formato ancho)
robustez <- emigracion |> filter(method %in% metodos_rob) |>
  select(method, pais, period, pct_vecinos) |>
  pivot_wider(names_from = method, values_from = pct_vecinos, names_prefix = "pct_vecinos_")

# Principales destinos (método de referencia)
top_destinos <- function(n) {
  flujos |> filter(method == metodo_ref) |>
    group_by(pais = orig, period) |> slice_max(flow, n = n, with_ties = FALSE) |>
    summarise(top = paste(dest, collapse = ", "), .groups = "drop")
}
top3 <- top_destinos(3)

# Tema y paleta comunes
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.title = element_blank())
etiquetas_metodo <- c(da_pb_closed = "Pseudo-Bayesian", da_min_closed = "Minimum migration",
                      sd_drop_neg = "Stock difference")
colores_metodo <- c("Pseudo-Bayesian" = "#2a78d6", "Minimum migration" = "#eb6834",
                    "Stock difference" = "#1baf7a")
guardar <- function(g, nombre, ancho, alto) {
  dispositivo_pdf <- if (capabilities("cairo")) cairo_pdf else pdf
  ggsave(file.path(dir_out, paste0(nombre, ".pdf")), g, width = ancho, height = alto,
         device = dispositivo_pdf)
  ggsave(file.path(dir_out, paste0(nombre, ".png")), g, width = ancho, height = alto, dpi = 200)
}
fmt_miles <- function(x) formatC(round(x / 1000), format = "d", big.mark = ",")
guion <- function(p) sub("-", "--", p)          # 2010-2015 -> 2010--2015 en LaTeX

# =============================================================================
# A. Casos principales
# =============================================================================
tipo_destino <- function(pais, dest, limitrofe) {
  case_when(limitrofe                ~ "Neighboring",
            dest %in% region[[pais]] ~ "Rest of region",
            TRUE                     ~ "Outside region")
}
salidas <- bind_rows(lapply(casos$pais, function(p) {
  flujos |> filter(orig == p) |> mutate(pais = p, tipo = tipo_destino(p, dest, limitrofe))
}))
pct_region <- salidas |> filter(method == metodo_ref) |>
  group_by(pais, period) |>
  summarise(pct_region = 100 * sum(flow[tipo != "Outside region"]) / sum(flow), .groups = "drop")

tabla_a <- emigracion |> filter(method == metodo_ref) |>
  inner_join(casos, by = "pais") |>
  filter(period >= pre) |>
  group_by(pais) |>
  mutate(mult_pre = emig_anual / emig_anual[period == pre[1]]) |>
  ungroup() |>
  left_join(pct_region, by = c("pais", "period")) |>
  left_join(top3, by = c("pais", "period")) |>
  left_join(robustez, by = c("pais", "period")) |>
  arrange(match(pais, casos$pais), period) |>
  select(pais, nombre, period, emig_anual, mult_pre, any_of("tasa_emig_mil"),
         pct_vecinos, pct_region, top, starts_with("pct_vecinos_"))
write_csv(tabla_a, file.path(dir_out, "h4_tabla_principales.csv"))

filas_a <- tabla_a |>
  mutate(linea = sprintf("%s & %s & %s & %.1f & %.1f & %.1f & %s \\\\",
                         ifelse(duplicated(nombre), "", nombre), guion(period),
                         fmt_miles(emig_anual), mult_pre, pct_vecinos, pct_region, top)) |>
  group_by(pais) |> summarise(bloque = paste(linea, collapse = "\n"), .groups = "drop") |>
  arrange(match(pais, casos$pais))
writeLines(c(
  "\\begin{table}[tbhp]", "\\centering",
  "\\caption{Estimated emigration flows (pseudo-Bayesian) before and after each crisis. Outflows in thousands per year; multiplier ($\\times$ pre-crisis) relative to the pre-crisis period, i.e., the first row of each country; share of outflows to neighboring countries and to the same region, the latter including neighboring countries (see Section~\\ref{sec:h4_flows} for the definition of regions); three main destinations.}",
  "\\label{tab:flujos_forzados}", "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{adjustbox}{max width=\\linewidth}",
  "\\begin{tabular}{llrrrrl}", "\\toprule",
  "Country & Period & Outflows & $\\times$ pre-crisis & \\% neighb. & \\% region & Main destinations \\\\",
  "\\midrule", paste(filas_a$bloque, collapse = "\n\\midrule\n"),
  "\\bottomrule", "\\end{tabular}", "\\end{adjustbox}", "\\end{table}"),
  file.path(dir_out, "h4_tabla_principales.tex"))

nombres <- setNames(casos$nombre, casos$pais)

# Figura A1: emigración anual por período, tres métodos
d1 <- emigracion |> filter(pais %in% casos$pais, method %in% c(metodo_ref, "da_min_closed", "sd_drop_neg")) |>
  mutate(metodo = factor(etiquetas_metodo[method], levels = names(colores_metodo)),
         pais_n = factor(nombres[pais], levels = casos$nombre),
         x = year0 + years / 2)
g1 <- ggplot(d1, aes(x, emig_anual / 1e6, colour = metodo)) +
  geom_vline(data = casos |> mutate(pais_n = factor(nombre, levels = casos$nombre)),
             aes(xintercept = inicio), linetype = 2, colour = "grey55") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  facet_wrap(~ pais_n, scales = "free_y") +
  scale_colour_manual(values = colores_metodo) +
  labs(x = "Period midpoint", y = "Outflows per year (millions)",
       caption = "Dashed line: onset of the crisis.") +
  tema
guardar(g1, "fig_h4_emigracion", 7, 3.2)

# Figura A2: composición de las salidas por tipo de destino
d2 <- salidas |> filter(method == metodo_ref) |>
  group_by(pais, period, tipo) |> summarise(flow = sum(flow), .groups = "drop") |>
  group_by(pais, period) |> mutate(pct = 100 * flow / sum(flow)) |> ungroup() |>
  mutate(tipo = factor(tipo, levels = c("Outside region", "Rest of region", "Neighboring")),
         pais_n = factor(nombres[pais], levels = casos$nombre),
         periodo = factor(sub("-", "\u2013", period), levels = sub("-", "\u2013", orden_periodos)))
marca <- casos |> mutate(pais_n = factor(nombre, levels = casos$nombre),
                         periodo = factor(sub("-", "\u2013", crisis), levels = levels(d2$periodo)))
g2 <- ggplot(d2, aes(periodo, pct, fill = tipo)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.4) +
  geom_text(data = marca, aes(periodo, 104, label = "crisis"), inherit.aes = FALSE,
            size = 3, colour = "grey30") +
  facet_wrap(~ pais_n) +
  scale_fill_manual(values = c("Neighboring" = "#184f95", "Rest of region" = "#86b6ef",
                               "Outside region" = "#d9d8d2"),
                    breaks = c("Neighboring", "Rest of region", "Outside region")) +
  scale_y_continuous(breaks = seq(0, 100, 25), expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "Share of outflows (%)") +
  tema + theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(g2, "fig_h4_tipo_destino", 7, 3.6)

# Figura A3: principales destinos, período previo vs. período de la crisis
d3 <- bind_rows(lapply(seq_len(nrow(casos)), function(i) {
  cs <- casos[i, ]
  base <- salidas |> filter(method == metodo_ref, pais == cs$pais,
                            period %in% c(cs$pre, cs$crisis)) |>
    mutate(anual = flow / years, momento = ifelse(period == cs$pre, "Pre-crisis", "Crisis"))
  top <- base |> filter(momento == "Crisis") |> slice_max(anual, n = 6) |> pull(dest)
  base |> filter(dest %in% top) |>
    mutate(pais_n = cs$nombre, dest_lab = paste0(dest, ifelse(limitrofe, " *", "")))
})) |>
  group_by(pais_n, dest_lab) |> mutate(max_crisis = max(anual[momento == "Crisis"], 0)) |> ungroup() |>
  mutate(pais_n = factor(pais_n, levels = casos$nombre),
         momento = factor(momento, levels = c("Crisis", "Pre-crisis")),   # pre arriba en cada par
         y = reorder(paste(pais_n, dest_lab, sep = "___"), max_crisis))
g3 <- ggplot(d3, aes(anual / 1e3, y, fill = momento)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~ pais_n, scales = "free") +
  scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
  scale_fill_manual(values = c("Pre-crisis" = "#bdbcb4", "Crisis" = "#2a78d6"),
                    breaks = c("Pre-crisis", "Crisis")) +
  labs(x = "Outflows per year (thousands)", y = NULL,
       caption = "* neighboring country.") +
  tema + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line())
guardar(g3, "fig_h4_principales_destinos", 7, 3.6)

# =============================================================================
# B. Barrido exploratorio: saltos bruscos de emigración en todos los países
#    No todos son crisis: incluye migración laboral, retornos de refugiados
#    (los flujos son por residencia) y revisiones de datos. Revisar caso a caso.
# =============================================================================
episodios <- emigracion |>
  filter(method == metodo_ref, multiplicador >= umbral_mult, emig_anual >= umbral_anual) |>
  left_join(robustez, by = c("pais", "period")) |>
  left_join(top_destinos(4), by = c("pais", "period")) |>
  arrange(desc(emig_anual)) |>
  select(pais, period, emig_anual_prev, emig_anual, multiplicador,
         pct_vecinos_prev, pct_vecinos, starts_with("pct_vecinos_"), top)
write_csv(episodios, file.path(dir_out, "h4_episodios_detectados.csv"))

# =============================================================================
# C. Ejemplos adicionales
# =============================================================================
tabla_c <- ejemplos |>
  left_join(emigracion |> filter(method == metodo_ref) |>
              select(pais, period, emig_anual, pct_vecinos),
            by = c("pais", "crisis" = "period")) |>
  left_join(emigracion |> filter(method == metodo_ref) |>
              select(pais, period, emig_pre = emig_anual, pct_vecinos_pre = pct_vecinos),
            by = c("pais", "pre" = "period")) |>
  left_join(robustez, by = c("pais", "crisis" = "period")) |>
  left_join(top3, by = c("pais", "crisis" = "period")) |>
  mutate(mult_pre = emig_anual / emig_pre)
write_csv(tabla_c, file.path(dir_out, "h4_tabla_ejemplos.csv"))

fmt_num <- function(x, d = 1) ifelse(is.na(x), "--", formatC(x, format = "f", digits = d))
filas_c <- tabla_c |>
  mutate(linea = sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
                         nombre, guion(crisis), contexto, fmt_miles(emig_anual),
                         fmt_num(mult_pre), fmt_num(pct_vecinos), top))
writeLines(c(
  "\\begin{table}[tbhp]", "\\centering",
  "\\caption{Additional examples of crisis-driven emigration (pseudo-Bayesian flows). Outflows in thousands per year in the period that contains the onset of the crisis; multiplier relative to the pre-crisis period, i.e., the previous five-year period (not available for 1990--1995, the first period); share of outflows to neighboring countries; three main destinations.}",
  "\\label{tab:ejemplos_forzados}", "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{adjustbox}{max width=\\linewidth}",
  "\\begin{tabular}{lllrrrl}", "\\toprule",
  "Country & Period & Context & Outflows & $\\times$ pre-crisis & \\% neighb. & Main destinations \\\\",
  "\\midrule",
  paste(filas_c$linea[filas_c$grupo == "Supports H4"], collapse = "\n"),
  "\\midrule",
  paste(filas_c$linea[filas_c$grupo != "Supports H4"], collapse = "\n"),
  "\\bottomrule", "\\end{tabular}", "\\end{adjustbox}", "\\end{table}"),
  file.path(dir_out, "h4_tabla_ejemplos.tex"))

# Figura C1: % de salidas a limítrofes en el período de la crisis, todos los casos,
# con el rango entre métodos como chequeo de robustez
todos <- bind_rows(
  casos |> transmute(pais, nombre, crisis,
                     grupo = ifelse(pais == "UKR", "Partial exception", "Supports H4")),
  ejemplos |> select(pais, nombre, crisis, grupo)) |>
  mutate(etiqueta = paste0(nombre, " (", sub("-", "\u2013", crisis), ")"))
d4 <- todos |>
  left_join(emigracion |> filter(method %in% c(metodo_ref, metodos_rob)) |>
              select(method, pais, period, pct_vecinos),
            by = c("pais", "crisis" = "period")) |>
  mutate(metodo = factor(etiquetas_metodo[method], levels = names(colores_metodo)))
orden4 <- d4 |> filter(method == metodo_ref) |> arrange(pct_vecinos) |> pull(etiqueta)
d4 <- d4 |> mutate(etiqueta = factor(etiqueta, levels = orden4),
                   grupo = factor(grupo, levels = c("Supports H4", "Partial exception", "Limited outflows")))
rango <- d4 |> group_by(etiqueta, grupo) |>
  summarise(min = min(pct_vecinos), max = max(pct_vecinos), .groups = "drop")
g4 <- ggplot() +
  geom_segment(data = rango, aes(x = min, xend = max, y = etiqueta, yend = etiqueta),
               colour = "grey75", linewidth = 2.2, lineend = "round") +
  geom_point(data = d4 |> filter(method != metodo_ref) |>
               mutate(familia = ifelse(grepl("^da_", method),
                                       "Minimum migration",
                                       "Stock difference")),
             aes(pct_vecinos, etiqueta, shape = familia), fill = "white", colour = "grey35", size = 1.8) +
  scale_shape_manual(values = c("Minimum migration" = 21,
                                "Stock difference" = 24)) +
  geom_point(data = d4 |> filter(method == metodo_ref),
             aes(pct_vecinos, etiqueta, fill = grupo), shape = 21, colour = "white", size = 3.2) +
  scale_fill_manual(values = c("Supports H4" = "#2a78d6", "Partial exception" = "#eb6834",
                               "Limited outflows" = "#eda100")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "Share of outflows to neighboring countries (%)", y = NULL,
       caption = "Filled dots: pseudo-Bayesian estimates. Open marks: the other four estimators.\nGrey bars: range across the five estimators.") +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 3.2)),
         shape = guide_legend(order = 2)) +
  tema + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(),
               legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
guardar(g4, "fig_h4_resumen_limitrofes", 6.5, 5)

message("Listo. Resultados en ", normalizePath(dir_out))
print(tabla_a, n = Inf, width = Inf)
print(tabla_c |> select(nombre, crisis, emig_anual, mult_pre, pct_vecinos, starts_with("pct_vecinos_"), top),
      n = Inf, width = Inf)
