# =============================================================================
# De stocks a flujos migratorios: procedimiento de Abel & Cohen (2019)
#
# El MISMO código estima flujos migratorios bilaterales con tres juegos de datos:
#
#   fuente = "abel2019"  : datos de Abel & Cohen (UN IMS 2017 + WPP 2017,
#                          1990-2015, 200 países). Valida la réplica contra la
#                          Tabla 7 del paper y, opcionalmente, contra las
#                          estimaciones publicadas (versión 1 de figshare).
#   fuente = "onu2024"   : datos ONU actuales leídos del Excel original
#                          (UN IMS 2024 + WPP 2024, 1990-2024).
#   fuente = "paper_red" : datos depurados del paper "The Shrinking Globe"
#                          (migraciones.csv y poblaciones.csv: UN IMS 2024 y
#                          WPP 2024, 201 países, 1990-2024) + nacimientos y
#                          muertes de WPP 2024.
#
# Para "onu2024" y "paper_red" se compara con las estimaciones actuales de
# Abel & Cohen en figshare (IMS 2020 + WPP 2022), si están disponibles.
# Como el código es idéntico, las diferencias entre corridas se deben solo a
# los datos.
#
#   Abel, G. J. & Cohen, J. E. (2019). Bilateral international migration flow
#   estimates for 200 countries. Scientific Data 6, 82.
#   https://doi.org/10.1038/s41597-019-0089-3
#   Datos y código originales: https://doi.org/10.6084/m9.figshare.c.4470464
#
# Métodos (mismos nombres de columna que en el paper):
#   sd_drop_neg   : diferencia de stocks, se descartan las negativas
#   sd_rev_neg    : diferencia de stocks, las negativas se invierten (retorno)
#   da_min_open   : contabilidad demográfica, migración mínima, sistema abierto
#   da_min_closed : contabilidad demográfica, migración mínima, sistema cerrado
#   da_pb_closed  : pseudo-bayesiano = 0.87 * da_min_closed + 0.13 * independiente
#   (mig_rate no se calcula)
#
# Es una reimplementación del script original de los autores (estimation.R)
# para la versión actual de migest (>= 2.0), cuyos argumentos cambiaron de nombre.
#
# -----------------------------------------------------------------------------
# ESTRUCTURA DE CARPETAS (crearla antes de correr el script)
#
#   <carpeta_base>/                   <- definir su ruta en "Configuración"
#     Data/
#       abel2019/                     fuente = "abel2019"
#         stock.csv                   pob, por, year, stock (personas)
#         pop.csv                     year, name, alpha3, pop (miles)
#         demog.csv                   period, year0, name, alpha3, b, d, net
#                                     (miles, por quinquenio)
#         abel_v1.csv                 OPCIONAL: estimaciones publicadas en la
#                                     versión 1 (2019) de figshare
#       onu2024/                      fuente = "onu2024" (y WPP para "paper_red")
#         undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx
#         WPP2024_Demographic_Indicators_Medium.csv.gz
#         bilat_mig.csv               OPCIONAL: estimaciones actuales de
#                                     Abel & Cohen (para comparar)
#       paper_red/                    fuente = "paper_red"
#         migraciones.csv             cod_orig, iso3_orig, iso3_des, año,
#                                     migrantes (personas)
#         poblaciones.csv             iso3_pobla, año_pobla, poblacion (personas)
#     output_abel2019/                se crean solos
#     output_onu2024/
#     output_paper_red/
#
# "paper_red" toma nacimientos y muertes del CSV de WPP 2024 de Data/onu2024/
# (se puede cambiar en ruta_wpp).
#
# DÓNDE DESCARGAR CADA ARCHIVO
#   stock.csv, pop.csv, demog.csv : figshare de los autores, ítem "R code and
#       input data" (carpeta data-input):
#       https://doi.org/10.6084/m9.figshare.c.4470464
#   abel_v1.csv : mismo figshare, ítem de estimaciones origen-destino; elegir
#       "Version 1" (2019) en el selector de versiones y renombrar el CSV.
#   bilat_mig.csv : mismo ítem, versión actual (renombrar el CSV).
#   Excel de stocks ONU 2024 : UN DESA, International Migrant Stock 2024,
#       matriz origen-destino ("by sex, destination and origin"):
#       https://www.un.org/development/desa/pd/content/international-migrant-stock
#   CSV de WPP 2024 : World Population Prospects 2024, Demographic Indicators,
#       variante media (Population > CSV format):
#       https://population.un.org/wpp/downloads
#   migraciones.csv, poblaciones.csv : repositorio del paper "The Shrinking
#       Globe" (UN IMS 2024 + WPP 2024 depurados, 201 países):
#       https://github.com/RexCaecos/evolucion_redes_migratorias
#   Si algún archivo se guarda con otro nombre, cambiarlo en "Configuración".
#
# SALIDAS (en <carpeta_base>/output_<fuente>/):
#   flujos_long.csv          flujos por período, par y método (> 0)
#   totales.csv              totales mundiales por período y método
#   diagnostico_ipfp.csv     control técnico del ajuste IPF
#   paises_usados.csv        países incluidos
#   solo "abel2019":  tabla7_vs_paper.csv
#   con referencia:   vs_abel.csv, vs_abel_paises.csv, fig_vs_abel.png
#
# PAQUETES: migest (>= 2.0), mipfp, readxl, dplyr, tidyr, readr, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(migest)    # >= 2.0
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

# ---------------------------- Configuración ---------------------------------
# >>> 1. ELEGIR los datos: "abel2019", "onu2024" o "paper_red"
fuente <- "paper_red"

# >>> 2. RUTA de la carpeta base (la que contiene Data/). Ejemplos:
#        Mac/Linux: "/Users/usuario/proyecto_migraciones"
#        Windows:   "C:/Users/usuario/proyecto_migraciones"  (usar / y no \\)
carpeta_base <- "RUTA/A/LA/CARPETA_BASE"

# >>> 3. ARCHIVOS (cambiar solo si se guardaron con otro nombre o en otro lugar)
dir_data_abel   <- file.path(carpeta_base, "Data", "abel2019")
dir_data_onu    <- file.path(carpeta_base, "Data", "onu2024")
dir_data_red    <- file.path(carpeta_base, "Data", "paper_red")
# abel2019
ruta_ref_abel   <- file.path(dir_data_abel, "abel_v1.csv")          # opcional
# onu2024
ruta_stock_onu  <- file.path(dir_data_onu, "undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx")
# WPP 2024 (nacimientos y muertes para onu2024 y paper_red; población para onu2024)
ruta_wpp        <- file.path(dir_data_onu, "WPP2024_Demographic_Indicators_Medium.csv.gz")
# paper_red
ruta_mig_red    <- file.path(dir_data_red, "migraciones.csv")
ruta_pob_red    <- file.path(dir_data_red, "poblaciones.csv")
# Estimaciones actuales de Abel & Cohen, para comparar onu2024 y paper_red (opcional)
ruta_ref_actual <- file.path(dir_data_onu, "bilat_mig.csv")

# >>> 4. AÑOS de los stocks para onu2024 (deben existir como columnas en el Excel)
anios_onu <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)

# >>> 5. OPCIONAL: TRUE para que onu2024 o paper_red usen solo los 200 países
#        de Abel & Cohen (requiere pop.csv y demog.csv en dir_data_abel).
paises_como_abel <- FALSE

w_pb <- 0.87          # peso del paper (Azose & Raftery 2018, citado por Abel & Cohen 2019)

if (carpeta_base == "RUTA/A/LA/CARPETA_BASE" || !dir.exists(carpeta_base))
  stop("Definir carpeta_base en la sección Configuración (ruta de la carpeta que contiene Data/).")
if (!fuente %in% c("abel2019", "onu2024", "paper_red"))
  stop('fuente debe ser "abel2019", "onu2024" o "paper_red"')
dir_out <- file.path(carpeta_base, paste0("output_", fuente))
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# Países con datos completos en Abel & Cohen (para paises_como_abel)
paises_abel <- function() {
  p <- read_csv(file.path(dir_data_abel, "pop.csv"), show_col_types = FALSE)
  d <- read_csv(file.path(dir_data_abel, "demog.csv"), show_col_types = FALSE)
  intersect(p$alpha3[!is.na(p$pop)], d$alpha3[!is.na(d$b) & !is.na(d$d)])
}

# ------------------- 1. Lectura de datos ------------------------------------
# Cualquiera sea la fuente, se arman tres tablas con el formato de Abel & Cohen:
#   d0: pob, por, year, stock      (stocks, en personas)
#   d1: year0, year1, alpha3, b, d (nacimientos y muertes del período, en miles)
#   d2: year, alpha3, pop          (población a mitad de año, en miles)

# WPP 2024: población al 1/7 (miles) y nacimientos/muertes por año calendario (miles)
leer_wpp <- function(ruta, anios) {
  if (!file.exists(ruta)) stop("No se encuentra ", ruta)
  read_csv(ruta, show_col_types = FALSE, guess_max = 1e5) |>
    filter(Variant == "Medium", LocTypeName == "Country/Area",
           Time >= min(anios), Time <= max(anios), !is.na(ISO3_code)) |>
    transmute(code = LocID, alpha3 = ISO3_code, year = Time,
              pop = TPopulation1July, births = Births, deaths = Deaths)
}
# Nacimientos y muertes entre el 1/7 de t0 y el 1/7 de t1: WPP 2024 publica
# años calendario, así que se suma medio año t0 + años intermedios + medio
# año t1. (WPP 2017, usado por Abel & Cohen, publicaba quinquenios julio-junio.)
demog_desde_wpp <- function(wpp, anios) {
  bind_rows(lapply(seq_len(length(anios) - 1), function(i) {
    t0 <- anios[i]; t1 <- anios[i + 1]
    wpp |> filter(year >= t0, year <= t1) |>
      mutate(peso = ifelse(year %in% c(t0, t1), 0.5, 1)) |>
      group_by(alpha3) |>
      summarise(b = sum(births * peso), d = sum(deaths * peso), .groups = "drop") |>
      mutate(year0 = t0, year1 = t1, .before = 1)
  }))
}

if (fuente == "abel2019") {
  for (f in c("stock.csv", "pop.csv", "demog.csv"))
    if (!file.exists(file.path(dir_data_abel, f))) stop("Falta ", f, " en ", dir_data_abel)
  d0 <- read_csv(file.path(dir_data_abel, "stock.csv"), guess_max = 1e6, show_col_types = FALSE)
  d1 <- read_csv(file.path(dir_data_abel, "demog.csv"), show_col_types = FALSE) |>
    filter(!is.na(year0)) |> mutate(year1 = year0 + 5) |> select(year0, year1, alpha3, b, d)
  d2 <- read_csv(file.path(dir_data_abel, "pop.csv"), show_col_types = FALSE) |>
    select(year, alpha3, pop)
  anios <- sort(unique(d0$year))
  ruta_ref <- ruta_ref_abel

} else if (fuente == "onu2024") {
  if (!file.exists(ruta_stock_onu)) stop("No se encuentra ", ruta_stock_onu)
  anios <- anios_onu

  # Stocks: el Excel tiene varias filas de encabezado; se busca la fila con
  # "Location code of destination". El primer bloque de años es "Both sexes".
  leer_stocks_undesa <- function(ruta, anios) {
    hojas <- excel_sheets(ruta)
    hoja  <- hojas[grepl("^Table ?1$", hojas)][1]
    if (is.na(hoja)) hoja <- hojas[1]
    crudo <- as.data.frame(read_excel(ruta, sheet = hoja, col_names = FALSE, .name_repair = "minimal"))
    fila_enc <- which(apply(crudo, 1, function(f)
      any(grepl("location code of destination", f, ignore.case = TRUE))))[1]
    if (is.na(fila_enc)) stop("No encontré la fila de encabezado en ", ruta)
    enc <- trimws(as.character(unlist(crudo[fila_enc, ])))
    col_dest <- grep("location code of destination", enc, ignore.case = TRUE)[1]
    col_orig <- grep("location code of origin",      enc, ignore.case = TRUE)[1]
    enc_num  <- suppressWarnings(as.numeric(sub("\\.0$", "", enc)))
    col_anio <- sapply(anios, function(a) which(enc_num == a)[1])
    if (anyNA(col_anio)) stop("Faltan columnas de años: ", paste(anios[is.na(col_anio)], collapse = ", "))
    datos <- crudo[(fila_enc + 1):nrow(crudo), c(col_dest, col_orig, col_anio)]
    names(datos) <- c("dest_code", "orig_code", paste0("y", anios))
    datos |>
      mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.x))))) |>
      filter(!is.na(dest_code), !is.na(orig_code)) |>
      pivot_longer(starts_with("y"), names_to = "year", values_to = "stock",
                   names_prefix = "y", names_transform = list(year = as.integer)) |>
      mutate(stock = coalesce(stock, 0))
  }

  wpp <- leer_wpp(ruta_wpp, anios)
  iso <- wpp |> distinct(code, alpha3)
  # Los stocks vienen con códigos M49; se pasan a ISO3 con la tabla de WPP.
  # Los agregados regionales (códigos >= 900) y el origen "Other" quedan afuera,
  # igual que en Abel & Cohen (stocks de origen desconocido).
  d0 <- leer_stocks_undesa(ruta_stock_onu, anios) |>
    inner_join(iso |> rename(dest_code = code, por = alpha3), by = "dest_code") |>
    inner_join(iso |> rename(orig_code = code, pob = alpha3), by = "orig_code") |>
    select(pob, por, year, stock)
  d2 <- wpp |> filter(year %in% anios) |> select(year, alpha3, pop)
  d1 <- demog_desde_wpp(wpp, anios)
  ruta_ref <- ruta_ref_actual

} else if (fuente == "paper_red") {
  for (f in c(ruta_mig_red, ruta_pob_red)) if (!file.exists(f)) stop("No se encuentra ", f)
  # Stocks: se descarta el origen "Otros" (ZZZ, código 2003), de origen
  # desconocido (7-9 % del stock), igual que hace Abel & Cohen.
  d0 <- read_csv(ruta_mig_red, show_col_types = FALSE) |>
    filter(iso3_orig != "ZZZ", cod_orig < 900, iso3_orig != iso3_des) |>
    transmute(pob = iso3_orig, por = iso3_des, year = as.integer(`año`), stock = migrantes)
  # Población del paper (personas) -> miles, como en Abel & Cohen
  d2 <- read_csv(ruta_pob_red, show_col_types = FALSE) |>
    transmute(year = as.integer(`año_pobla`), alpha3 = iso3_pobla, pop = poblacion / 1e3)
  anios <- sort(unique(d0$year))
  # Nacimientos y muertes: de WPP 2024
  d1 <- demog_desde_wpp(leer_wpp(ruta_wpp, anios), anios)
  ruta_ref <- ruta_ref_actual
}

periodos <- tibble(year0 = head(anios, -1), year1 = tail(anios, -1)) |>
  mutate(period = paste0(year0, "-", year1), years = year1 - year0)

# ------------------- 2. Conjunto común de países ----------------------------
# Como en estimation.R: países con población, nacimientos y muertes completos
# en todos los años/períodos, que figuran como origen y destino en los stocks.
completos <- function(df, vars, n_tiempos, col_t) {
  df |> filter(if_all(all_of(vars), ~ !is.na(.x))) |>
    group_by(alpha3) |> filter(n_distinct(.data[[col_t]]) == n_tiempos) |> pull(alpha3) |> unique()
}
cc <- Reduce(intersect, list(
  completos(d2 |> filter(year %in% anios), "pop", length(anios), "year"),
  completos(d1, c("b", "d"), nrow(periodos), "year0"),
  unique(d0$pob), unique(d0$por)))
if (fuente != "abel2019" && paises_como_abel) cc <- intersect(cc, paises_abel())
cc <- sort(cc)
message("Fuente: ", fuente, " | países con datos completos: ", length(cc),
        if (fuente == "abel2019") " (Abel & Cohen usan 200)" else "")
write_csv(tibble(alpha3 = cc), file.path(dir_out, "paises_usados.csv"))

# ------------------- 3. Nativos (diagonal) ----------------------------------
# Nativos = población total - nacidos en otro país del conjunto
nativos <- d0 |>
  filter(pob %in% cc, por %in% cc, pob != por, year %in% anios) |>
  group_by(alpha3 = por, year) |>
  summarise(fb = sum(stock), .groups = "drop") |>
  right_join(d2 |> filter(alpha3 %in% cc, year %in% anios), by = c("alpha3", "year")) |>
  mutate(fb = coalesce(fb, 0), nb = pop * 1e3 - fb)
if (any(nativos$nb < 0))
  warning("Países con más inmigrantes que población: ",
          paste(unique(nativos$alpha3[nativos$nb < 0]), collapse = ", "),
          ". migest los corrige a 0.")

armar_matriz <- function(a) {
  s <- d0 |> filter(year == a, pob %in% cc, por %in% cc, pob != por)
  m <- matrix(0, length(cc), length(cc), dimnames = list(birth = cc, dest = cc))
  m[cbind(match(s$pob, cc), match(s$por, cc))] <- s$stock
  nb <- nativos |> filter(year == a)
  diag(m) <- nb$nb[match(cc, nb$alpha3)]
  m
}
vector_demog <- function(var, t0) {
  v <- d1 |> filter(year0 == t0)
  setNames(v[[var]][match(cc, v$alpha3)] * 1e3, cc)
}

# ------------------- 4. Estimación ------------------------------------------
# Envoltorio de mipfp::Ipfp: si los márgenes difieren apenas por redondeo, se
# reescalan al mismo total (mipfp exige coincidencia con tolerancia absoluta de
# 0.001 personas). Con los datos de Abel & Cohen no actúa (discrepancia 0).
ipfp_original <- mipfp::Ipfp
registro_ipfp <- new.env(); registro_ipfp$max_rel <- 0
ipfp_tolerante <- function(seed, target.list, target.data, ...) {
  if (length(target.data) > 1) {
    tots <- vapply(target.data, function(x) sum(x), numeric(1))
    if (all(is.finite(tots)) && all(tots > 0)) {
      rel <- max(abs(tots - tots[1])) / tots[1]
      registro_ipfp$max_rel <- max(registro_ipfp$max_rel, rel)
      if (rel > 0) target.data <- lapply(target.data, function(x) x * tots[1] / sum(x))
    }
  }
  ipfp_original(seed = seed, target.list = target.list, target.data = target.data, ...)
}
utils::assignInNamespace("Ipfp", ipfp_tolerante, ns = "mipfp")

# Suma sobre país de nacimiento y pone la diagonal (no migrantes) en 0
a_od <- function(f) { if (length(dim(f)) == 3) f <- apply(f, c(1, 2), sum); diag(f) <- 0; f }
a_largo <- function(m, metodo, t0) {
  as.data.frame(as.table(m), stringsAsFactors = FALSE) |>
    setNames(c("orig", "dest", "flow")) |>
    mutate(method = metodo, year0 = t0, .before = 1)
}

resultados <- list(); diagnostico <- list()
for (i in seq_len(nrow(periodos))) {
  t0 <- periodos$year0[i]; t1 <- periodos$year1[i]
  message("Período ", t0, "-", t1, " ...")
  s1 <- armar_matriz(t0); s2 <- armar_matriz(t1)
  b  <- vector_demog("b", t0); d <- vector_demog("d", t0)

  # Diferencia de stocks (ffs_diff ignora la diagonal)
  e1 <- ffs_diff(stock_start = s1, stock_end = s2, decrease = "zero")
  e2 <- ffs_diff(stock_start = s1, stock_end = s2, decrease = "return")

  # Contabilidad demográfica (como en el paper, se redondea a enteros)
  registro_ipfp$max_rel <- 0
  e4 <- round(a_od(ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d,
                            match_birthplace_tot_method = "open-dr")))
  e5 <- round(a_od(ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)))
  e6_ind <- round(a_od(ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d,
                                stayer_assumption = FALSE)))
  diagnostico[[i]] <- tibble(period = periodos$period[i], discrepancia_rel_max = registro_ipfp$max_rel)
  # Pseudo-bayesiano (en estimation.R: e6 <- 0.87 * e5 + (1 - 0.87) * e6_ind)
  e6 <- round(w_pb * e5 + (1 - w_pb) * e6_ind)

  resultados[[i]] <- bind_rows(
    a_largo(a_od(e1), "sd_drop_neg", t0), a_largo(a_od(e2), "sd_rev_neg", t0),
    a_largo(e4, "da_min_open", t0), a_largo(e5, "da_min_closed", t0),
    a_largo(e6, "da_pb_closed", t0))
}
utils::assignInNamespace("Ipfp", ipfp_original, ns = "mipfp")   # se restaura

flujos <- bind_rows(resultados) |> left_join(periodos, by = "year0")
write_csv(flujos |> filter(flow > 0), file.path(dir_out, "flujos_long.csv"))
write_csv(bind_rows(diagnostico), file.path(dir_out, "diagnostico_ipfp.csv"))

# Totales mundiales. total_anual divide por la duración (2020-2024 dura 4 años).
pob_mundial <- d2 |> filter(alpha3 %in% cc) |> group_by(year0 = year) |>
  summarise(pop = sum(pop) * 1e3, .groups = "drop")
totales <- flujos |> group_by(method, period, year0, years) |>
  summarise(total = sum(flow), .groups = "drop") |>
  left_join(pob_mundial, by = "year0") |>
  mutate(total_anual = total / years, tasa_por_mil_anual = 1000 * total_anual / pop) |>
  select(-pop)
write_csv(totales, file.path(dir_out, "totales.csv"))

# ------------------- 5. Validación con la Tabla 7 (solo "abel2019") ----------
# La Tabla 7 resume los 200 x 200 x 5 = 200.000 valores de cada método
# (incluye la diagonal, que vale 0).
tabla7 <- NULL
if (fuente == "abel2019") {
  tabla7_publicada <- tribble(
    ~method,         ~media, ~maximo, ~desvio, ~prop_cero,
    "sd_drop_neg",      675, 2763183,   15322,      0.82,
    "sd_rev_neg",       883, 2763183,   17770,      0.79,
    "da_min_open",      853, 2865526,   17470,      0.72,
    "da_min_closed",    976, 2899536,   16926,      0.70,
    "da_pb_closed",    1928, 3918816,   25492,      0.53)
  tabla7 <- flujos |> group_by(method) |>
    summarise(N = n(), media = mean(flow), maximo = max(flow), desvio = sd(flow),
              prop_cero = mean(flow == 0), .groups = "drop") |>
    left_join(tabla7_publicada, by = "method", suffix = c("_replica", "_paper")) |>
    mutate(ratio_media = media_replica / media_paper,
           ratio_maximo = maximo_replica / maximo_paper)
  write_csv(tabla7, file.path(dir_out, "tabla7_vs_paper.csv"))
}

# ------------------- 6. Comparación con estimaciones de Abel & Cohen ---------
# "abel2019"            : contra la versión 1 (mismos datos): casi idéntica.
# "onu2024", "paper_red": contra la versión actual (IMS 2020 + WPP 2022): las
#            diferencias miden el efecto de la revisión de los datos.
# Para obtener abel_v1.csv: en https://doi.org/10.6084/m9.figshare.c.4470464
# abrir el ítem de estimaciones origen-destino, elegir "Version 1" (2019) y
# guardar el CSV como abel_v1.csv en dir_data_abel.
comp <- NULL
if (file.exists(ruta_ref)) {
  ab <- read_csv(ruta_ref, show_col_types = FALSE)
  names(ab)[names(ab) %in% c("pob", "Var1")] <- "orig"   # según la versión del archivo
  names(ab)[names(ab) %in% c("por", "Var2")] <- "dest"
  metodos <- intersect(unique(flujos$method), names(ab))
  ab <- ab |>
    filter(year0 %in% periodos$year0[periodos$years == 5],   # la referencia es quinquenal
           orig != dest, orig %in% cc, dest %in% cc) |>
    select(year0, orig, dest, all_of(metodos)) |>
    pivot_longer(all_of(metodos), names_to = "method", values_to = "flow_abel")
  per_comunes <- unique(ab$year0)

  par <- flujos |> filter(method %in% metodos, orig != dest, year0 %in% per_comunes) |>
    select(method, year0, period, orig, dest, flow) |>
    full_join(ab, by = c("method", "year0", "orig", "dest")) |>
    mutate(flow = coalesce(flow, 0), flow_abel = coalesce(flow_abel, 0),
           period = paste0(year0, "-", year0 + 5))

  # Corredor a corredor (si la réplica es exacta: ratio 1, correlaciones 1)
  comp <- par |> group_by(method, period) |>
    summarise(total_propio = sum(flow), total_abel = sum(flow_abel),
              ratio_totales = total_propio / total_abel,
              pearson = cor(flow, flow_abel),
              pearson_log1p = cor(log1p(flow), log1p(flow_abel)),
              spearman = cor(flow, flow_abel, method = "spearman"),
              prop_iguales = mean(abs(flow - flow_abel) <= 1),  # difieren en 1 persona o menos
              prop_factor2 = mean((flow >= flow_abel / 2 & flow <= 2 * flow_abel)[pmax(flow, flow_abel) >= 1000]),
              wape = sum(abs(flow - flow_abel)) / sum(flow_abel),
              .groups = "drop")
  write_csv(comp, file.path(dir_out, "vs_abel.csv"))

  # Por país: emigración, inmigración y saldo
  paises <- full_join(
    par |> group_by(method, period, pais = orig) |>
      summarise(emig = sum(flow), emig_abel = sum(flow_abel), .groups = "drop"),
    par |> group_by(method, period, pais = dest) |>
      summarise(inm = sum(flow), inm_abel = sum(flow_abel), .groups = "drop"),
    by = c("method", "period", "pais")) |>
    mutate(across(c(emig, emig_abel, inm, inm_abel), ~ coalesce(.x, 0)))
  write_csv(paises |> group_by(method, period) |>
              summarise(pearson_log1p_emig = cor(log1p(emig), log1p(emig_abel)),
                        pearson_log1p_inm  = cor(log1p(inm), log1p(inm_abel)),
                        pearson_neta = cor(inm - emig, inm_abel - emig_abel),
                        signo_neta_igual = mean(sign(inm - emig) == sign(inm_abel - emig_abel)),
                        mediana_ratio_emig = median((emig / emig_abel)[emig_abel > 0]),
                        mediana_ratio_inm  = median((inm / inm_abel)[inm_abel > 0]),
                        .groups = "drop"),
            file.path(dir_out, "vs_abel_paises.csv"))

  g <- par |> filter(flow > 0 | flow_abel > 0) |>
    ggplot(aes(flow_abel + 1, flow + 1)) +
    geom_point(alpha = 0.15, size = 0.5) +
    geom_abline(linetype = 2, colour = "grey40") +
    scale_x_log10() + scale_y_log10() + facet_grid(method ~ period) +
    labs(x = "Abel & Cohen (+1, escala log)", y = paste0("Estimación propia, datos ", fuente, " (+1, escala log)"),
         title = paste("Mismo procedimiento, datos", fuente, "vs estimaciones de Abel & Cohen")) +
    theme_minimal(base_size = 10)
  ggsave(file.path(dir_out, "fig_vs_abel.png"), g, width = 12, height = 10, dpi = 150)
} else {
  message("No está ", ruta_ref, ": se omite la comparación con Abel & Cohen.")
}

# ------------------- 7. Resumen en consola -----------------------------------
message("\nTotales mundiales (millones de migrantes por año):")
print(totales |> mutate(millones_anuales = round(total_anual / 1e6, 2)) |>
        select(method, period, millones_anuales) |>
        pivot_wider(names_from = method, values_from = millones_anuales), width = Inf)
if (!is.null(tabla7)) {
  message("\nTabla 7: réplica vs paper (ratio = réplica / paper; 1 = idéntico):")
  print(tabla7 |> select(method, media_replica, media_paper, ratio_media,
                         maximo_replica, maximo_paper, prop_cero_replica, prop_cero_paper),
        width = Inf)
}
if (!is.null(comp)) {
  message("\nComparación con Abel & Cohen (promedio de los períodos comunes):")
  print(comp |> group_by(method) |>
          summarise(across(c(ratio_totales, pearson_log1p, spearman, prop_factor2, wape), mean)),
        width = Inf)
}
message("\nListo. Resultados en ", normalizePath(dir_out))
