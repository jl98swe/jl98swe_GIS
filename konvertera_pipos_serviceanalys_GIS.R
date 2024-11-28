# ===========================================================
# Skript: Bearbeta Pipos-serviceanalysdata och skapa GeoJSON
# ===========================================================
# Indata:
# - Data hämtas från Pipos Serviceanalys i form av en xlsx-fil som innehåller 
#   information om olika serviceenheter, inklusive koordinater (X och Y), servicetyp,
#   serviceform, och tillhörande kommun och län.
# - Variabler inkluderar län för filtrering av data.
#
# Utdata:
# - En GeoJSON-fil som sparar den filtrerade och geografiskt transformerade datan. 
#   Den inkluderar servicetyp, serviceform, namn, adress, samt koordinater som omvandlas 
#   från SWEREF 99 TM (EPSG:3006) till WGS 84 (EPSG:4326).
# - Filen sparas i en definierad mapp och heter "Pipos_serviceanalys_GIS.geojson".
# ===========================================================


# Installera nödvändiga paket om de inte redan är installerade.
if (!require(readxl)) install.packages("readxl")       # Läs in Excel-filer.
if (!require(tidyverse)) install.packages("tidyverse") # Databearbetning.
if (!require(sf)) install.packages("sf")               # Bearbeta geografisk data.

# Inställningar.
utmapp <- "~/GIS/GIS-lager/Skolor/" # Välj mapp att spara filerna i.
spara_GIS <- TRUE                   # Om GeoJSON-fil ska sparas.
lan <- c("Norrbottens län")         # Vektor med län som ska sparas.

# Läs in datan från Pipos.
pipos <- read_excel("~/GIS/Pipos_serviceanalys_export.xlsx")

# Filtrera datan efter inställningarna.
pipos <- pipos %>%
  filter(Län %in% lan)

# Gör om X och Y till numeriska variabler.
pipos <- pipos %>%
  mutate(X = as.numeric(X), Y = as.numeric(Y))

# Ta bort alla observationer som saknar data.
pipos <- pipos %>%
  filter(complete.cases(.))

# Skapa ett spatialt objekt med X och Y som koordinater i SWEREF 99 TM (EPSG:3006).
pipos_sf <- st_as_sf(pipos, coords = c("X", "Y"), crs = 3006, agr = "constant")

# Konvertera koordinaterna till WGS 84 (EPSG:4326)
pipos_sf_wgs84 <- st_transform(pipos_sf, crs = 4326)

# Spara GeoJSON.
if(spara_GIS) {
  st_write(pipos_sf_wgs84,
           dsn = paste0(utmapp, "Pipos_serviceanalys_GIS.geojson"),
           delete_dsn = TRUE)
}
