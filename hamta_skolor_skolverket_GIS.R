# ===========================================================
# Skript: Hämta och bearbeta skolenhetsdata
# ===========================================================
# Indata:
# - Data hämtas från Skolverkets öppna API för att hämta skolenheter för kommuner.
#   Det hämtas även detaljerad information om varje skola såsom namn, adress, skolform, koordinater och kommunnamn.
# - Variabler inkluderar kommunkoder för Västernorrland, Jämtland, Västerbotten och Norrbotten.
#
# Utdata:
# - En GeoJSON-fil som sparar den insamlade datan, inklusive skolans namn, adress, 
#   skolform och kommuninformation. Den geografiska informationen kommer från WGS84-koordinaterna
#   och det valda koordinatreferenssystemet (crs) är 4326.
# - En CSV-fil som sparar den insamlade datan, inklusive skolans namn, adress, koordinater, 
#   skolform och kommuninformation.
# ===========================================================

# Inställningar.
utmapp <- "~/GIS/GIS-lager/Skolor/" # Välj mapp att spara filerna i.
spara_GIS <- TRUE                   # Om GeoJSON-fil ska sparas.
spara_CSV <- TRUE                   # Om CSV-fil (semikolonseparerad) ska sparas.

# Installera nödvändiga paket om de inte redan är installerade.
if (!require(httr)) install.packages("httr")           # För HTTP-förfrågningar.
if (!require(jsonlite)) install.packages("jsonlite")   # Hantera JSON-data.
if (!require(tidyverse)) install.packages("tidyverse") # Databearbetning.
if (!require(sf)) install.packages("sf")               # Bearbeta geografisk data.

# Kommun koder för länen.
kommunkoder <- c(
  "2260",
  "2262",
  "2280",
  "2281",
  "2282",
  "2283",
  "2284",
  "2303",
  "2305",
  "2309",
  "2313",
  "2321",
  "2326",
  "2361",
  "2380",
  "2401",
  "2403",
  "2404",
  "2409",
  "2417",
  "2418",
  "2421",
  "2422",
  "2425",
  "2460",
  "2462",
  "2463",
  "2480",
  "2481",
  "2482",
  "2505", "2506", "2510", "2513", "2514",
  "2518", "2521", "2523", "2560", "2580",
  "2581", "2582", "2583", "2584"
)

# Bas-URL för Skolenhetsregistret API-endpoint.
url_skolenheter <- "https://api.skolverket.se/skolenhetsregistret/v1/kommun/"

# Initialisera en tom lista för att lagra resultat.
skolor_detalj <- list()

# Loopa igenom varje kommunkod för att hämta data.
for (kommunKod in kommunkoder) {
  # Bygg query-URL med kommunkod.
  queryUrl <- paste0(url_skolenheter, kommunKod)
  
  # Skicka GET-förfrågan till API:t.
  response <- GET(queryUrl)
  
  # Kontrollera om förfrågan var framgångsrik.
  if (status_code(response) == 200) {
    # Bearbeta JSON-svaret.
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"),
                     flatten = TRUE)
    
    # Extrahera Skolenhetskod för varje skola.
    skolenhetskoder <- data$Skolenheter$Skolenhetskod
    
    # För varje Skolenhetskod, hämta detaljerad information.
    for (skolenhetskod in skolenhetskoder) {
      skola_url <- paste0("https://api.skolverket.se/skolenhetsregistret/v1/skolenhet/", skolenhetskod)
      skola_response <- GET(skola_url)
      
      if (status_code(skola_response) == 200) {
        # Bearbeta detaljerad skoldata.
        skola_data <- fromJSON(content(skola_response, as = "text", encoding = "UTF-8"),
                               flatten = TRUE)
        
        # Extrahera relevant information för varje skola.
        skola_info <- skola_data$SkolenhetInfo
        
        # Spara relevant data.
        skolor_detalj[[length(skolor_detalj) + 1]] <- list(
          Skolenhetskod = skola_info$Skolenhetskod,
          Namn = skola_info$Namn,
          Adress = skola_info$Besoksadress$Adress,
          Postnr = skola_info$Besoksadress$Postnr,
          Ort = skola_info$Besoksadress$Ort,
          Skolform = skola_info$Skolformer$Benamning,
          Status = skola_info$Status,
          Kommunkod = skola_info$Kommun$Kommunkod,
          Kommun = skola_info$Kommun$Namn,
          Koordinat_SweRef_E = skola_info$Besoksadress$GeoData$Koordinat_SweRef_E,
          Koordinat_SweRef_N = skola_info$Besoksadress$GeoData$Koordinat_SweRef_N,
          Koordinat_WGS84_Lat = skola_info$Besoksadress$GeoData$Koordinat_WGS84_Lat,
          Koordinat_WGS84_Lng = skola_info$Besoksadress$GeoData$Koordinat_WGS84_Lng
        )
      } else {
        warning(paste("Misslyckades att hämta detaljerad information för Skolenhetskod:", skolenhetskod))
      }
    }
  } else {
    warning(paste("Misslyckades att hämta skolenhetsdata för kommunkod:", kommunKod))
  }
}

# Omvandla listan till en data frame.
skolor_detalj_df <- bind_rows(skolor_detalj)

# Skapa ett sf-objekt från data framen.
skolor_detalj_sf <- skolor_detalj_df %>%
  filter(complete.cases(Koordinat_WGS84_Lng, Koordinat_WGS84_Lat),                          # Rensa bort rader som saknar koordinater.
         Status == "Aktiv",                                                                 # Endast aktiva skolor.
         Skolform != c("Central insamlingsenhet", "Fritidshem", "Öppen fritidsverksamhet"), # Exkludera dessa skolformer.
         !is.na(Skolform)                                                                   # Det förekom endast NA's för Status == "Vilande" men för säkerhets skull tas dom bort.
         ) %>%
  select(-c(Koordinat_SweRef_E, Koordinat_SweRef_N)) %>%                                    # Ta bort överflödiga kolumner.
  st_as_sf(coords = c("Koordinat_WGS84_Lng", "Koordinat_WGS84_Lat"),                        # Konvertera WGS84-koordinaterna till ett rumsligt objekt.
           crs = 4326,
           remove = T)

# Exportera sf-objektet till GeoJSON.
if (spara_GIS) {
  st_write(skolor_detalj_sf,
           dsn = paste0(utmapp, "Skolor-GIS_F-9_gymnasie_VUX-alla_spec_anpassad.geojson"),
           driver = "GeoJSON",
           delete_dsn = TRUE) # Tillåt filen att skrivas över.
}

# Spara rådatan till en CSV-fil.
if (spara_CSV) {
  write.csv2(skolor_detalj_df,
            file = paste0(utmapp, "Skolor-GIS_rådata.csv"),
            row.names = FALSE)
}
