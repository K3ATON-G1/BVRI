# ============================================================
# BALTIMORE VACANTS REINVESTMENT INITIATIVE
# ============================================================

# Install once if needed:
# install.packages(c(
#   "shiny", "leaflet", "httr", "jsonlite",
#   "sf", "htmltools"
# ))

library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(htmltools)


# ============================================================
# FILES AND SERVICES
# ============================================================

PROPERTY_FILE <- paste0(
  "Master_Spreadsheet_SPP_Matched",
  "(SPP Master List (homes)).csv"
)

BVRI_FILE <- "L15_BVRI_LISTEDAREAS.zip"

# Optional developer-contact CSV.
# Leave blank until you have exported that sheet.
DEVELOPER_FILE <- ""

CITY_PROPERTY_URL <- paste0(
  "https://baltegis.baltimorecity.gov/mapping/rest/services/",
  "CityView/RealProperty_OB/FeatureServer/0"
)

CITY_BOUNDARY_URL <- paste0(
  "https://gis.baltimorecity.gov/egis/rest/services/",
  "Misc/Boundaries/FeatureServer/1"
)

# Esri grayscale basemap.
BASEMAP_PROVIDER <- providers$Esri.WorldGrayCanvas

BLACK <- "#111111"
GOLD <- "#F2C400"

PUBLIC_STATUSES <- c(
  "Available now",
  "Coming soon",
  "In construction",
  "Future homes"
)

STATUS_COLORS <- c(
  "Available now" = "#16734A",
  "Coming soon" = "#B56A0A",
  "In construction" = "#2563A5",
  "Future homes" = "#756199",
  "Under contract" = "#667085",
  "Sold" = "#667085",
  "Ineligible" = "#B42318",
  "Awaiting application" = "#667085",
  "Approval unknown" = "#667085"
)


# ============================================================
# BASIC HELPERS
# ============================================================

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

escape_html <- function(x) {
  as.character(htmlEscape(clean_text(x)))
}

name_key <- function(x) {
  gsub("[^A-Z0-9]", "", toupper(clean_text(x)))
}

address_key <- function(x) {
  x <- toupper(clean_text(x))
  x <- sub(",? +BALTIMORE.*$", "", x)
  
  replacements <- c(
    STREET = "ST",
    AVENUE = "AVE",
    ROAD = "RD",
    BOULEVARD = "BLVD",
    DRIVE = "DR",
    PLACE = "PL",
    COURT = "CT",
    LANE = "LN",
    NORTH = "N",
    SOUTH = "S",
    EAST = "E",
    WEST = "W"
  )
  
  for (word in names(replacements)) {
    x <- gsub(
      paste0("\\b", word, "\\b"),
      replacements[[word]],
      x
    )
  }
  
  name_key(x)
}

resolve_file <- function(filename, required = TRUE) {
  if (!nzchar(filename)) {
    if (required) stop("A required filename is blank.")
    return("")
  }
  
  candidates <- c(
    filename,
    file.path("data", filename),
    file.path("..", filename)
  )
  
  found <- candidates[file.exists(candidates)]
  
  if (!length(found)) {
    if (required) {
      stop(paste(
        "File not found:",
        filename,
        "\nPlace it beside app.R or check the filename."
      ))
    }
    
    return("")
  }
  
  normalizePath(found[1], winslash = "/", mustWork = TRUE)
}

get_column <- function(data, possible_names) {
  matches <- match(
    name_key(possible_names),
    name_key(names(data))
  )
  
  matches <- matches[!is.na(matches)]
  
  if (!length(matches)) {
    return(rep("", nrow(data)))
  }
  
  clean_text(data[[matches[1]]])
}

read_csv_headers <- function(path, required_headers) {
  preview <- read.csv(
    path,
    header = FALSE,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    blank.lines.skip = FALSE,
    na.strings = "",
    nrows = 20,
    fileEncoding = "UTF-8-BOM"
  )
  
  header_row <- which(vapply(
    seq_len(nrow(preview)),
    function(i) {
      all(
        name_key(required_headers) %in%
          name_key(unlist(preview[i, ]))
      )
    },
    logical(1)
  ))[1]
  
  if (is.na(header_row)) {
    stop(paste(
      "Required column headers were not found in:",
      basename(path)
    ))
  }
  
  read.csv(
    path,
    skip = header_row - 1,
    header = TRUE,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    na.strings = "",
    fileEncoding = "UTF-8-BOM"
  )
}

status_group <- function(status) {
  status <- tolower(clean_text(status))
  result <- rep("Approval unknown", length(status))
  
  result[grepl("awaiting", status)] <-
    "Awaiting application"
  
  result[grepl("declined|ineligible", status)] <-
    "Ineligible"
  
  result[
    grepl("^approv.*pipeline|^approv.*pre-development", status)
  ] <- "Future homes"
  
  result[
    grepl("^approv.*construction", status)
  ] <- "In construction"
  
  result[
    grepl("^approv.*coming|^approv.*comming", status)
  ] <- "Coming soon"
  
  result[
    grepl("^approv.*on market", status)
  ] <- "Available now"
  
  result[grepl("^pre.?sale", status)] <-
    "Under contract"
  
  result[grepl("^sold", status)] <-
    "Sold"
  
  result
}

property_status_message <- function(group) {
  switch(
    group,
    "Ineligible" =
      "This property is recorded as ineligible for SPP.",
    
    "Awaiting application" =
      "An application is awaited. SPP approval is not confirmed.",
    
    "Approval unknown" =
      "SPP approval has not been confirmed.",
    
    "Sold" =
      "This property has been sold and is no longer available.",
    
    "Under contract" =
      "This property is recorded as presold to an SPP buyer.",
    
    paste(
      "This property is recorded as SPP approved.",
      "Current stage:",
      group
    )
  )
}


# ============================================================
# LOAD BVRI SHAPEFILE
# ============================================================

PROPERTY_PATH <- resolve_file(PROPERTY_FILE)
BVRI_PATH <- resolve_file(BVRI_FILE)

load_bvri <- function(zip_path) {
  contents <- unzip(zip_path, list = TRUE)$Name
  
  unsafe <- grepl(
    "(^/|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))",
    contents
  )
  
  if (any(unsafe)) {
    stop("The boundary ZIP contains an unsafe file path.")
  }
  
  folder <- tempfile("bvri_boundary_")
  dir.create(folder)
  
  unzip(zip_path, exdir = folder)
  
  shapefiles <- list.files(
    folder,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(shapefiles) != 1) {
    stop("The ZIP must contain exactly one shapefile.")
  }
  
  areas <- st_read(shapefiles[1], quiet = TRUE)
  
  if (is.na(st_crs(areas))) {
    stop("The boundary shapefile needs its .prj file.")
  }
  
  st_transform(st_make_valid(areas), 4326)
}

bvri_areas <- load_bvri(BVRI_PATH)


# ============================================================
# LOAD BALTIMORE CITY BOUNDARY
# ============================================================

read_geojson_service <- function(url) {
  response <- GET(
    paste0(url, "/query"),
    query = list(
      where = "1=1",
      outFields = "*",
      returnGeometry = "true",
      outSR = 4326,
      f = "geojson"
    ),
    timeout(30)
  )
  
  stop_for_status(response)
  
  text <- content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  parsed <- fromJSON(text, simplifyVector = FALSE)
  
  if (!is.null(parsed$error)) {
    stop("The geographic service returned an error.")
  }
  
  if (isTRUE(parsed$exceededTransferLimit)) {
    stop("The geographic service returned incomplete data.")
  }
  
  result <- st_read(text, quiet = TRUE)
  
  if (!nrow(result)) {
    stop("The geographic service returned no boundary.")
  }
  
  st_transform(st_make_valid(result), 4326)
}

# You can optionally save a city-boundary GeoJSON beside app.R
# with this filename to avoid downloading it at startup.
local_city_boundary <- resolve_file(
  "Baltimore_City_Boundary.geojson",
  required = FALSE
)

city_boundary <- if (nzchar(local_city_boundary)) {
  st_transform(
    st_make_valid(
      st_read(local_city_boundary, quiet = TRUE)
    ),
    4326
  )
} else {
  read_geojson_service(CITY_BOUNDARY_URL)
}


# ============================================================
# CREATE MAP SHADING
# ============================================================

# Perform polygon operations in a projected coordinate system.
city_projected <- st_transform(city_boundary, 3857)
bvri_projected <- st_transform(bvri_areas, 3857)

city_geometry <- st_union(
  st_make_valid(st_geometry(city_projected))
)

bvri_geometry <- st_union(
  st_make_valid(st_geometry(bvri_projected))
)

# BVRI outlines are restricted to Baltimore City.
bvri_clipped <- suppressWarnings(
  st_intersection(bvri_geometry, city_geometry)
)

# Light red over city areas outside BVRI.
city_outside_bvri <- st_difference(
  city_geometry,
  bvri_geometry
)

# Dark shading surrounding Baltimore.
# This extends well beyond the map's allowed viewing bounds.
outer_rectangle <- st_as_sfc(
  st_bbox(
    c(
      xmin = -78.5,
      ymin = 37.5,
      xmax = -74.5,
      ymax = 41.5
    ),
    crs = st_crs(4326)
  )
)

outer_rectangle <- st_transform(outer_rectangle, 3857)

outside_city <- st_difference(
  outer_rectangle,
  city_geometry
)

outside_city <- st_transform(outside_city, 4326)
city_outside_bvri <- st_transform(city_outside_bvri, 4326)
bvri_clipped <- st_transform(bvri_clipped, 4326)


# ============================================================
# BVRI GEOGRAPHIC CHECK
# ============================================================

in_bvri_area <- function(longitude, latitude) {
  if (!all(is.finite(c(longitude, latitude)))) {
    return(NA)
  }
  
  point <- st_sfc(
    st_point(c(longitude, latitude)),
    crs = 4326
  )
  
  any(lengths(
    st_intersects(point, bvri_areas)
  ) > 0)
}

bvri_message <- function(inside) {
  if (is.na(inside)) {
    return("BVRI geography could not be checked.")
  }
  
  if (inside) {
    return(paste(
      "This property is in a BVRI area and may qualify.",
      "Developer approval and HNI program requirements",
      "still need confirmation."
    ))
  }
  
  paste(
    "This property is outside a BVRI area.",
    "It does not meet the BVRI geographic requirement."
  )
}


# ============================================================
# CITY REAL PROPERTY SEARCH
# ============================================================

city_query <- function(where) {
  response <- GET(
    paste0(CITY_PROPERTY_URL, "/query"),
    query = list(
      where = where,
      outFields = "BLOCKLOT,FULLADDR,ZIP_CODE,NEIGHBOR",
      returnGeometry = "true",
      outSR = 4326,
      f = "geojson",
      resultRecordCount = 2000
    ),
    timeout(30)
  )
  
  stop_for_status(response)
  
  result <- fromJSON(
    content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  
  if (!is.null(result$error)) {
    stop("The city property service returned an error.")
  }
  
  if (isTRUE(result$exceededTransferLimit)) {
    stop("The city search returned incomplete results.")
  }
  
  result$features
}

parcel_point <- function(feature) {
  if (is.null(feature$geometry)) {
    return(c(NA_real_, NA_real_))
  }
  
  geojson <- toJSON(
    list(
      type = "FeatureCollection",
      features = list(feature)
    ),
    auto_unbox = TRUE,
    null = "null"
  )
  
  parcel <- st_read(geojson, quiet = TRUE)
  
  parcel <- st_transform(
    st_make_valid(parcel),
    26918
  )
  
  point <- suppressWarnings(
    st_point_on_surface(st_geometry(parcel))
  )
  
  point <- st_transform(point, 4326)
  
  as.numeric(st_coordinates(point)[1, 1:2])
}

location_cache <- new.env(parent = emptyenv())

load_locations <- function(properties) {
  blocklots <- unique(properties$blocklot)
  blocklots <- blocklots[nzchar(blocklots)]
  
  missing <- blocklots[
    !vapply(
      blocklots,
      exists,
      logical(1),
      envir = location_cache,
      inherits = FALSE
    )
  ]
  
  if (length(missing)) {
    batches <- split(
      missing,
      ceiling(seq_along(missing) / 50)
    )
    
    for (batch in batches) {
      quoted <- paste0(
        "'",
        gsub("'", "''", batch, fixed = TRUE),
        "'"
      )
      
      where <- paste0(
        "BLOCKLOT IN (",
        paste(quoted, collapse = ","),
        ")"
      )
      
      features <- tryCatch(
        city_query(where),
        error = function(e) {
          warning(
            "Some city parcel locations could not be loaded.",
            call. = FALSE
          )
          list()
        }
      )
      
      returned_ids <- vapply(
        features,
        function(feature) {
          clean_text(feature$properties$BLOCKLOT)
        },
        character(1)
      )
      
      for (blocklot in unique(returned_ids)) {
        matches <- which(returned_ids == blocklot)
        
        if (length(matches) != 1) next
        
        feature <- features[[matches]]
        
        coordinates <- tryCatch(
          parcel_point(feature),
          error = function(e) c(NA_real_, NA_real_)
        )
        
        if (all(is.finite(coordinates))) {
          assign(
            blocklot,
            list(
              longitude = coordinates[1],
              latitude = coordinates[2],
              city_address = clean_text(
                feature$properties$FULLADDR
              )
            ),
            envir = location_cache
          )
        }
      }
    }
  }
  
  properties$longitude <- NA_real_
  properties$latitude <- NA_real_
  
  for (i in seq_len(nrow(properties))) {
    blocklot <- properties$blocklot[i]
    
    if (
      nzchar(blocklot) &&
      exists(
        blocklot,
        envir = location_cache,
        inherits = FALSE
      )
    ) {
      location <- get(
        blocklot,
        envir = location_cache,
        inherits = FALSE
      )
      
      properties$longitude[i] <- location$longitude
      properties$latitude[i] <- location$latitude
      properties$city_address[i] <- location$city_address
    }
  }
  
  properties
}


# ============================================================
# READ PROPERTY CSV AND OPTIONAL CONTACT CSV
# ============================================================

load_data <- function() {
  homes <- read_csv_headers(
    PROPERTY_PATH,
    c("Organization", "Address", "Status")
  )
  
  properties <- data.frame(
    organization = get_column(
      homes,
      "Organization"
    ),
    
    neighborhood = get_column(
      homes,
      c("Neighborhood", "Heighborhood")
    ),
    
    address = get_column(
      homes,
      "Address"
    ),
    
    status = get_column(
      homes,
      c("Status", "Statues")
    ),
    
    approved_date = get_column(
      homes,
      c("Approved Date", "Approval Date")
    ),
    
    blocklot = get_column(
      homes,
      c("Real Property BLOCKLOT", "BLOCKLOT")
    ),
    
    city_address = get_column(
      homes,
      c("Real Property Address", "City Address")
    ),
    
    mls = get_column(
      homes,
      c(
        "MLS Number",
        "MLS",
        "MLS #",
        "Bright MLS",
        "Listing Number"
      )
    ),
    
    link = get_column(
      homes,
      c("Listing URL", "Listing Link", "Link")
    ),
    
    stringsAsFactors = FALSE
  )
  
  properties <- properties[
    nzchar(properties$address),
    ,
    drop = FALSE
  ]
  
  # Handle MLS identifiers stored in the Link column.
  contains_id <- nzchar(properties$link) &
    !grepl(
      "^https?://",
      properties$link,
      ignore.case = TRUE
    )
  
  use_as_mls <- contains_id & !nzchar(properties$mls)
  
  properties$mls[use_as_mls] <-
    properties$link[use_as_mls]
  
  properties$link[contains_id] <- ""
  
  properties$group <- status_group(properties$status)
  properties$id <- as.character(seq_len(nrow(properties)))
  
  properties <- load_locations(properties)
  
  developers <- data.frame(
    organization = character(),
    name = character(),
    title = character(),
    email = character(),
    phone = character(),
    stringsAsFactors = FALSE
  )
  
  developer_path <- resolve_file(
    DEVELOPER_FILE,
    required = FALSE
  )
  
  if (nzchar(developer_path)) {
    contacts <- read_csv_headers(
      developer_path,
      c("Name", "Email Address")
    )
    
    developers <- data.frame(
      organization = get_column(
        contacts,
        c(
          "BVRI Awardee",
          "BVRI AWARDEE (DEVELOPER)",
          "Organization"
        )
      ),
      name = get_column(contacts, "Name"),
      title = get_column(contacts, "Title"),
      email = get_column(contacts, "Email Address"),
      phone = get_column(contacts, "Phone Number"),
      stringsAsFactors = FALSE
    )
    
    developers <- developers[
      nzchar(developers$organization) &
        nzchar(developers$name),
      ,
      drop = FALSE
    ]
  }
  
  list(
    properties = properties,
    developers = developers,
    updated = format(
      file.info(PROPERTY_PATH)$mtime,
      "%m/%d/%Y %I:%M %p"
    )
  )
}

initial_data <- load_data()


# ============================================================
# PROPERTY POPUPS
# ============================================================

property_popup <- function(property) {
  listing_link <- ""
  
  if (
    grepl(
      "^https?://",
      property$link,
      ignore.case = TRUE
    )
  ) {
    listing_link <- paste0(
      "<p><a href='",
      escape_html(property$link),
      "' target='_blank' rel='noopener noreferrer'>",
      "View listing / developer website",
      "</a></p>"
    )
  }
  
  mls <- if (nzchar(property$mls)) {
    paste0(
      "<p>Bright MLS: ",
      escape_html(property$mls),
      "</p>"
    )
  } else {
    ""
  }
  
  paste0(
    "<div class='property-popup'>",
    
    "<strong>",
    escape_html(property$address),
    "</strong>",
    
    "<p>",
    escape_html(property$neighborhood),
    "</p>",
    
    "<p>",
    escape_html(property$group),
    "</p>",
    
    "<p>Developer: ",
    escape_html(property$organization),
    "</p>",
    
    mls,
    listing_link,
    
    "<button type='button' class='popup-info-button'",
    " data-property-id='",
    escape_html(property$id),
    "'>More information</button>",
    
    "</div>"
  )
}


# ============================================================
# INTERFACE
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$style(HTML("
      :root {
        --black: #111111;
        --gold: #F2C400;
        --muted: #647581;
      }

      body {
        background: #F5F5F2;
        color: var(--black);
        font-family: Arial, sans-serif;
        margin: 0;
      }

      .container-fluid {
        padding: 0;
      }

      .page-header {
        width: 100%;
        background: var(--black);
        border-bottom: 6px solid var(--gold);
        text-align: center;
        padding: 30px 24px;
        margin: 0;
      }

      .page-header h1 {
        color: var(--gold);
        font-size: clamp(25px, 3vw, 42px);
        font-weight: 800;
        line-height: 1.2;
        margin: 0 0 12px;
      }

      .page-header p {
        color: white;
        font-size: 16px;
        margin: 0;
      }

      .main-layout {
        display: grid;
        grid-template-columns: 340px minmax(0, 1fr);
        gap: 24px;
        padding: 24px;
        align-items: start;
      }

      .sidebar,
      .main-content {
        min-width: 0;
      }

      .panel-box {
        background: white;
        border: 1px solid #D9DEDA;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 18px;
      }

      .panel-box h3 {
        margin-top: 0;
        font-weight: 700;
      }

      .map-sticky {
        position: sticky;
        top: 20px;
        align-self: start;
      }

      #map {
        height: calc(100vh - 100px) !important;
        min-height: 450px;
        max-height: 850px;
        border: 2px solid var(--black);
        border-radius: 12px;
      }

      .map-caption {
        color: var(--muted);
        font-size: 12px;
        padding: 10px 2px;
      }

      .property-card {
        display: block;
        width: 100%;
        background: white;
        color: var(--black);
        text-align: left;
        padding: 15px;
        border: 1px solid #D9DEDA;
        border-radius: 8px;
        margin-bottom: 10px;
        cursor: pointer;
      }

      .property-card:hover,
      .property-card:focus {
        background: #FFF9DB;
        border-color: var(--black);
      }

      .property-card strong {
        display: block;
        font-size: 16px;
        margin: 7px 0;
      }

      .property-card span {
        display: block;
      }

      .status-label {
        font-size: 11px;
        text-transform: uppercase;
        font-weight: 700;
        letter-spacing: 0.6px;
      }

      .muted {
        color: var(--muted);
        font-size: 12px;
      }

      .search-result {
        background: #FFF9DB;
        border-left: 4px solid var(--gold);
        padding: 12px;
        border-radius: 6px;
        margin: 12px 0;
      }

      .btn-primary,
      .listing-link,
      .popup-info-button,
      .back-button {
        background: var(--gold);
        border: 1px solid var(--gold);
        color: var(--black);
        font-weight: 700;
        border-radius: 6px;
      }

      .btn-primary:hover,
      .btn-primary:focus,
      .listing-link:hover,
      .popup-info-button:hover,
      .back-button:hover {
        background: #D9AF00;
        border-color: #D9AF00;
        color: var(--black);
      }

      .popup-info-button {
        display: block;
        width: 100%;
        padding: 9px 12px;
        margin-top: 12px;
        cursor: pointer;
      }

      .listing-link {
        display: inline-block;
        padding: 10px 14px;
        margin: 10px 0;
        text-decoration: none;
      }

      .details-heading {
        border-bottom: 3px solid var(--gold);
        padding-bottom: 15px;
        margin-bottom: 20px;
      }

      .contact {
        border-top: 1px solid #E6ECE8;
        padding: 12px 0;
      }

      .contact p {
        margin: 5px 0;
      }

      .detail-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 18px;
        margin: 20px 0;
      }

      .detail-label {
        font-size: 12px;
        text-transform: uppercase;
        font-weight: 700;
        color: var(--muted);
      }

      a {
        color: var(--black);
      }

      .property-popup {
        min-width: 210px;
      }

      .property-popup p {
        margin: 8px 0;
      }

      .page-footer {
        text-align: center;
        padding: 15px 24px 25px;
        color: var(--muted);
        font-size: 12px;
      }

      @media (max-width: 900px) {
        .main-layout {
          grid-template-columns: 290px minmax(0, 1fr);
          gap: 15px;
          padding: 15px;
        }
      }

      @media (max-width: 700px) {
        .main-layout {
          display: flex;
          flex-direction: column;
        }

        .sidebar,
        .main-content {
          width: 100%;
        }

        .map-sticky {
          position: static;
        }

        #map {
          height: 65vh !important;
          min-height: 400px;
        }

        .detail-grid {
          grid-template-columns: 1fr;
        }
      }
    ")),
    
    tags$script(HTML("
      $(document).on('click', '.property-card', function() {
        Shiny.setInputValue(
          'selected_property',
          $(this).attr('data-id'),
          {priority: 'event'}
        );
      });

      $(document).on('click', '.popup-info-button', function() {
        Shiny.setInputValue(
          'more_information',
          $(this).attr('data-property-id'),
          {priority: 'event'}
        );
      });

      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('resize-map', function(message) {
          setTimeout(function() {
            var widget = HTMLWidgets.find('#map');

            if (widget) {
              widget.getMap().invalidateSize();
            }
          }, 200);
        });
      });
    "))
  ),
  
  div(
    class = "page-header",
    
    h1("Baltimore Vacants Reinvestment Initiative"),
    
    p(
      "Healthy Neighborhoods · Special Purchase Program Property Finder"
    )
  ),
  
  div(
    class = "main-layout",
    
    tags$aside(
      class = "sidebar",
      
      div(
        class = "panel-box",
        
        h3("Search a property"),
        
        textInput(
          "address",
          "Baltimore City address",
          placeholder = "Example: 1554 Carswell St"
        ),
        
        actionButton(
          "search",
          "Search property",
          class = "btn-primary"
        ),
        
        uiOutput("search_result"),
        uiOutput("parcel_choices"),
        
        tags$hr(),
        
        checkboxGroupInput(
          "statuses",
          "Show homes",
          choices = PUBLIC_STATUSES,
          selected = "Available now"
        ),
        
        selectInput(
          "neighborhood",
          "Neighborhood",
          choices = c(
            "All neighborhoods",
            sort(unique(
              initial_data$properties$neighborhood[
                nzchar(initial_data$properties$neighborhood)
              ]
            ))
          )
        ),
        
        selectInput(
          "developer",
          "Developer",
          choices = c(
            "All developers",
            sort(unique(
              initial_data$properties$organization[
                nzchar(initial_data$properties$organization)
              ]
            ))
          )
        )
      ),
      
      div(
        class = "panel-box",
        
        h3(textOutput("property_count")),
        
        uiOutput("property_list"),
        
        tags$hr(),
        
        p(class = "muted", textOutput("updated")),
        
        p(
          class = "muted",
          "Contact the developer to confirm current availability."
        )
      )
    ),
    
    tags$main(
      class = "main-content map-sticky",
      
      conditionalPanel(
        condition = "output.view_mode !== 'details'",
        
        leafletOutput("map", height = "650px"),
        
        div(
          class = "map-caption",
          paste(
            "Hover over a pin for a summary.",
            "Click its More information button for property details."
          )
        )
      ),
      
      conditionalPanel(
        condition = "output.view_mode === 'details'",
        
        div(
          class = "panel-box",
          
          actionButton(
            "back_to_map",
            "← Back to map",
            class = "back-button"
          ),
          
          tags$hr(),
          
          uiOutput("property_details")
        )
      )
    )
  ),
  
  div(
    class = "page-footer",
    paste(
      "Public demonstration.",
      "Property approval does not establish buyer",
      "or financing eligibility."
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  search_message <- reactiveVal(NULL)
  parcel_candidates <- reactiveVal(NULL)
  view_mode <- reactiveVal("map")
  
  output$view_mode <- renderText(view_mode())
  
  outputOptions(
    output,
    "view_mode",
    suspendWhenHidden = FALSE
  )
  
  filtered_properties <- reactive({
    properties <- initial_data$properties
    
    properties <- properties[
      properties$group %in% input$statuses,
      ,
      drop = FALSE
    ]
    
    if (
      !is.null(input$neighborhood) &&
      input$neighborhood != "All neighborhoods"
    ) {
      properties <- properties[
        properties$neighborhood == input$neighborhood,
        ,
        drop = FALSE
      ]
    }
    
    if (
      !is.null(input$developer) &&
      input$developer != "All developers"
    ) {
      properties <- properties[
        properties$organization == input$developer,
        ,
        drop = FALSE
      ]
    }
    
    properties[
      order(properties$address),
      ,
      drop = FALSE
    ]
  })
  
  output$map <- renderLeaflet({
    map <- leaflet(
      options = leafletOptions(
        minZoom = 10,
        maxZoom = 20
      )
    ) |>
      addProviderTiles(
        BASEMAP_PROVIDER,
        options = providerTileOptions(
          maxNativeZoom = 16,
          maxZoom = 20
        )
      ) |>
      
      # Black transparent shading outside Baltimore.
      addPolygons(
        data = outside_city,
        group = "Outside Baltimore",
        stroke = FALSE,
        fillColor = BLACK,
        fillOpacity = 0.55,
        options = pathOptions(
          interactive = FALSE
        )
      ) |>
      
      # Light red inside Baltimore but outside BVRI.
      addPolygons(
        data = city_outside_bvri,
        group = "Outside BVRI",
        stroke = FALSE,
        fillColor = "#D64242",
        fillOpacity = 0.15,
        options = pathOptions(
          interactive = FALSE
        )
      ) |>
      
      # City boundary.
      addPolygons(
        data = city_boundary,
        group = "City boundary",
        fill = FALSE,
        color = BLACK,
        weight = 2,
        options = pathOptions(
          interactive = FALSE
        )
      )
    
    if (
      length(bvri_clipped) &&
      any(!st_is_empty(bvri_clipped))
    ) {
      # Black base stroke.
      map <- map |>
        addPolygons(
          data = bvri_clipped,
          group = "BVRI boundary",
          fill = FALSE,
          color = BLACK,
          weight = 5,
          opacity = 1,
          options = pathOptions(
            interactive = FALSE,
            lineCap = "butt"
          )
        ) |>
        
        # Gold dashed stroke over black creates stripes.
        addPolygons(
          data = bvri_clipped,
          group = "BVRI boundary",
          fill = FALSE,
          color = GOLD,
          weight = 5,
          opacity = 1,
          dashArray = "10, 8",
          options = pathOptions(
            interactive = FALSE,
            lineCap = "butt"
          )
        )
    }
    
    map |>
      addLegend(
        position = "bottomright",
        colors = unname(STATUS_COLORS[PUBLIC_STATUSES]),
        labels = PUBLIC_STATUSES,
        title = "HNI homes"
      ) |>
      addScaleBar(position = "bottomleft") |>
      setMaxBounds(
        lng1 = -77.15,
        lat1 = 38.85,
        lng2 = -76.05,
        lat2 = 39.85
      ) |>
      setView(
        lng = -76.6122,
        lat = 39.296,
        zoom = 12
      )
  })
  
  outputOptions(
    output,
    "map",
    suspendWhenHidden = FALSE
  )
  
  observe({
    properties <- filtered_properties()
    
    mapped <- properties[
      is.finite(properties$longitude) &
        is.finite(properties$latitude),
      ,
      drop = FALSE
    ]
    
    proxy <- leafletProxy("map", session) |>
      clearGroup("listings")
    
    if (nrow(mapped)) {
      proxy |>
        addCircleMarkers(
          lng = mapped$longitude,
          lat = mapped$latitude,
          layerId = mapped$id,
          group = "listings",
          radius = 8,
          color = "white",
          weight = 2,
          fillColor = unname(
            STATUS_COLORS[mapped$group]
          ),
          fillOpacity = 0.95,
          label = paste(
            mapped$address,
            mapped$group,
            sep = " · "
          ),
          popup = vapply(
            seq_len(nrow(mapped)),
            function(i) property_popup(mapped[i, ]),
            character(1)
          )
        )
    }
  })
  
  output$property_count <- renderText({
    paste(nrow(filtered_properties()), "properties")
  })
  
  output$updated <- renderText({
    paste("CSV updated:", initial_data$updated)
  })
  
  output$property_list <- renderUI({
    properties <- filtered_properties()
    
    if (!nrow(properties)) {
      return(tags$p(
        "No properties match these filters."
      ))
    }
    
    tagList(lapply(
      seq_len(nrow(properties)),
      function(i) {
        property <- properties[i, ]
        
        tags$button(
          type = "button",
          class = "property-card",
          `data-id` = property$id,
          
          span(
            class = "status-label",
            style = paste0(
              "color:",
              STATUS_COLORS[property$group]
            ),
            property$group
          ),
          
          strong(property$address),
          
          span(property$neighborhood),
          
          span(
            class = "muted",
            property$organization
          )
        )
      }
    ))
  })
  
  select_property <- function(id, pan = TRUE) {
    properties <- initial_data$properties
    
    property <- properties[
      properties$id == id,
      ,
      drop = FALSE
    ]
    
    if (nrow(property) != 1) return(FALSE)
    
    selected(property)
    parcel_candidates(NULL)
    search_message(NULL)
    
    if (pan) {
      view_mode("map")
      session$sendCustomMessage("resize-map", TRUE)
      
      if (
        is.finite(property$longitude) &&
        is.finite(property$latitude)
      ) {
        leafletProxy("map", session) |>
          clearGroup("selection") |>
          clearPopups() |>
          setView(
            property$longitude,
            property$latitude,
            17
          ) |>
          addCircleMarkers(
            lng = property$longitude,
            lat = property$latitude,
            group = "selection",
            radius = 13,
            color = BLACK,
            fillOpacity = 0,
            weight = 3
          ) |>
          addPopups(
            lng = property$longitude,
            lat = property$latitude,
            popup = property_popup(property)
          )
      } else {
        search_message(
          "Property found, but its map location needs verification."
        )
        view_mode("details")
      }
    }
    
    TRUE
  }
  
  observeEvent(input$selected_property, {
    select_property(input$selected_property)
  })
  
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    
    if (!is.null(id)) {
      select_property(id)
    }
  })
  
  observeEvent(input$more_information, {
    if (
      isTRUE(select_property(
        input$more_information,
        pan = FALSE
      ))
    ) {
      view_mode("details")
    }
  })
  
  observeEvent(input$back_to_map, {
    view_mode("map")
    session$sendCustomMessage("resize-map", TRUE)
  })
  
  output$property_details <- renderUI({
    property <- selected()
    
    if (is.null(property)) {
      return(tagList(
        h3("Property information"),
        p("Select a property on the map.")
      ))
    }
    
    contacts <- initial_data$developers
    
    contacts <- contacts[
      name_key(contacts$organization) ==
        name_key(property$organization),
      ,
      drop = FALSE
    ]
    
    inside <- in_bvri_area(
      property$longitude,
      property$latitude
    )
    
    detail_item <- function(label, value) {
      div(
        div(class = "detail-label", label),
        p(if (nzchar(value)) value else "Not recorded")
      )
    }
    
    tagList(
      div(
        class = "details-heading",
        h3("Property information"),
        h2(property$address),
        p(property_status_message(property$group))
      ),
      
      div(
        class = "detail-grid",
        
        detail_item(
          "Neighborhood",
          property$neighborhood
        ),
        
        detail_item(
          "Developer",
          property$organization
        ),
        
        detail_item(
          "Approval date",
          property$approved_date
        ),
        
        detail_item(
          "Block / lot",
          property$blocklot
        ),
        
        detail_item(
          "Bright MLS",
          property$mls
        ),
        
        detail_item(
          "City-record address",
          property$city_address
        )
      ),
      
      if (
        grepl(
          "^https?://",
          property$link,
          ignore.case = TRUE
        )
      ) {
        tags$a(
          class = "listing-link",
          href = property$link,
          target = "_blank",
          rel = "noopener noreferrer",
          "View listing / developer website"
        )
      },
      
      h4("BVRI geography"),
      
      p(bvri_message(inside)),
      
      h4("Developer contacts"),
      
      if (!nrow(contacts)) {
        p("No matching developer contact is recorded.")
      } else {
        tagList(lapply(
          seq_len(nrow(contacts)),
          function(i) {
            contact <- contacts[i, ]
            
            div(
              class = "contact",
              
              strong(contact$name),
              p(contact$title),
              
              if (nzchar(contact$email)) {
                tags$a(
                  href = paste0(
                    "mailto:",
                    contact$email
                  ),
                  contact$email
                )
              },
              
              if (nzchar(contact$phone)) {
                p(tags$a(
                  href = paste0(
                    "tel:",
                    gsub(
                      "[^+0-9]",
                      "",
                      contact$phone
                    )
                  ),
                  contact$phone
                ))
              }
            )
          }
        ))
      },
      
      p(
        class = "muted",
        paste(
          "Property approval does not establish buyer",
          "or financing eligibility.",
          "Confirm current availability with the developer."
        )
      )
    )
  })
  
  output$search_result <- renderUI({
    result <- search_message()
    
    if (!is.null(result)) {
      div(
        class = "search-result",
        role = "status",
        result
      )
    }
  })
  
  output$parcel_choices <- renderUI({
    candidates <- parcel_candidates()
    
    if (is.null(candidates)) return(NULL)
    
    labels <- vapply(
      candidates,
      function(feature) {
        paste(
          feature$properties$FULLADDR,
          feature$properties$BLOCKLOT,
          sep = " · "
        )
      },
      character(1)
    )
    
    tagList(
      selectInput(
        "city_parcel",
        "Choose the matching parcel",
        choices = setNames(
          as.character(seq_along(candidates)),
          labels
        )
      ),
      
      actionButton(
        "show_parcel",
        "Show parcel",
        class = "btn-primary"
      )
    )
  })
  
  show_city_property <- function(feature) {
    coordinates <- tryCatch(
      parcel_point(feature),
      error = function(e) c(NA_real_, NA_real_)
    )
    
    if (!all(is.finite(coordinates))) {
      search_message(
        paste(
          "The city record was found,",
          "but its location could not be checked."
        )
      )
      return()
    }
    
    inside <- in_bvri_area(
      coordinates[1],
      coordinates[2]
    )
    
    result <- paste(
      bvri_message(inside),
      paste(
        "This address is not on HNI's current",
        "recorded SPP property list."
      )
    )
    
    # Build a temporary property record for the details button.
    city_property <- data.frame(
      organization = "",
      neighborhood = clean_text(
        feature$properties$NEIGHBOR
      ),
      address = clean_text(
        feature$properties$FULLADDR
      ),
      status = "",
      approved_date = "",
      blocklot = clean_text(
        feature$properties$BLOCKLOT
      ),
      city_address = clean_text(
        feature$properties$FULLADDR
      ),
      mls = "",
      link = "",
      group = "Approval unknown",
      id = "city-search",
      longitude = coordinates[1],
      latitude = coordinates[2],
      stringsAsFactors = FALSE
    )
    
    selected(city_property)
    search_message(result)
    view_mode("map")
    
    session$sendCustomMessage("resize-map", TRUE)
    
    color <- if (isTRUE(inside)) {
      GOLD
    } else {
      "#B42318"
    }
    
    popup <- paste0(
      "<div class='property-popup'>",
      
      "<strong>",
      escape_html(city_property$address),
      "</strong>",
      
      "<p>",
      escape_html(result),
      "</p>",
      
      "<button type='button' class='popup-info-button'",
      " data-property-id='city-search'>",
      "More information",
      "</button>",
      
      "</div>"
    )
    
    leafletProxy("map", session) |>
      clearGroup("selection") |>
      clearPopups() |>
      setView(
        coordinates[1],
        coordinates[2],
        17
      ) |>
      addCircleMarkers(
        lng = coordinates[1],
        lat = coordinates[2],
        group = "selection",
        radius = 10,
        color = BLACK,
        weight = 2,
        fillColor = color,
        fillOpacity = 0.95
      ) |>
      addPopups(
        lng = coordinates[1],
        lat = coordinates[2],
        popup = popup
      )
  }
  
  # Handle the temporary city-search record.
  observeEvent(input$more_information, {
    if (
      identical(input$more_information, "city-search") &&
      !is.null(selected()) &&
      identical(selected()$id, "city-search")
    ) {
      view_mode("details")
    }
  })
  
  observeEvent(input$show_parcel, {
    candidates <- parcel_candidates()
    
    index <- suppressWarnings(
      as.integer(input$city_parcel)
    )
    
    if (
      !is.null(candidates) &&
      length(index) == 1 &&
      !is.na(index) &&
      index >= 1 &&
      index <= length(candidates)
    ) {
      show_city_property(candidates[[index]])
    }
  })
  
  observeEvent(input$search, {
    address <- trimws(input$address)
    
    selected(NULL)
    parcel_candidates(NULL)
    search_message(NULL)
    view_mode("map")
    
    session$sendCustomMessage("resize-map", TRUE)
    
    leafletProxy("map", session) |>
      clearGroup("selection") |>
      clearPopups()
    
    if (nchar(address) < 4) {
      search_message(
        "Enter a street number and street name."
      )
      return()
    }
    
    properties <- initial_data$properties
    
    matches <- which(
      address_key(properties$address) ==
        address_key(address)
    )
    
    if (length(matches) == 1) {
      select_property(properties$id[matches])
      return()
    }
    
    if (length(matches) > 1) {
      search_message(
        paste(
          "Multiple HNI records match this address.",
          "Contact HNI to confirm the correct record."
        )
      )
      return()
    }
    
    street_number <- regmatches(
      address,
      regexpr("^[0-9]+", address)
    )
    
    if (
      !length(street_number) ||
      !nzchar(street_number)
    ) {
      search_message(
        "Include the street number, such as 1554 Carswell St."
      )
      return()
    }
    
    withProgress(
      message = "Searching Baltimore City records",
      value = 0.5,
      {
        tryCatch({
          features <- city_query(
            paste0(
              "BLDG_NO = '",
              street_number,
              "'"
            )
          )
          
          exact_matches <- Filter(
            function(feature) {
              address_key(feature$properties$FULLADDR) ==
                address_key(address)
            },
            features
          )
          
          if (length(exact_matches) == 1) {
            show_city_property(exact_matches[[1]])
            
          } else if (length(exact_matches) > 1) {
            parcel_candidates(exact_matches)
            
            search_message(
              "Multiple city parcels match. Choose a parcel below."
            )
            
          } else {
            search_message(paste(
              "No exact address match was found.",
              "Check spelling and include the street direction.",
              "This does not establish ineligibility."
            ))
          }
          
        }, error = function(e) {
          search_message(paste(
            "City address search is temporarily unavailable.",
            "You can still browse the HNI property list."
          ))
        })
      }
    )
  })
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(ui, server)