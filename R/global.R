# -----------------------------------------------------------------------
# global.R
# Loads/creates all data objects used by the dashboard: the main project
# database (projects), the point shapefile used for Project Sites
# (project_points), and the boundary shapefiles used for the Regions/
# Counties choropleth maps (region_bounds, county_bounds).
# -----------------------------------------------------------------------

library(dplyr)

set.seed(11)

# month/year shown as "Last updated: ..." beneath the description at the
# top of every Data View and Map View subtab — update this one value
# whenever the database is refreshed
LAST_UPDATED <- "November 2026"

# the database version, written with periods. The one place to change it
# for a new release: the Download Data tab's button label ("Download
# Database (v1.0.0)") and the downloaded file's name (...v1-0-0.csv)
# are both built from it. (The data files' own names below, and the
# citation page's text, also contain the version and are separate.)
DB_VERSION <- "1.0.0"

# ---- Contribute forms (embedded Google Forms) ----------------------------
# The two forms on the Contribute tab are Google Forms, shown inside the
# dashboard. The embedded form is a LIVE view of the Google Form, so edits
# made in Google (questions, options, required fields, the confirmation
# message...) appear here automatically — no change to this code. These
# values only need updating if a form is replaced by a NEW form (a copy
# has a different address) or if its length changes a lot (the height is
# how tall the embedded frame is). Addresses: in Google Forms, Send > link
# (or the <> embed code); use the part up to and including "/viewform".
FORM_SUBMIT_URL     <- "https://docs.google.com/forms/d/e/1FAIpQLSfFSzBMMvgiNfvqEUHtQsC8IvhHcpaKi5iI9Matuo9g9QMZeg/viewform"
FORM_SUBMIT_HEIGHT  <- 7473   # pixels (from Google's embed code, 4 Nov 2026)
FORM_CORRECT_URL    <- "https://docs.google.com/forms/d/e/1FAIpQLSd3ZxnYMv63rNCt4QNpDp6MUZRvQZPXfqCjwAXINbvp5VwgoA/viewform"
FORM_CORRECT_HEIGHT <- 1200   # pixels (measured 4 Nov 2026: the form is about 1,165 px tall; Google's original number was 958)

# The heights above are the ones Google's embed code gives (Send > <> in
# Google Forms; it calculates them from the form as it is at that moment).
# If a form gets longer later (new questions), the frame becomes too short
# and a second scrollbar appears INSIDE the form. To prevent that, each
# frame is made a little taller than the number above:
#   FORM_HEIGHT_HEADROOM        on tablets and computers (1.05 = 5% taller)
#   FORM_HEIGHT_HEADROOM_PHONE  on phones (screens under 768 px wide), where
#                               the narrower form wraps onto more lines
# Spare height just looks like a little empty space under the form, as
# long as the form's background is white. After you add or remove several
# questions, re-copy the height from Google's embed code and update it above.
FORM_HEIGHT_HEADROOM       <- 1.05
FORM_HEIGHT_HEADROOM_PHONE <- 1.15

# ---- load the master database -------------------------------------------
# ONE file feeds everything: the dashboard's charts and maps (through
# `raw` below) and the Download Data tab (through `download_data`, which
# is the very same data, with all of its columns).

DATA_FILE <- here::here("Data", "master_db_V1.0.0.csv")

# `encoding = "UTF-8"` (rather than `fileEncoding = "UTF-8"`) is deliberate:
# it DECLARES the file's encoding without asking R to translate it into
# the session's native one. The data has non-ASCII characters (en dashes,
# curly apostrophes, accented letters in project names), and in a
# non-UTF-8 R locale (common on Linux servers) `fileEncoding` silently
# stops reading at the first one — loading just a handful of rows with
# only a warning. This form reads the whole file in any locale.
raw <- read.csv(DATA_FILE, stringsAsFactors = FALSE, na.strings = c("NA", ""), encoding = "UTF-8")

# The columns the dashboard reads from the file. If one is missing (say,
# a field was renamed), stop right here and name it — otherwise the
# failure shows up further down as a confusing "Tibble columns must have
# compatible sizes" error. (The data source column is Data_Source.)
required_cols <- c(
  "Master_Project_ID", "Project_Name", "EDA", "County", "Habitat_Type",
  "Restoration_Technique", "Data_Source", "Project_Status", "NEP_Activity_Name",
  "Main_Funding_Category", "Lead_Implementer_Category", "Completion_Year",
  "Latitude", "Longitude", "Acres_Restored", "Miles_Restored", "Reported_Project_Cost"
)
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop(basename(DATA_FILE), " is missing column(s) the dashboard needs: ",
       paste(missing_cols, collapse = ", "), ". Columns found: ",
       paste(names(raw), collapse = ", "), call. = FALSE)
}

# the name the downloaded file gets in the user's browser (without ".csv"):
# the version from DB_VERSION above with its periods swapped for dashes
# (v1.0.0 -> v1-0-0; fixed = TRUE so "." means a literal period, not "any
# character"). "_subset" is added to it when the download holds only some
# of the records (see output$dwnld in app.R).
DOWNLOAD_NAME <- paste0("fl_estuarine_restoration_database_v", gsub(".", "-", DB_VERSION, fixed = TRUE))

# The metadata workbook that travels with the database: every download is
# a ZIP holding the data file AND this file. Its name follows the same
# version setting (v1.0.0 -> Data/metadata_v1-0-0.xlsx), so a version bump
# only means saving the new workbook under the new name in the Data folder.
METADATA_FILE <- here::here("Data", paste0("metadata_v", gsub(".", "-", DB_VERSION, fixed = TRUE), ".xlsx"))

# Can this machine make ZIP files? Needs either the R "zip" package (best:
# install.packages("zip")) or a "zip" command on the system. If neither is
# there, downloads quietly fall back to the plain CSV instead of failing.
ZIP_OK <- requireNamespace("zip", quietly = TRUE) || nzchar(Sys.which("zip"))

if (!file.exists(METADATA_FILE)) {
  message("Download Data: ", basename(METADATA_FILE), " was not found in the Data folder, ",
          "so downloads will contain the database only.")
}
if (!ZIP_OK) {
  message("Download Data: no way to make ZIP files here (neither the R 'zip' package nor a ",
          "'zip' command), so downloads will be the plain CSV. Fix: install.packages('zip')")
}

# What the Download Data tab shows, searches/filters, and serves: the
# master database exactly as read, with every column it has.
download_data <- raw


# ---- MULTI-VALUE FIELDS ---------------------------------------------------
# Restoration_Technique and Data_Source can each list MULTIPLE comma-separated
# values in one cell (e.g. "Species control, Planting"). (County used to
# as well; the final database lists a single primary county per project.)
# Two different operations need this handled differently, and each is
# handled at a different point rather than by cleaning the data up front:
#
#   - FILTERING: a project should match if it contains ANY of the
#     selected values — see multi_match() in funcs.R, used wherever
#     these fields are filtered. A project matches (and is counted)
#     once, no matter how many selected values it happens to list.
#   - BREAKDOWN CHARTS ("count of projects BY technique"): a multi-value
#     project should contribute to EACH of its bars/slices — that's
#     correct multi-label behavior, not double-counting. See
#     explode_multi() in funcs.R, called at the point each specific
#     chart is built (not here, so it doesn't affect anything else).
#
# split_choices() below just builds the individual-value list used to
# populate filter dropdowns/pills (e.g. "Species control" and
# "Planting" as two separate choices, not "Species control, Planting"
# as one combined choice).
split_choices <- function(x) {
  x <- x[!is.na(x)]
  vals <- unlist(strsplit(x, ",\\s*"))
  sort(unique(trimws(vals)))
}

# ---- derive the columns/names the rest of the app expects ----------------
# The data file has no row-number column of its own, so the app numbers the
# records itself: record 1 is the first data row of the file, and so on.
row_num <- seq_len(nrow(raw))

# project_name: sourced from the real Project_Name field. Falls back to
# a constructed "region + row number" label only for any rows where
# Project_Name is missing — a defensive default in case it's not 100%
# populated, not because it's expected to be common.
project_name_derived <- ifelse(
  !is.na(raw$Project_Name) & nzchar(trimws(raw$Project_Name)),
  raw$Project_Name,
  paste0(ifelse(is.na(raw$EDA), "Unspecified region", raw$EDA), " project (row ", row_num, ")")
)

projects <- tibble(
  project_id           = row_num,
  master_project_id    = raw$Master_Project_ID,        # a single real-world project can
                                                         # span MULTIPLE rows (different habitat/
                                                         # technique/acreage entries sharing one
                                                         # overall cost) — see dedupe_projects()
                                                         # below and its usage notes throughout
                                                         # funcs.R for how this is handled
  project_name         = project_name_derived,
  region               = raw$EDA,                      # single-value
  county               = raw$County,                    # single-value (the project's primary county)
  habitat              = raw$Habitat_Type,              # single-value
  technique            = raw$Restoration_Technique,     # MULTI-VALUE (raw string, kept as-is)
  source               = raw$Data_Source,               # MULTI-VALUE (raw string, kept as-is)
  project_status       = raw$Project_Status,
  nep_activity_name    = raw$NEP_Activity_Name,
  funding_category     = raw$Main_Funding_Category,
  implementer_category = raw$Lead_Implementer_Category,
  year                 = raw$Completion_Year,
  Latitude             = raw$Latitude,
  Longitude            = raw$Longitude,
  area_acres           = raw$Acres_Restored,
  # NOTE: the source field is in MILES, not meters — renamed from
  # length_m to length_miles throughout the app to match. If you'd
  # rather standardize on meters instead, convert here:
  #   length_miles = raw$Miles_Restored * 1609.34
  length_miles         = raw$Miles_Restored,
  cost_dollars         = raw$Reported_Project_Cost
)


# ---- lookup vectors (drive dropdown/pill choices across the app) -------
# NA is excluded here (missing isn't a real selectable category) — see
# funcs.R for how NA gets displayed as "Unspecified" inside charts.

regions    <- sort(unique(na.omit(projects$region)))

# exclusion criteria for the pre-filtered "working" dataset used by
# Restored Habitats/Techniques/Dollars (see working_data_fun() below) —
# defined here, before the habitat/technique choice lists are built,
# since those dropdowns also need to exclude these names (they
# shouldn't be selectable as a standalone focus, even though a project
# listing one ALONGSIDE a real habitat/technique stays in the working
# dataset)
EXCLUDED_HABITATS   <- c("Upland", "Coastal upland", "Upland and freshwater", "Unknown", "Other", "Freshwater wetland")
EXCLUDED_TECHNIQUES <- c("Species control")

habitats   <- setdiff(sort(unique(na.omit(projects$habitat))), EXCLUDED_HABITATS)
techniques <- setdiff(split_choices(projects$technique), EXCLUDED_TECHNIQUES)

# ---- fixed color palette for habitat charts ------------------------------
# Keys must match the real Habitat_Type values EXACTLY (case-sensitive)
# for the lookup to work — see stacked_bar_fun()/donut_fun()'s `palette`
# argument in funcs.R, which uses this by name-matching against
# whatever habitat values are actually present in the filtered data.
# "Unspecified" (a recoded-NA marker used inside those chart functions)
# is included here too, kept visually distinct from the literal
# "Unknown" survey response.
# colors below were audited/adjusted for a 3:1 minimum contrast ratio
# against white (WCAG non-text contrast, per an accessibility review) —
# several originals were pastel/light colors that failed this check;
# darkened versions preserve each one's general hue
habitat_colors <- c(
  "Coastal wetland"                         = "seagreen",
  "Estuarine"                               = "#0097C9",
  "Estuarine shoreline"                     = "#5B9D21",
  "Freshwater wetland"                      = "steelblue",
  "Hard bottom"                             = "#D47400",
  "Mangroves"                               = "darkgreen",
  "Mud/sand"                                = "orchid4",
  "Other"                                   = "#8181EE",
  "Oysters"                                 = "rosybrown4",
  "Salt marsh"                              = "firebrick",
  "Submerged aquatic vegetation"            = "#B68200",
  "Tidal tributary/creek/river"             = "darkcyan",
  "Upland"                                  = "#8C8C8C",
  "Water column"                            = "#0E9BAA",
  "Unknown"                                 = "gray30",
  "Coastal upland"                          = "#B7823C",
  "Shell bottom"                            = "#A58866",
  "Estuarine with freshwater and/or upland" = "darkslategray4",
  "Upland and freshwater"                   = "#958E3A",
  "Unspecified"                             = "#6F6F6F"
)

# confirmed against the real data: all 15 keys match Restoration_Technique
# exactly (including "Species control"/"Education/recreation", which
# only surface in charts using the full unfiltered data, e.g. Database
# Overview's own Restoration Techniques chart — working_data_fun()'s
# scrubbing removes them from every other chart's technique values)
technique_colors <- c(
  "Species control"                    = "#958E3A",
  "Planting"                           = "darkgreen",
  "Reef construction"                  = "orchid4",
  "Conservation/preservation"          = "#00A200",
  "Unknown"                            = "gray30",
  "Process-based physical restoration" = "#B68200",
  "Prescribed burn"                    = "#D47400",
  "Hydrologic restoration"             = "steelblue",
  "Debris removal"                     = "#B7823C",
  "Living shoreline"                   = "#3AA109",
  "Aquaculture (shellfish)"            = "maroon",
  "Education/recreation"               = "#FF3B9D",
  "Erosion control"                    = "rosybrown4",
  "Other"                              = "#8C8C8C",
  "Stream modification"                = "#0E9BAA"
)

# used for Map View's "Activity" color-by option specifically (NEP_
# Activity_Name) — "Maintenance" is a real 3rd value in the raw data,
# but is always excluded by working_data_fun()'s pre-filter, so it
# never actually reaches Map View's data and doesn't need a color here
activity_colors <- c(
  "Restoration" = "#148F1F",
  "Protection"  = "#1D5BED",
  "Unknown"     = "#666666"
)

funding_colors <- c(
  "Academic"      = "#919100",
  "Federal"       = "#5B9D21",
  "Local"         = "#D07500",
  "Mixed-partner" = "saddlebrown",
  "NGO/Nonprofit" = "#1f78b4",
  "NRDA Settlement" = "navy",
  "Other"         = "#e7298a",
  "Private"       = "firebrick",
  "Regional"      = "#A97CC0",
  "State"         = "#6a3d9a",
  "Unknown"       = "#666666"
)


yrmin <- min(projects$year, na.rm = TRUE)
yrmax <- max(projects$year, na.rm = TRUE)

# ---- Download Data tab: choices for its search/filter box -----------------
# Built from the DOWNLOADABLE database itself (not the dashboard's
# pre-filtered lists above), so every category in the file is selectable —
# including ones the dashboard's charts leave out (e.g. Upland, Species
# control) — since people may want to download exactly those records.
dl_has_col <- function(col) !is.null(download_data) && col %in% names(download_data)
dl_choices <- function(col) if (dl_has_col(col)) split_choices(download_data[[col]]) else character(0)

dl_regions    <- dl_choices("EDA")
dl_habitats   <- dl_choices("Habitat_Type")
dl_techniques <- dl_choices("Restoration_Technique")
dl_years      <- if (dl_has_col("Completion_Year")) range(download_data$Completion_Year, na.rm = TRUE) else c(yrmin, yrmax)

# ---- boundary shapefiles for the Regions/Counties choropleth maps -------
has_sf <- requireNamespace("sf", quietly = TRUE)

region_bounds <- NULL
county_bounds <- NULL

# confirmed against the real shapefiles: estuaries_final's name field
# is "EDA" (matches project_points$region exactly — every region in the
# final data is one of its 24); counties_final's name field is "County"
# (matches project_points$county, with the capitalization safety net
# below)
REGION_NAME_FIELD <- "EDA"
COUNTY_NAME_FIELD <- "County"

# Snaps county names to the boundary file's exact spelling when they differ
# only by capitalization — the final data spells it "Desoto" but the
# boundary file has "DeSoto", and a mismatch would leave that county's
# polygon empty (showing 0 projects). Names with no case-insensitive
# match are returned unchanged. Used on project_points below.
match_county_names <- function(x, bounds) {
  if (is.null(bounds) || !COUNTY_NAME_FIELD %in% names(bounds)) return(x)
  canon <- as.character(bounds[[COUNTY_NAME_FIELD]])
  hit <- match(tolower(x), tolower(canon))
  ifelse(is.na(hit), x, canon[hit])
}

if (has_sf) {

  region_bounds <- tryCatch({
    .region_env <- new.env()
    load(here::here("Data", "estuaries.RData"), envir = .region_env)
    get("estuaries_final", envir = .region_env)
  }, error = function(e) {
    message("Could not load region boundary shapefile: ", conditionMessage(e))
    NULL
  })

  county_bounds <- tryCatch({
    .county_env <- new.env()
    load(here::here("Data", "counties.RData"), envir = .county_env)
    get("counties_final", envir = .county_env)
  }, error = function(e) {
    message("Could not load county boundary shapefile: ", conditionMessage(e))
    NULL
  })

} else {
  message("Package `sf` not installed — Regions/Counties map tabs will show a placeholder message until it is (install.packages('sf')).")
}

# ---- real point shapefile for individual project sites -------------------
# Same underlying data as the CSV (same records, same field names),
# pre-joined with point geometry — same CRS (EPSG:4326) leaflet expects,
# no empty geometries. Loaded into its own environment so only the sf
# object inside gets pulled out, whatever it's named in the file.
#
# The loader expects the final database's field names (Project_Name,
# Master_Project_ID, Reported_Project_Cost, ...). If the file is missing
# one, the startup message below names it and the map tabs stay empty.
POINTS_FILE <- here::here("Data", "dashboard_points_V1.0.0.RData")

project_points <- NULL

if (has_sf) {
  project_points <- tryCatch({
    .pts_env <- new.env()
    load(POINTS_FILE, envir = .pts_env)
    # the file's one sf object, whatever it happens to be named
    sf_objs <- Filter(function(o) inherits(o, "sf"), mget(ls(.pts_env), envir = .pts_env))
    if (length(sf_objs) != 1) {
      stop("expected exactly one sf object in ", basename(POINTS_FILE), ", found ", length(sf_objs))
    }
    final_pts <- sf_objs[[1]]

    # renamed to match the same field names used everywhere else in the
    # app (projects tibble, working_data_fun(), multi_match(), etc.) —
    # several source columns come in as factors rather than character;
    # coerced explicitly to avoid subtle factor-level mismatches later
    # the data source column is "Data_Source" in the master database (it
    # was "Source" in earlier versions) — either works here, in case the
    # points file hasn't been regenerated from the master database yet
    src_col <- intersect(c("Data_Source", "Source"), names(final_pts))[1]
    if (is.na(src_col)) stop("the points file has no Data_Source (or Source) column")

    pts <- final_pts %>%
      dplyr::transmute(
        project_name         = as.character(Project_Name),
        master_project_id    = Master_Project_ID,
        region               = as.character(EDA),
        county               = as.character(County),
        habitat              = as.character(Habitat_Type),
        technique            = as.character(Restoration_Technique),
        source               = as.character(.data[[src_col]]),
        project_status       = as.character(Project_Status),
        nep_activity_name    = as.character(NEP_Activity_Name),
        funding_category     = as.character(Main_Funding_Category),
        implementer_category = as.character(Lead_Implementer_Category),
        year                 = Completion_Year,
        Latitude,
        Longitude,
        area_acres           = Acres_Restored,
        length_miles         = Miles_Restored,
        acres_category       = as.character(Acres_Restored_Category),
        miles_category       = as.character(Miles_Restored_Category),
        cost_dollars         = Reported_Project_Cost
      )

    # snap county names to the boundary file's spelling (see
    # match_county_names() above)
    pts$county <- match_county_names(pts$county, county_bounds)
    pts
  }, error = function(e) {
    message("Could not load point shapefile: ", conditionMessage(e))
    NULL
  })
}

# ---- pre-filtered "working" dataset used by Restored Habitats onward ----
# Database Overview intentionally shows the FULL, unfiltered dataset —
# every other Data View subtab builds on this filtered baseline instead,
# layered under the shared year/region/habitat/technique filters.
#
#   1. Activity type: exclude "Maintenance" only (Restoration,
#      Protection, and NA/unknown-source all stay in)
#   2. Habitat type: exclude Upland, Coastal upland, Upland and
#      freshwater, Unknown, and Other
#   3. Restoration technique: exclude a row only if EVERY technique it
#      lists is Education/recreation and/or Species control — a
#      project listing one of these ALONGSIDE a real technique (e.g.
#      "Species control, Planting") still represents genuine
#      restoration work and stays in, but with its technique field
#      scrubbed down to just the real technique(s) — "Species control,
#      Planting" becomes just "Planting" (see
#      technique_all_excluded()/strip_excluded_techniques() in
#      funcs.R). Those two names are also dropped from the technique
#      filter dropdown's choices above, since they're never a
#      selectable focus on their own.
working_data_fun <- function(data) {
  data <- data %>%
    dplyr::filter(
      is.na(nep_activity_name) | nep_activity_name != "Maintenance",
      is.na(habitat) | !(habitat %in% EXCLUDED_HABITATS),
      !technique_all_excluded(technique, EXCLUDED_TECHNIQUES)
    )
  data$technique <- strip_excluded_techniques(data$technique, EXCLUDED_TECHNIQUES)
  data
}

# ---- collapse to one row per real-world project --------------------------
# A single project can span MULTIPLE rows sharing one master_project_id —
# e.g. one row per habitat restored, each with its own acres/technique/
# year, but ALL of them repeating the SAME overall project cost. Used
# wherever a chart needs an accurate "number of projects" or "total
# cost" that ISN'T broken down by habitat/technique — counting or
# summing raw rows in that case would over-count multi-row projects
# (a 6-row project would count as 6 projects, or sum its cost 6 times).
#
# NOT used for habitat/technique breakdowns — there, each row
# genuinely represents a distinct habitat/technique the project
# touched, and per this dashboard's established design (matching how
# multi-value technique/source fields are already exploded elsewhere),
# each should be represented on its own, cost duplication and all.
dedupe_projects <- function(data) {
  data %>% dplyr::distinct(master_project_id, .keep_all = TRUE)
}
