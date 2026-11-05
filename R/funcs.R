# -----------------------------------------------------------------------
# funcs.R
# Helper functions that build the plots/tables used in server().
# -----------------------------------------------------------------------

library(dplyr)
library(plotly)
library(ggplot2)

# ---- catch-all categories always go last ---------------------------------
# "Other", "Unknown", and "Unspecified" (how missing values are labeled
# in several charts) are catch-all buckets rather than real categories,
# so every chart places them at the END of its categories/legend —
# however the remaining categories are ordered (alphabetical, by size,
# etc.) is left exactly as-is.
#
# catchall_rank() is 0 for a normal category and 1/2/3 for the catch-
# alls, so ordering by it is stable for everything else.
CATCHALL_LABELS <- c("Other", "Unknown", "Unspecified")

catchall_rank <- function(x) match(as.character(x), CATCHALL_LABELS, nomatch = 0L)

# reorders a vector of category labels (already in the desired order)
# so the catch-all labels come last
catchall_last <- function(x) x[order(catchall_rank(x))]

# ---- Download Data tab: search + filter the downloadable records ---------
# Returns the rows of `data` (the downloadable database) that meet `crit`,
# a list with:
#   search      text typed in "Search by Project Name". Case-insensitive;
#               a record matches only if its name contains EVERY word typed
#               (in any order), and special characters are taken literally.
#   all_years   TRUE = don't filter on year at all (so records with no
#               completion year are kept); FALSE = keep only records whose
#               Completion_Year falls within `years`
#   years       c(first, last)
#   regions, habitats, techniques
#               selected values; an EMPTY selection means "don't filter on
#               this field" (the dropdowns show "All records" when nothing
#               is selected). Otherwise a record matches if it contains ANY
#               of the selected values, so a record listing several
#               (e.g. "Planting, Species control") is returned for each.
# A filter whose column isn't in `data` is skipped rather than failing.
filter_download_records <- function(data, crit) {
  if (is.null(data)) return(data)
  keep <- rep(TRUE, nrow(data))
  has  <- function(col) col %in% names(data)

  terms <- strsplit(tolower(trimws(crit$search)), "\\s+")[[1]]
  terms <- terms[nzchar(terms)]
  if (length(terms) > 0 && has("Project_Name")) {
    nm <- tolower(as.character(data$Project_Name))
    for (term in terms) keep <- keep & grepl(term, nm, fixed = TRUE)
  }

  if (!isTRUE(crit$all_years) && has("Completion_Year")) {
    yr <- suppressWarnings(as.numeric(data$Completion_Year))
    keep <- keep & !is.na(yr) & yr >= crit$years[1] & yr <= crit$years[2]
  }

  if (length(crit$regions)    > 0 && has("EDA"))                   keep <- keep & multi_match(data$EDA, crit$regions)
  if (length(crit$habitats)   > 0 && has("Habitat_Type"))          keep <- keep & multi_match(data$Habitat_Type, crit$habitats)
  if (length(crit$techniques) > 0 && has("Restoration_Technique")) keep <- keep & multi_match(data$Restoration_Technique, crit$techniques)

  data[keep, , drop = FALSE]
}

# ---- Download Data tab: writing the CSV, and the ZIP around it ------------
# write_csv_utf8(): the same layout as write.csv(df, path, row.names = FALSE)
# (text columns in quotes with embedded quotes doubled, numbers bare, NA
# written as NA) — but the file is ALWAYS UTF-8, whatever locale R runs in.
# write.csv() translates text to the *session's* encoding, and in a
# non-UTF-8 locale (common on servers) that turns accented letters, en
# dashes and curly quotes into literal "<U+00ED>"-style text, even with
# fileEncoding = "UTF-8", corrupting project names. Here the text is
# converted to UTF-8 explicitly and written as bytes, so nothing is ever
# translated.
write_csv_utf8 <- function(df, path) {
  quote_text <- function(x) {
    x <- enc2utf8(as.character(x))
    out <- paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
    out[is.na(x)] <- "NA"
    out
  }
  cols <- lapply(df, function(col) {
    if (is.character(col) || is.factor(col)) {
      quote_text(col)
    } else {
      s <- as.character(col)
      s[is.na(col)] <- "NA"
      s
    }
  })
  lines <- if (nrow(df) > 0) do.call(paste, c(unname(cols), sep = ",")) else character(0)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(c(paste(quote_text(names(df)), collapse = ","), lines), con, useBytes = TRUE)
  invisible(path)
}

# make_zip(): puts `files` (full paths, all in ONE folder) into a ZIP file
# at `zipfile`, with no folder structure inside. Uses the R "zip" package
# when it's installed (self-contained, behaves the same everywhere), and
# otherwise the system's `zip` command. Stops with a plain message if
# neither works or nothing was produced.
make_zip <- function(zipfile, files) {
  if (file.exists(zipfile)) file.remove(zipfile)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile, files = basename(files), root = dirname(files[1]))
  } else if (nzchar(Sys.which("zip"))) {
    utils::zip(zipfile, files = files, flags = "-j -q -X")
  } else {
    stop("no way to create a ZIP file: install the R 'zip' package")
  }
  if (!file.exists(zipfile) || file.size(zipfile) == 0) stop("creating the ZIP file failed")
  invisible(zipfile)
}

# ---- Contribute forms: an embedded Google Form ---------------------------
# Shows a Google Form inside the dashboard (addresses and heights are set
# in global.R). The iframe is "lazy": the browser doesn't fetch the form
# until the tab holding it is actually opened, so the Contribute forms
# add nothing to the dashboard's own loading time. `title` names the frame
# for screen readers. The link underneath is a fallback for the rare
# visitor whose browser or blocker stops the embedded form from loading.
google_form_embed <- function(url, height, title) {
  # The frame is made a little taller than Google's number so that adding a
  # question or two doesn't cause a second, nested scrollbar inside the
  # form: 5% taller normally, 15% on phones (FORM_HEIGHT_HEADROOM and
  # FORM_HEIGHT_HEADROOM_PHONE in global.R). The three numbers are handed
  # to the stylesheet as CSS variables, which does the sizing, so the
  # extra height can differ by screen size. The height attribute is only a
  # fallback for a browser that doesn't understand that.
  spare       <- if (exists("FORM_HEIGHT_HEADROOM")) FORM_HEIGHT_HEADROOM else 1
  spare_phone <- if (exists("FORM_HEIGHT_HEADROOM_PHONE")) FORM_HEIGHT_HEADROOM_PHONE else spare
  div(
    class = "fer-form-embed",
    tags$iframe(
      src = paste0(url, "?embedded=true"), title = title, loading = "lazy",
      width = "100%", height = ceiling(height * spare),
      style = sprintf("border: 0; --form-h: %spx; --form-spare: %s; --form-spare-phone: %s;",
                      height, spare, spare_phone)
    ),
    # one HTML string (not separate pieces) so there's no stray space
    # between the link and the full stop
    p(
      class = "fer-form-fallback",
      HTML(paste0(
        "Having trouble viewing the form? ",
        '<a href="', htmltools::htmlEscape(url, attribute = TRUE),
        '" target="_blank" rel="noopener noreferrer">Open it in a new tab</a>.'
      ))
    )
  )
}

# ---- x-axis for the year-based charts -------------------------------------
# Every temporal chart is given the year range selected in the dashboard
# (xrange) and pins its x-axis to exactly that span, with half a year of
# padding each side so edge bars aren't clipped. Without this, each chart
# would size its own axis from whichever years it happens to have data
# for — so the charts disagree whenever one metric (e.g. cost, or miles)
# is missing in some of the selected years. xrange = NULL keeps the old
# behavior (the axis fits the data).
year_axis <- function(xrange = NULL) {
  if (is.null(xrange)) list(title = "") else list(title = "", range = c(xrange[1] - 0.5, xrange[2] + 0.5))
}

# ---- multi-value field helpers -------------------------------------------
# Restoration_Technique and Data_Source can each hold MULTIPLE comma-separated
# values in one cell (e.g. "Species control, Planting"). (County could too
# in earlier versions of the database; the final one lists a single
# primary county per project — the county maps still tolerate multiple
# values, see polygon_summary_fun().)
# These two functions handle that correctly for two different situations:

# FILTERING: a row matches if it contains ANY of the selected values.
# Returns one TRUE/FALSE per row, so a project is counted once in the
# filtered result no matter how many selected values it happens to list.
multi_match <- function(field, selected) {
  if (length(selected) == 0) return(rep(TRUE, length(field)))
  vapply(field, function(x) {
    if (is.na(x)) return(FALSE)
    vals <- trimws(strsplit(x, ",")[[1]])
    any(vals %in% selected)
  }, logical(1), USE.NAMES = FALSE)
}

# BREAKDOWN CHARTS ("count of projects BY technique"): a multi-value row
# should contribute to EACH of its own categories — that's correct
# multi-label behavior (bars/slices can sum to more than the total
# project count), not double-counting. This expands one row per value
# listed in `col`, duplicating the row's other columns as-is. Call this
# right before building a chart that groups/counts by a multi-value
# column — not on the master `projects` table itself, so it only
# affects that one chart.
explode_multi <- function(data, col) {
  # nothing to expand — return as-is. (Without this, the zero-row case
  # ends up assigning NULL to `col` below, which silently DELETES the
  # column and breaks whatever groups by it next.)
  if (nrow(data) == 0) return(data)

  vals_list <- strsplit(as.character(data[[col]]), ",\\s*")
  n_each <- lengths(vals_list)
  n_each[n_each == 0] <- 1  # NA/empty stays as one row (shows as NA)

  idx <- rep(seq_len(nrow(data)), n_each)
  out <- data[idx, , drop = FALSE]

  flat_vals <- unlist(lapply(vals_list, function(v) {
    if (length(v) == 0) return(NA_character_)
    trimws(v)
  }))
  out[[col]] <- flat_vals
  rownames(out) <- NULL
  out
}

# ---- check whether a multi-value field's ENTIRE list falls within a ----
# given excluded set — e.g. a project listing "Species control" alone
# should be dropped, but "Species control, Planting" should NOT be
# (Planting isn't excluded, so the project still represents real
# restoration work — its technique field just gets scrubbed down to
# "Planting" alone, see strip_excluded_techniques() below). NA (no
# technique recorded at all) is never excluded by this check — we
# can't confirm "entirely excluded" without knowing what's there.
technique_all_excluded <- function(field, excluded) {
  vapply(field, function(x) {
    if (is.na(x)) return(FALSE)
    techs <- trimws(strsplit(x, ",\\s*")[[1]])
    all(techs %in% excluded)
  }, logical(1), USE.NAMES = FALSE)
}

# ---- remove specific technique names from a multi-value field, --------
# keeping whatever remains. E.g. "Species control, Planting" with
# "Species control" excluded becomes just "Planting" — the project
# stays represented, but only by its genuine restoration technique(s).
# NA stays NA. Pairs with technique_all_excluded() above, which drops
# rows where NOTHING would remain after this scrubbing.
strip_excluded_techniques <- function(field, excluded) {
  vapply(field, function(x) {
    if (is.na(x)) return(NA_character_)
    techs <- trimws(strsplit(x, ",\\s*")[[1]])
    kept <- techs[!(techs %in% excluded)]
    if (length(kept) == 0) return(NA_character_)
    paste(kept, collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

# ---- Data View: "Restoration Footprint" style dual-axis chart ----------
# bar = count of projects per year, line = sum of a numeric field per year
# (plotted on a secondary y-axis), matching the mockup's combo charts.
#
# Plotly normally lets each y-axis pick its own "nice" tick values
# independently, which draws two separate, misaligned sets of gridlines
# (confusing to read). To avoid that, both axes are forced to start at
# 0 and use the same number of evenly-spaced ticks (n_ticks) — since
# they divide their own 0-to-max range into the same number of equal
# steps, the gridlines land at identical heights for both axes even
# though the numbers differ. Only the primary axis actually draws grid
# lines (showgrid = FALSE on the secondary) so there's just one set of
# horizontal lines on the chart, serving both axes' labels.
footprint_plot_fun <- function(data, sumvar = "area_acres",
                                bar_label = "Completed projects",
                                line_label = "Restored area (acres)",
                                xvar = "year",
                                n_ticks = 6,
                                dedupe_sumvar = FALSE,
                                palette = NULL,
                                line_color = "black") {

  # data may be an sf object (e.g. project_points) — group_by()/
  # summarise() on a grouped sf tibble automatically unions/carries
  # geometry, which then breaks the left_join() below with a
  # "should not have class sf" error. Dropping geometry upfront keeps
  # the whole pipeline as plain data frames.
  data <- sf::st_drop_geometry(data)

  # year is a numeric/time axis, not a category — a project with a
  # missing completion year has nothing meaningful to plot here, so it's
  # dropped from this specific chart (it can still appear in other
  # charts that don't depend on year). Other xvar values (e.g. a
  # funding category) are categorical — missing values become their
  # own "Unspecified" bucket instead, same reasoning as elsewhere.
  if (identical(xvar, "year")) {
    data <- data %>% dplyr::filter(!is.na(.data[[xvar]]))
  } else {
    data[[xvar]][is.na(data[[xvar]])] <- "Unspecified"
  }

  # this function never breaks down by habitat/technique (only by year
  # or a category like funding sector), so the project COUNT always
  # uses distinct master_project_id — a real-world project spanning
  # multiple rows should still count once. The SUM, though, depends on
  # what's being summed: acres/miles genuinely vary per row (correct
  # to sum raw rows), but cost is repeated identically across a multi-
  # row project's rows — dedupe_sumvar = TRUE dedupes before summing
  # for exactly that case (pass it when sumvar is "cost_dollars").
  toplo <- data %>%
    dplyr::group_by(.data[[xvar]]) %>%
    dplyr::summarise(n_projects = dplyr::n_distinct(master_project_id), .groups = "drop")

  sum_data <- if (dedupe_sumvar) dedupe_projects(data) else data
  sums <- sum_data %>%
    dplyr::group_by(.data[[xvar]]) %>%
    dplyr::summarise(total = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop")

  toplo <- toplo %>% dplyr::left_join(sums, by = xvar)
  toplo$total[is.na(toplo$total)] <- 0

  # categorical axes (e.g. funding sector): Other/Unknown/etc. last
  if (!identical(xvar, "year")) {
    toplo <- toplo[order(catchall_rank(toplo[[xvar]])), ]
  }

  max1 <- max(pretty(c(0, toplo$n_projects)))
  max2 <- max(pretty(c(0, toplo$total)))
  if (!is.finite(max1) || max1 <= 0) max1 <- 1
  if (!is.finite(max2) || max2 <= 0) max2 <- 1

  ticks1 <- seq(0, max1, length.out = n_ticks)
  ticks2 <- seq(0, max2, length.out = n_ticks)

  # explicit hover text with the full, comma-formatted number — plotly
  # can auto-abbreviate large axis tick labels (e.g. "40k"), and without
  # a custom hovertext that abbreviation can carry into the tooltip too
  bar_hover  <- paste0(bar_label, ": ", format(toplo$n_projects, big.mark = ",", scientific = FALSE))
  line_hover <- paste0(line_label, ": ", format(round(toplo$total, 1), big.mark = ",", scientific = FALSE))

  # a flat color everywhere by default (year-axis charts have nothing
  # meaningful to color-by-category); when a named palette IS supplied
  # (e.g. funding_colors), each bar gets its own category's color instead
  bar_colors <- if (!is.null(palette)) {
    cols <- unname(palette[toplo[[xvar]]])
    cols[is.na(cols)] <- "#3190C2"
    cols
  } else {
    "#3190C2"
  }

  plotly::plot_ly(toplo) %>%
    plotly::add_bars(
      x = toplo[[xvar]], y = ~n_projects, name = bar_label,
      marker = list(color = bar_colors),
      text = bar_hover, hoverinfo = "text", textposition = "none"
    ) %>%
    # add_paths() draws the points in the data's own order, matching the
    # bars/axis above — add_lines() would re-sort them by x (alphabetical
    # for a categorical axis), which zig-zags the line as soon as the
    # catch-all categories are moved to the end
    plotly::add_paths(
      x = toplo[[xvar]], y = ~total, name = line_label, yaxis = "y2",
      line = list(color = line_color, width = 3),
      text = line_hover, hoverinfo = "text", textposition = "none"
    ) %>%
    plotly::layout(
      yaxis  = list(title = list(text = bar_label, standoff = 25), range = c(0, max1), tickvals = ticks1, zeroline = FALSE),
      yaxis2 = list(title = list(text = line_label, standoff = 40), overlaying = "y", side = "right",
                     range = c(0, max2), tickvals = ticks2, showgrid = FALSE, zeroline = FALSE),
      # categorical x-axis (e.g. funding sector) gets the same tick
      # rotation direction as the ggplot2-based bar charts elsewhere;
      # a numeric year axis doesn't need it (short, non-overlapping labels)
      # (the category order is pinned explicitly, since plotly can
      # otherwise re-sort a categorical axis on its own)
      xaxis  = if (identical(xvar, "year")) list(title = "") else
        list(title = "", tickangle = -45, categoryorder = "array", categoryarray = as.character(toplo[[xvar]])),
      legend = list(orientation = "h", y = 1.15, yanchor = "bottom"),
      margin = list(r = 60),
      hoverlabel = list(bgcolor = "white")
    )
}

# ---- stacked bar of project counts per x-axis category, broken out by --
# a second category. Defaults to year on the x-axis (existing behavior);
# pass xvar = "region" (or any other column) to group by something else.
#
# Built with ggplot2 (scale_fill_manual + explicit factor levels for
# reliable colors/ordering — plotly's native bar coloring/ordering
# proved unpredictable across several attempts), then converted to a
# plotly widget via ggplotly() to keep hover tooltips. This is different
# from the pie chart situation: pie/donut needed coord_polar, which
# ggplotly's conversion handles poorly (it crashed outright). Plain
# Cartesian bar charts like this one are ggplotly's standard, well-
# supported case, so hover should work reliably here.
#
# Pass dedupe_by_project = TRUE when groupvar is NOT habitat or
# technique — a real-world project can span multiple ROWS (see
# dedupe_projects() in global.R), which inflates both counts and cost
# sums for any breakdown that isn't itself by habitat/technique.
stacked_bar_fun <- function(data, groupvar, xvar = "year", xlab = "", palette = NULL, pct = FALSE, sumvar = NULL, ylab = NULL, dedupe_by_project = FALSE, xrange = NULL) {

  if (dedupe_by_project) {
    data <- dedupe_projects(data)
  }

  # numeric/time x-axis (year): drop rows missing that value entirely,
  # same reasoning as footprint_plot_fun. Categorical x-axis (region,
  # habitat, etc.): missing values instead become their own visible
  # "Unspecified" bucket below, since there's a real bar to draw for
  # "we don't know" as a category — unlike a missing year.
  if (identical(xvar, "year")) {
    data <- data %>% dplyr::filter(!is.na(.data[[xvar]]))
  } else {
    data[[xvar]][is.na(data[[xvar]])] <- "Unspecified"
  }

  # the color-by dimension is always categorical in every current use
  # of this function, so missing values always become "Unspecified"
  # here (kept distinct from any real "Unknown" values already in the
  # data — those stay untouched as their own genuine category).
  data[[groupvar]][is.na(data[[groupvar]])] <- "Unspecified"

  # by default this counts rows per (xvar, groupvar) combination; pass
  # sumvar (e.g. "cost_dollars") to sum that column instead — used for
  # e.g. a 100%-stacked chart of dollars rather than project counts
  if (is.null(sumvar)) {
    toplo <- data %>%
      dplyr::count(.data[[xvar]], .data[[groupvar]], name = "n")
  } else {
    toplo <- data %>%
      dplyr::group_by(.data[[xvar]], .data[[groupvar]]) %>%
      dplyr::summarise(n = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop")
  }

  # alphabetical order (A-Z) for the color-by dimension (with any
  # Other/Unknown/etc. category last), as an explicit factor —
  # ggplot2's scale_fill_manual()/stacking order reliably respects
  # factor levels, unlike whatever plotly was doing natively.
  toplo[[groupvar]] <- factor(toplo[[groupvar]], levels = catchall_last(sort(unique(toplo[[groupvar]]))))

  # for the 100%-stacked variant, also compute each bar's share of its
  # own x-axis group, for the hover text (position="fill" handles the
  # visual proportion automatically, but the raw fraction isn't
  # otherwise exposed anywhere to show in the tooltip)
  if (pct) {
    toplo <- toplo %>%
      dplyr::group_by(.data[[xvar]]) %>%
      dplyr::mutate(share = n / sum(n)) %>%
      dplyr::ungroup()
  }

  if (is.null(sumvar)) {
    toplo$tooltip_text <- if (pct) {
      paste0(toplo[[xvar]], "<br>", toplo[[groupvar]], ": ", round(toplo$share * 100), "% (", toplo$n, " project", ifelse(toplo$n == 1, "", "s"), ")")
    } else {
      paste0(toplo[[xvar]], "<br>", toplo[[groupvar]], ": ", toplo$n, " project", ifelse(toplo$n == 1, "", "s"))
    }
  } else {
    # sum-based (e.g. dollars) — no "project(s)" wording, just the
    # formatted total (and share, for the pct variant)
    dollar_text <- paste0("$", format(round(toplo$n), big.mark = ",", scientific = FALSE))
    toplo$tooltip_text <- if (pct) {
      paste0(toplo[[xvar]], "<br>", toplo[[groupvar]], ": ", round(toplo$share * 100), "% (", dollar_text, ")")
    } else {
      paste0(toplo[[xvar]], "<br>", toplo[[groupvar]], ": ", dollar_text)
    }
  }

  default_ylab <- if (pct) "Proportion of projects" else "Completed projects"
  y_label <- if (!is.null(ylab)) ylab else default_ylab

  gg <- ggplot2::ggplot(
    toplo,
    ggplot2::aes(x = .data[[xvar]], y = n, fill = .data[[groupvar]], text = tooltip_text)
  ) +
    ggplot2::geom_col(position = if (pct) "fill" else "stack", color = "white", linewidth = 0.3) +
    ggplot2::labs(x = xlab, y = y_label, fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)))

  if (identical(xvar, "year")) {
    # force whole-number year labels — ggplot2's automatic breaks can
    # otherwise pick fractional values (e.g. 2017.5) for narrow ranges.
    # When the selected year range (xrange) is given, the axis is pinned
    # to exactly that span (see year_axis()) and the labels are spread
    # evenly across it, so every temporal chart shares the same axis
    # even if this one has no data in some of the selected years.
    if (!is.null(xrange)) {
      gg <- gg +
        ggplot2::scale_x_continuous(breaks = unique(round(seq(xrange[1], xrange[2], length.out = 6)))) +
        ggplot2::coord_cartesian(xlim = c(xrange[1] - 0.5, xrange[2] + 0.5), expand = FALSE)
    } else {
      gg <- gg + ggplot2::scale_x_continuous(
        breaks = function(lims) unique(round(seq(lims[1], lims[2], length.out = 6)))
      )
    }
  } else {
    # x-axis is categorical here — vertical gridlines between categories
    # aren't meaningful, so drop them
    gg <- gg + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
  }

  if (pct) {
    gg <- gg + ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"))
  }

  if (!is.null(palette)) {
    gg <- gg + ggplot2::scale_fill_manual(values = palette)
  }

  plotly::ggplotly(gg, tooltip = "text") %>%
    plotly::layout(
      # vertical legend to the right rather than horizontal-bottom —
      # with many categories (habitat/technique can run ~15-20 items),
      # a single horizontal row forces horizontal scrolling; a tall
      # single column avoids that as long as the chart itself has
      # enough height (see height= in app.R's plotlyOutput calls)
      legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
      margin = list(r = 160),  # reserve room on the right for the legend
      # forced here (not just via ggplot2's theme) since ggplotly()'s
      # conversion doesn't reliably carry over axis.title.y margins
      yaxis = list(title = list(standoff = 25)),
      hoverlabel = list(bgcolor = "white")
    )
}

# ---- pie chart of proportions for a categorical field --------------------
# By default shows the share of PROJECT COUNTS per category. Pass
# sumvar (e.g. "cost_dollars") to instead show the share of a numeric
# total (e.g. dollars) per category.
#
# Returns a STATIC ggplot2 object (not a plotly widget) — rendered via
# shiny::renderPlot()/plotOutput() in app.R, not plotly::renderPlotly().
# Two things led here: (1) plotly's own native pie color handling
# (marker.colors matched to labels) proved unreliable across several
# attempts, with colors landing on the wrong slice depending on data
# order; (2) converting a ggplot2 coord_polar pie to plotly via
# ggplotly() then crashed outright (a real bug in that conversion path,
# not a config issue). ggplot2's own rendering has neither problem, so
# this skips the plotly conversion step entirely. The tradeoff: no more
# hover tooltips on these specific charts — a legend and on-slice
# percentage labels are included to compensate.
donut_fun <- function(data, groupvar, sumvar = NULL, palette = NULL, label_threshold = 0.02, dedupe_by_project = FALSE) {

  if (dedupe_by_project) {
    data <- dedupe_projects(data)
  }

  # categorical breakdown dimension — missing values become their own
  # visible "Unspecified" slice, distinct from any real "Unknown" values
  # already present in the data.
  data[[groupvar]][is.na(data[[groupvar]])] <- "Unspecified"

  if (is.null(sumvar)) {
    toplo <- data %>% dplyr::count(.data[[groupvar]], name = "n")
  } else {
    toplo <- data %>%
      dplyr::group_by(.data[[groupvar]]) %>%
      dplyr::summarise(n = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop")
  }

  toplo <- toplo %>% dplyr::arrange(dplyr::desc(n))
  toplo <- toplo[order(catchall_rank(toplo[[groupvar]])), ]  # Other/Unknown/etc. last
  toplo$fraction <- toplo$n / sum(toplo$n)
  # hide the label for slices below label_threshold so tiny overlapping
  # text doesn't clutter the chart (legend still lists every category
  # regardless of size) — rounded to the nearest whole number
  toplo$pct_label <- ifelse(toplo$fraction >= label_threshold, paste0(round(toplo$fraction * 100), "%"), "")

  # lock in factor levels matching our sort order (scale_fill_manual
  # matches by name regardless, but this keeps legend order consistent
  # too)
  toplo[[groupvar]] <- factor(toplo[[groupvar]], levels = toplo[[groupvar]])

  gg <- ggplot2::ggplot(
    toplo,
    ggplot2::aes(x = "", y = n, fill = .data[[groupvar]])
  ) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.3) +
    ggplot2::geom_text(
      ggplot2::aes(label = pct_label),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white", size = 5.5, fontface = "bold"
    ) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))

  if (!is.null(palette)) {
    gg <- gg + ggplot2::scale_fill_manual(values = palette)
  }

  gg
}

# unlike a generic auto-generated categorical palette (which just
# picks colors for whatever values are present), this uses a FIXED
# named color lookup (e.g. habitat_colors, technique_colors) so the
# map matches the same colors used everywhere else in the dashboard.
# Anything not found in the lookup falls back to a neutral gray rather
# than breaking the map.
named_map_palette_fun <- function(values, lookup, fallback = "#999999") {
  lvls <- catchall_last(sort(unique(values)))
  cols <- unname(lookup[lvls])
  cols[is.na(cols)] <- fallback
  leaflet::colorFactor(palette = cols, domain = values, levels = lvls)
}

# small shared helper: one legend row (a circle swatch + label), used
# by quartile_size_legend_fun() below
size_legend_row <- function(r, label) {
  sprintf(
    '<div style="display:flex; align-items:center; margin-bottom:4px;">
       <div style="width:%.1fpx; height:%.1fpx; border-radius:50%%; background:#3182bd; opacity:0.65; border:1px solid #1f5a8a; margin-right:8px; flex-shrink:0;"></div>
       <span style="font-size:14px; color:#333;">%s</span>
     </div>',
    r * 2, r * 2, label
  )
}

# ---- quartile-based point radius scaling ---------------------------------
# splits `values` into 4 bins by ITS OWN quartiles (computed from the
# real, currently-filtered data — not a fixed pre-defined category or
# a continuous min-max scale), assigning each bin a fixed radius.
# Returns a list: radius_fun(x) maps a raw value to its bin's radius,
# plus the breakpoints themselves (for building a matching "$X - $Y"
# range legend via quartile_size_legend_fun() below).
#
# Falls back to fewer bins if the data has heavy ties (e.g. many
# identical values collapsing multiple quantile breakpoints together) —
# cut() requires strictly increasing breaks, so duplicates are dropped
# and the largest radii are kept (biggest values still read as "big").
quartile_radius_fun <- function(values, radii = c(4, 7, 10, 14)) {
  values <- values[!is.na(values)]
  breaks <- stats::quantile(values, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, type = 7)
  breaks <- unique(breaks)

  n_bins <- length(breaks) - 1
  radii <- utils::tail(radii, n_bins)

  radius_fun <- function(x) {
    bin <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    radii[bin]
  }

  list(radius_fun = radius_fun, breaks = breaks, radii = radii)
}

# companion legend for quartile_radius_fun() — shows each bin's actual
# "$X - $Y" (or "X - Y acres", etc.) range rather than a single value,
# since a lone number doesn't convey what a quartile bin actually
# spans. Largest bin shown first, matching the other size legends'
# largest-first convention.
quartile_size_legend_fun <- function(qr, title = "", prefix = "", suffix = "", digits = 0) {

  n_bins <- length(qr$breaks) - 1
  bin_order <- n_bins:1  # largest bin first

  fmt <- function(v) paste0(prefix, format(round(v, digits), big.mark = ",", trim = TRUE, nsmall = digits, scientific = FALSE), suffix)

  rows <- mapply(function(i, r) {
    label <- if (i == n_bins) {
      paste0(fmt(qr$breaks[i]), "+")
    } else if (i == 1) {
      paste0("&lt; ", fmt(qr$breaks[i + 1]))
    } else {
      paste0(fmt(qr$breaks[i]), " - ", fmt(qr$breaks[i + 1]))
    }
    size_legend_row(r, label)
  }, bin_order, qr$radii[bin_order], SIMPLIFY = TRUE)

  htmltools::HTML(paste0(
    '<div style="background:rgba(255,255,255,0.85); padding:6px 10px; border-radius:4px; box-shadow:0 1px 4px rgba(0,0,0,0.3); line-height:1.2;">',
    if (nzchar(title)) paste0('<div style="font-weight:600; font-size:13px; margin-bottom:6px;">', title, '</div>') else "",
    paste(rows, collapse = ""),
    '</div>'
  ))
}

# ---- aggregate filtered point data into per-region/county summary -------
# stats, ready to feed into choropleth_fun(). Reused for both Regions
# (group_field="region") and Counties (group_field="county").
#
# multi_value=TRUE explodes a comma-separated field so a project counts
# toward EACH value it lists. The final database gives every project a
# single primary county, so this is a no-op for the data as it stands
# now — it's kept (and still used for Counties) so that a points file
# listing several counties in one cell would still be handled correctly
# instead of silently matching no county polygon.
#
# Sequencing matters here, especially for cost: dedupe_projects() always
# runs BEFORE explode_multi() (never after) — a multi-row project's
# cost is identical across its rows, so deduping first avoids inflating
# the total; only THEN does exploding (when it applies) attribute that
# one deduped project's cost to every value it lists, matching the same
# accepted tradeoff already used for the Restoration Technique Funding
# chart. Acres/miles skip the dedupe step entirely, since those
# genuinely vary per row already.
polygon_summary_fun <- function(data, group_field, metric, multi_value = FALSE) {

  # mv_data() returns an sf object — dplyr::summarise() on a GROUPED sf
  # tibble automatically unions all the point geometries per group by
  # default, which is wasted computation here (we only want the plain
  # numeric summary). Dropping geometry upfront keeps the whole
  # pipeline as plain data frames.
  data <- sf::st_drop_geometry(data)

  if (metric == "Restoration costs") {
    data <- dedupe_projects(data)
  }

  if (multi_value) {
    data <- explode_multi(data, group_field)
  }

  if (metric == "Restoration projects") {
    data %>%
      dplyr::group_by(.data[[group_field]]) %>%
      dplyr::summarise(value = dplyr::n_distinct(master_project_id), .groups = "drop")
  } else if (metric == "Restoration footprint (area)") {
    data %>%
      dplyr::group_by(.data[[group_field]]) %>%
      dplyr::summarise(value = sum(area_acres, na.rm = TRUE), .groups = "drop")
  } else if (metric == "Restoration footprint (length)") {
    data %>%
      dplyr::group_by(.data[[group_field]]) %>%
      dplyr::summarise(value = sum(length_miles, na.rm = TRUE), .groups = "drop")
  } else if (metric == "Restoration costs") {
    data %>%
      dplyr::group_by(.data[[group_field]]) %>%
      dplyr::summarise(value = sum(cost_dollars, na.rm = TRUE), .groups = "drop")
  } else {
    data %>% dplyr::group_by(.data[[group_field]]) %>% dplyr::summarise(value = 0, .groups = "drop")
  }
}

# ---- join aggregated project data onto a boundary polygon layer ---------
# `bounds`      an sf polygon object (e.g. region_bounds or county_bounds)
# `name_field`  the attribute column in `bounds` holding the region/county name
# `agg_df`      a data frame with one row per region/county and a value column
# `join_field`  the column in `agg_df` matching `name_field` (e.g. "region")
# `value_field` the numeric column in `agg_df` to map onto fill color
#
# Returns NULL if `bounds` is missing or `name_field` doesn't exist, so
# calling code can fall back gracefully instead of erroring.
choropleth_fun <- function(bounds, name_field, agg_df, join_field, value_field) {

  if (is.null(bounds) || !name_field %in% names(bounds)) return(NULL)

  bounds$.joinkey <- as.character(bounds[[name_field]])
  agg_df$.joinkey <- as.character(agg_df[[join_field]])
  agg_df$.joinval <- agg_df[[value_field]]

  out <- dplyr::left_join(
    bounds,
    sf::st_drop_geometry(agg_df)[, c(".joinkey", ".joinval")],
    by = ".joinkey"
  )

  out
}

# ---- simple count-by-category bar chart (single series, no stacking) ---
# Used for Database Overview's data-quality/characteristic charts (Type
# of Activity, Project Status, Estuarine Boundary, Temporal Record,
# Project Size, Source(s)) — each just counts projects by ONE category,
# with no second breakdown dimension.
#
# Pass explode_col to first split a multi-value field (e.g. "source")
# so a project contributes to each value it lists, rather than being
# counted once under a combined string — matches how Database Overview's
# own description explains the Source(s) chart should behave.
#
# Missing values are recoded to "Unknown" (not "Unspecified" — matching
# the exact language Database Overview's own descriptions use for
# missing/non-NEP-sourced data).
#
# Pass dedupe_by_project = TRUE when xvar is NOT habitat or technique —
# a single real-world project can span multiple ROWS (see
# dedupe_projects() in global.R), which would otherwise inflate counts
# for any breakdown that isn't itself by habitat/technique. Dedup
# happens BEFORE exploding, so a multi-value field (e.g. source) still
# fans out correctly from that one representative row.
category_count_fun <- function(data, xvar, xlab = "", explode_col = NULL, fill_color = "#4c8bf5", palette = NULL, angle_labels = TRUE, dedupe_by_project = FALSE) {

  if (dedupe_by_project) {
    data <- dedupe_projects(data)
  }

  if (!is.null(explode_col)) {
    data <- explode_multi(data, explode_col)
  }

  data[[xvar]][is.na(data[[xvar]])] <- "Unknown"

  toplo <- data %>% dplyr::count(.data[[xvar]], name = "n")
  toplo <- toplo %>% dplyr::arrange(dplyr::desc(n))
  toplo <- toplo[order(catchall_rank(toplo[[xvar]])), ]  # Other/Unknown/etc. last
  toplo[[xvar]] <- factor(toplo[[xvar]], levels = toplo[[xvar]])

  toplo$tooltip_text <- paste0(
    toplo[[xvar]], ": ", toplo$n, " project", ifelse(toplo$n == 1, "", "s")
  )

  x_axis_theme <- if (angle_labels) {
    ggplot2::element_text(angle = 45, hjust = 1)
  } else {
    ggplot2::element_text(angle = 0, hjust = 0.5)
  }

  # with a palette, color each bar by its own category (and drop the
  # legend, since the x-axis already labels each bar — a legend would
  # just repeat the same information); without one, every bar is the
  # same flat fill_color as before.
  if (!is.null(palette)) {
    gg <- ggplot2::ggplot(toplo, ggplot2::aes(x = .data[[xvar]], y = n, fill = .data[[xvar]], text = tooltip_text)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = palette) +
      ggplot2::labs(x = xlab, y = "Number of projects") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none", axis.text.x = x_axis_theme,
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
        panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank()
      )
  } else {
    gg <- ggplot2::ggplot(toplo, ggplot2::aes(x = .data[[xvar]], y = n, text = tooltip_text)) +
      ggplot2::geom_col(fill = fill_color) +
      ggplot2::labs(x = xlab, y = "Number of projects") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = x_axis_theme,
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
        panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank()
      )
  }

  # standoff forced here too (not just via ggplot2's theme) since
  # ggplotly()'s conversion doesn't reliably carry over axis.title.y
  # margins
  plotly::ggplotly(gg, tooltip = "text") %>%
    plotly::layout(yaxis = list(title = list(standoff = 25)), hoverlabel = list(bgcolor = "white"))
}

# ---- single-series bar of a numeric total per category, colored by a --
# fixed palette (e.g. habitat_colors) — used for "Area Restored"/"Miles
# Restored" style charts (x = habitat, y = sum of acres/miles). This is
# a separate function from sum_bar_fun (rather than extending it)
# specifically so it doesn't change how sum_bar_fun's existing callers
# render — this one is ggplot2-based with per-bar coloring + legend
# suppressed (redundant with the x-axis labels), sum_bar_fun stays
# native plotly with a single flat color.
habitat_metric_bar_fun <- function(data, xvar, sumvar, ylab = "", palette = NULL, hover_prefix = "", hover_suffix = "") {

  data[[xvar]][is.na(data[[xvar]])] <- "Unspecified"

  toplo <- data %>%
    dplyr::group_by(.data[[xvar]]) %>%
    dplyr::summarise(total = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(total > 0) %>%
    dplyr::arrange(dplyr::desc(total))

  toplo <- toplo[order(catchall_rank(toplo[[xvar]])), ]  # Other/Unknown/etc. last
  toplo[[xvar]] <- factor(toplo[[xvar]], levels = toplo[[xvar]])
  toplo$tooltip_text <- paste0(
    toplo[[xvar]], ": ", hover_prefix, format(round(toplo$total, 1), big.mark = ",", scientific = FALSE), hover_suffix
  )

  gg <- ggplot2::ggplot(
    toplo,
    ggplot2::aes(x = .data[[xvar]], y = total, fill = .data[[xvar]], text = tooltip_text)
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "", y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )

  if (!is.null(palette)) {
    gg <- gg + ggplot2::scale_fill_manual(values = palette)
  }

  # force k/M-notation on the axis (e.g. "40k", "2.5M") instead of
  # ggplot2's default scientific notation for large numbers (e.g.
  # "4e+04") — the hover tooltip is untouched, since it uses the
  # full-precision tooltip_text built above, not these axis labels
  gg <- gg + ggplot2::scale_y_continuous(
    labels = function(x) {
      vapply(x, function(v) {
        if (is.na(v)) return(NA_character_)
        av <- abs(v)
        if (av >= 1e6) {
          m <- v / 1e6
          if (m == round(m)) paste0(round(m), "M") else paste0(round(m, 1), "M")
        } else if (av >= 1000) {
          paste0(round(v / 1000), "k")
        } else {
          format(v, scientific = FALSE)
        }
      }, character(1))
    }
  )

  plotly::ggplotly(gg, tooltip = "text") %>%
    plotly::layout(yaxis = list(title = list(standoff = 25)), hoverlabel = list(bgcolor = "white"))
}

# ---- cumulative dual-axis line chart -------------------------------------
# Unlike footprint_plot_fun (per-year bar + per-year line), this shows
# RUNNING/cumulative totals over time as two lines — cumulative project
# count (left axis) and a cumulative numeric total, e.g. acres or miles
# (right axis). Same "match tick counts across both axes" trick as
# footprint_plot_fun so the gridlines from both axes line up.
#
# cum_n always counts DISTINCT master_project_id (this function never
# breaks down by habitat/technique, only by year) — a real-world
# project spanning multiple rows should still count once. Pass
# dedupe_sumvar = TRUE if sumvar is ever cost-like (repeated per row
# within a multi-row project) rather than acres/miles (which genuinely
# vary per row and are fine to sum as-is).
cumulative_line_fun <- function(data, sumvar, line_label, n_ticks = 6, dedupe_sumvar = FALSE, xrange = NULL) {

  # see footprint_plot_fun() above for why this matters — group_by()/
  # summarise() on a grouped sf tibble carries geometry, which then
  # breaks the left_join() below with a spatial-join error
  data <- sf::st_drop_geometry(data)

  # both the year AND the metric need to be present for a meaningful
  # cumulative total — rows missing either are dropped entirely
  data <- data %>% dplyr::filter(!is.na(year), !is.na(.data[[sumvar]]))

  toplo <- data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n = dplyr::n_distinct(master_project_id), .groups = "drop") %>%
    dplyr::arrange(year)

  sum_data <- if (dedupe_sumvar) dedupe_projects(data) else data
  sums <- sum_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop")

  toplo <- toplo %>% dplyr::left_join(sums, by = "year")
  toplo$total[is.na(toplo$total)] <- 0

  toplo$cum_n <- cumsum(toplo$n)
  toplo$cum_total <- cumsum(toplo$total)

  max1 <- max(pretty(c(0, toplo$cum_n)))
  max2 <- max(pretty(c(0, toplo$cum_total)))
  if (!is.finite(max1) || max1 <= 0) max1 <- 1
  if (!is.finite(max2) || max2 <= 0) max2 <- 1
  ticks1 <- seq(0, max1, length.out = n_ticks)
  ticks2 <- seq(0, max2, length.out = n_ticks)

  n_hover     <- paste0("Cumulative number of projects: ", format(toplo$cum_n, big.mark = ",", scientific = FALSE))
  total_hover <- paste0(line_label, ": ", format(round(toplo$cum_total, 1), big.mark = ",", scientific = FALSE))

  plotly::plot_ly(toplo) %>%
    plotly::add_lines(
      x = ~year, y = ~cum_n, name = "Cumulative number of projects",
      line = list(color = "#3190C2", width = 3),
      text = n_hover, hoverinfo = "text", textposition = "none"
    ) %>%
    plotly::add_lines(
      x = ~year, y = ~cum_total, name = line_label, yaxis = "y2",
      line = list(color = "black", width = 3),
      text = total_hover, hoverinfo = "text", textposition = "none"
    ) %>%
    plotly::layout(
      yaxis  = list(title = list(text = "Cumulative number of projects", standoff = 25), range = c(0, max1), tickvals = ticks1, zeroline = FALSE),
      yaxis2 = list(title = list(text = line_label, standoff = 40), overlaying = "y", side = "right",
                     range = c(0, max2), tickvals = ticks2, showgrid = FALSE, zeroline = FALSE),
      xaxis  = year_axis(xrange),
      legend = list(orientation = "h", y = 1.15, yanchor = "bottom"),
      margin = list(r = 60),
      hoverlabel = list(bgcolor = "white")
    )
}

# ---- per-year total + cumulative total, same metric ----------------------
# Unlike footprint_plot_fun (bar = project COUNT, line = per-year sum of
# a different metric) or cumulative_line_fun (two cumulative lines of
# two different metrics), this is bar = per-year SUM of one metric
# (e.g. cost), line = the RUNNING/cumulative version of that SAME
# metric. Rows missing year OR the metric itself are dropped entirely
# (both are required for a meaningful total here).
#
# Always dedupes to one row per master_project_id before summing — this
# function is inherently a cost-over-time chart with no habitat/
# technique breakdown, and cost is repeated identically across a multi-
# row project's rows, so summing raw rows would multiply-count it.
cost_trend_fun <- function(data, sumvar = "cost_dollars",
                             bar_label = "Reported project cost ($)",
                             line_label = "Cumulative reported costs ($)",
                             n_ticks = 6, xrange = NULL) {

  # data may be an sf object (e.g. project_points) — group_by()/
  # summarise() on a grouped sf tibble automatically unions/carries
  # geometry, which is wasted computation here since only the plain
  # numeric summary is needed
  data <- sf::st_drop_geometry(data)

  data <- data %>% dplyr::filter(!is.na(year), !is.na(.data[[sumvar]])) %>% dedupe_projects()

  toplo <- data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(year)

  toplo$cum_total <- cumsum(toplo$total)

  max1 <- max(pretty(c(0, toplo$total)))
  max2 <- max(pretty(c(0, toplo$cum_total)))
  if (!is.finite(max1) || max1 <= 0) max1 <- 1
  if (!is.finite(max2) || max2 <= 0) max2 <- 1
  ticks1 <- seq(0, max1, length.out = n_ticks)
  ticks2 <- seq(0, max2, length.out = n_ticks)

  bar_hover  <- paste0(bar_label, ": $", format(round(toplo$total), big.mark = ",", scientific = FALSE))
  line_hover <- paste0(line_label, ": $", format(round(toplo$cum_total), big.mark = ",", scientific = FALSE))

  plotly::plot_ly(toplo) %>%
    plotly::add_bars(
      x = ~year, y = ~total, name = bar_label,
      marker = list(color = "#3190C2"),
      text = bar_hover, hoverinfo = "text", textposition = "none"
    ) %>%
    plotly::add_lines(
      x = ~year, y = ~cum_total, name = line_label, yaxis = "y2",
      line = list(color = "black", width = 3),
      text = line_hover, hoverinfo = "text", textposition = "none"
    ) %>%
    plotly::layout(
      yaxis  = list(title = list(text = bar_label, standoff = 25), range = c(0, max1), tickvals = ticks1, zeroline = FALSE),
      yaxis2 = list(title = list(text = line_label, standoff = 40), overlaying = "y", side = "right",
                     range = c(0, max2), tickvals = ticks2, showgrid = FALSE, zeroline = FALSE),
      xaxis  = year_axis(xrange),
      legend = list(orientation = "h", y = 1.15, yanchor = "bottom"),
      margin = list(r = 60),
      hoverlabel = list(bgcolor = "white")
    )
}

# ---- alluvial (flow) diagram ----------------------------------------------
# Shows how a numeric total (e.g. cost) "flows" from a source category
# (e.g. funding sector) to a destination category (e.g. habitat type).
# Uses the ggalluvial package (a real, purpose-built ggplot2 extension
# for exactly this kind of diagram) — a NEW dependency, install via
# install.packages("ggalluvial").
#
# Returns a STATIC ggplot2 object (not converted via ggplotly()) — after
# the pie chart experience, alluvial/Sankey-style geoms are exactly the
# kind of non-Cartesian layout ggplotly's conversion tends to handle
# poorly, so this skips that risk entirely from the start rather than
# discovering it the hard way again.
# small helper for alluvial_fun: wraps each string onto multiple lines
# at word boundaries (base R strwrap — no new dependency), so long
# category names like "Submerged aquatic vegetation" wrap within their
# stratum box instead of overflowing past its edges. vapply (not a
# bare strwrap() call) keeps each input string's wrapped lines properly
# joined and aligned one-to-one with the input vector — plain strwrap()
# on a multi-element vector flattens all lines together and loses which
# line belongs to which original string.
#
# strwrap() only breaks at actual spaces between words — a name with
# no spaces at all (e.g. "Tidal tributary/creek/river", "Mixed-
# partner") is treated as one unbreakable "word" and won't wrap no
# matter how long it is. Inserting a space right after "/" and "-"
# gives strwrap a valid place to break while keeping the punctuation
# itself visible in the output — this is a general fix (any future
# category name with a slash/hyphen benefits too), not a one-off patch
# for just these two names.
wrap_label <- function(x, width = 12, overrides = NULL) {
  # ggalluvial's stat likely returns stratum as a FACTOR — critical to
  # convert to character FIRST, before any ifelse() below. ifelse()
  # silently converts an unmatched factor branch to its underlying
  # INTEGER codes rather than its text labels when mixed with a
  # character replacement value, which is exactly what caused every
  # non-overridden label to render as a number.
  x <- as.character(x)

  # swap in a shorter display name for specific labels before wrapping
  # (e.g. a long habitat name that wraps awkwardly) — this only affects
  # what's SHOWN on this chart, not the underlying category data used
  # everywhere else (filters, other charts, the hotspot table, etc.)
  if (!is.null(overrides)) {
    matched <- match(x, names(overrides))
    x <- ifelse(!is.na(matched), overrides[matched], x)
  }
  x <- gsub("([/-])(?!\\s)", "\\1 ", x, perl = TRUE)
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1), USE.NAMES = FALSE)
}

alluvial_fun <- function(data, source_var, target_var, sumvar,
                          source_label = source_var, target_label = target_var,
                          palette = NULL, habitat_gray_color = "grey70",
                          min_share = 0.05,
                          label_overrides = c(
                            "Tidal tributary/creek/river" = "Tidal tributary",
                            "Submerged aquatic vegetation" = "SAV"
                          )) {

  data[[source_var]][is.na(data[[source_var]])] <- "Unspecified"
  data[[target_var]][is.na(data[[target_var]])] <- "Unspecified"

  toplo_full <- data %>% dplyr::filter(!is.na(.data[[sumvar]]))
  grand_total <- sum(toplo_full[[sumvar]], na.rm = TRUE)

  # drop any source/target category below min_share of the overall
  # total (relative to the data actually passed in — i.e. whatever the
  # dashboard's current filters already narrowed things down to, not
  # the whole unfiltered database). This is what was causing tiny,
  # tightly-packed strata whose labels collided with their neighbors —
  # removing the small slivers means the remaining boxes are naturally
  # big enough to hold their own label, so no positioning tricks
  # (outside labels, hjust, check_overlap) are needed anymore.
  source_keep <- toplo_full %>%
    dplyr::group_by(.data[[source_var]]) %>%
    dplyr::summarise(tot = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(tot / grand_total >= min_share) %>%
    dplyr::pull(.data[[source_var]])

  target_keep <- toplo_full %>%
    dplyr::group_by(.data[[target_var]]) %>%
    dplyr::summarise(tot = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(tot / grand_total >= min_share) %>%
    dplyr::pull(.data[[target_var]])

  toplo <- toplo_full %>%
    dplyr::filter(.data[[source_var]] %in% source_keep, .data[[target_var]] %in% target_keep) %>%
    dplyr::group_by(.data[[source_var]], .data[[target_var]]) %>%
    dplyr::summarise(total = sum(.data[[sumvar]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(total > 0)

  # give source and target values distinct internal identities before
  # plotting, even when the SAME text (e.g. "Unknown") appears on both
  # sides — a named color palette can't tell apart two entries sharing
  # one name, so without this, ggplot2's fill scale collapses them
  # into a single color (this is exactly what caused "Unknown" habitat
  # to render with the same color as "Unknown" funding sector). The
  # prefix is stripped back off for the displayed label text below, so
  # it's invisible to the viewer — it only prevents the color collision.
  toplo[[source_var]] <- paste0("SRC:::", toplo[[source_var]])
  toplo[[target_var]] <- paste0("TGT:::", toplo[[target_var]])

  # sector (source) strata get distinct colors, matching their own
  # alluvium flow color; habitat (target) strata are all plain gray.
  # Combined into ONE named palette, since ggplot2 needs a single fill
  # scale covering both geom_alluvium's fill (source values only) and
  # geom_stratum's fill (source AND target values) at once.
  # alphabetical, with Other/Unknown/etc. last on both sides (the
  # internal prefix has to be stripped to recognize those labels).
  # Explicit factor levels are what ggalluvial uses to stack the strata.
  by_catchall <- function(x) x[order(catchall_rank(sub("^(SRC|TGT):::", "", x)))]
  source_levels <- by_catchall(sort(unique(toplo[[source_var]])))
  target_levels <- by_catchall(sort(unique(toplo[[target_var]])))
  toplo[[source_var]] <- factor(toplo[[source_var]], levels = source_levels)
  toplo[[target_var]] <- factor(toplo[[target_var]], levels = target_levels)

  if (!is.null(palette)) {
    # a custom palette is assumed to be keyed to the RAW (unprefixed)
    # names — re-key it to match our prefixed internal values
    sector_colors <- stats::setNames(palette[sub("^SRC:::", "", source_levels)], source_levels)
  } else {
    # reproduces ggplot2's own default discrete hue palette (evenly
    # spaced hues) so sector colors match what geom_alluvium would
    # have picked automatically anyway
    sector_colors <- stats::setNames(
      grDevices::hcl(h = seq(15, 375, length.out = length(source_levels) + 1), c = 100, l = 65)[seq_along(source_levels)],
      source_levels
    )
  }
  habitat_fill <- stats::setNames(rep(habitat_gray_color, length(target_levels)), target_levels)
  combined_palette <- c(sector_colors, habitat_fill)

  gg <- ggplot2::ggplot(
    toplo,
    ggplot2::aes(axis1 = .data[[source_var]], axis2 = .data[[target_var]], y = total)
  ) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data[[source_var]]), width = 1/8, alpha = 0.75) +
    ggalluvial::geom_stratum(ggplot2::aes(fill = ggplot2::after_stat(stratum)), width = 1/8, color = "white") +
    ggplot2::geom_text(
      stat = ggalluvial::StatStratum,
      ggplot2::aes(
        label = ggplot2::after_stat(wrap_label(sub("^(SRC|TGT):::", "", stratum), overrides = label_overrides)),
        # funder-side (source) strata now use much darker fills post-
        # accessibility-fix, so their labels need white text; habitat-
        # side (target) strata stay on a fixed light gray (grey70),
        # where the existing black text is still perfectly readable
        color = ggplot2::after_stat(ifelse(grepl("^SRC:::", stratum), "white", "black"))
      ),
      size = 4, fontface = "bold", lineheight = 0.8
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_manual(values = combined_palette) +
    ggplot2::scale_x_discrete(limits = c(source_label, target_label), expand = c(.12, .12)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  gg
}

# ---- cross-tab "hotspot" heatmap table -----------------------------------
# row_var (e.g. "technique") down the left, col_var (e.g. "habitat")
# across the top, cell = count of projects with that combination,
# colored on a gradient between the table's own overall min and max.
# Built with reactable (already used in Download Data) — cell
# background color is set via a colDef style function per column.
#
# Rows/columns are sorted by their own total (descending, biggest
# first) rather than alphabetically, so the busiest combinations cluster
# toward the top-left and the pattern is visible at a glance (except
# Other/Unknown/etc., which always go last). True-zero
# combinations render as a blank cell (no number, no fill) rather than
# a colored "0" — the color scale itself is based on the smallest
# NON-zero value, so a "1" still gets the lightest fill on the scale
# rather than looking identical to an empty cell.
hotspot_table_fun <- function(data, row_var, col_var,
                                low_color = "#f7fcf0", mid_color = "#7fcdbb", high_color = "#0c2c84") {

  data[[row_var]][is.na(data[[row_var]])] <- "Unspecified"
  data[[col_var]][is.na(data[[col_var]])] <- "Unspecified"

  counts <- data %>% dplyr::count(.data[[row_var]], .data[[col_var]], name = "n")

  # sort by each dimension's OWN total, descending — not alphabetical
  row_vals <- counts %>%
    dplyr::group_by(.data[[row_var]]) %>%
    dplyr::summarise(tot = sum(n), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tot)) %>%
    dplyr::pull(.data[[row_var]])
  col_vals <- counts %>%
    dplyr::group_by(.data[[col_var]]) %>%
    dplyr::summarise(tot = sum(n), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tot)) %>%
    dplyr::pull(.data[[col_var]])

  # Other/Unknown/etc. always go last, whatever their totals
  row_vals <- catchall_last(row_vals)
  col_vals <- catchall_last(col_vals)

  # fill in every row/column combination, including the ones with zero
  # projects, so the table has a consistent rectangular shape
  full_grid <- expand.grid(row = row_vals, col = col_vals, stringsAsFactors = FALSE)
  names(full_grid) <- c(row_var, col_var)
  full_grid <- full_grid %>%
    dplyr::left_join(counts, by = c(row_var, col_var)) %>%
    dplyr::mutate(n = ifelse(is.na(n), 0, n))

  # factor levels (in our sorted order) so xtabs' resulting dimnames —
  # and therefore the final table's row/column order — respect it,
  # rather than xtabs' own default alphabetical ordering
  full_grid[[row_var]] <- factor(full_grid[[row_var]], levels = row_vals)
  full_grid[[col_var]] <- factor(full_grid[[col_var]], levels = col_vals)

  tab <- stats::xtabs(stats::as.formula(paste0("n ~ ", row_var, " + ", col_var)), data = full_grid)
  mat <- as.data.frame.matrix(tab)
  mat <- mat[row_vals, col_vals, drop = FALSE]  # enforce sorted order explicitly
  mat <- cbind(setNames(data.frame(rownames(mat), stringsAsFactors = FALSE), row_var), mat)
  rownames(mat) <- NULL

  value_cols <- setdiff(names(mat), row_var)
  all_vals <- unlist(mat[value_cols])
  nonzero_vals <- all_vals[all_vals > 0]
  val_min <- if (length(nonzero_vals) > 0) min(nonzero_vals, na.rm = TRUE) else 0
  val_max <- max(all_vals, na.rm = TRUE)

  cell_palette <- grDevices::colorRampPalette(c(low_color, mid_color, high_color))(101)

  cell_color <- function(value) {
    if (is.na(value) || value == 0 || val_max == val_min) return("#ffffff")
    pct <- (value - val_min) / (val_max - val_min)
    cell_palette[round(pct * 100) + 1]
  }

  # angled column headers, matching the reference layout — reactable
  # doesn't rotate headers natively, so this is a small custom header
  # renderer per column. Long names WRAP onto extra lines within a fixed
  # width (rather than running out one long line and getting cut off at
  # the top of the table). Names with few spaces to wrap at, like "Tidal
  # tributary/creek/river", can also break after each "/" — via an
  # invisible zero-width space (plain text, since reactable escapes raw
  # HTML in headers) — except in "and/or", which stays together.
  #
  # reactable's OWN header row container clips anything taller than a
  # normal single line of text via overflow/height on ITS OWN wrapper
  # (not this div) — see the matching CSS in styles.css
  # (.hotspot-table) for the other half of this fix, including a header
  # height tall enough for the wrapped, rotated block.
  angled_header <- function(name) {
    label <- gsub("/(?!or\\b)", "/\u200B", name, perl = TRUE)
    htmltools::div(
      style = paste0(
        "transform: rotate(-35deg); transform-origin: left bottom; ",
        "width: 115px; white-space: normal; text-align: left; ",
        "line-height: 1.15; font-size: 0.75rem; margin-top: 10px; margin-left: 20px;"
      ),
      label
    )
  }

  col_defs <- stats::setNames(
    lapply(value_cols, function(cn) {
      reactable::colDef(
        header = angled_header(cn),
        cell = function(value) if (is.na(value) || value == 0) "" else format(value, big.mark = ",", scientific = FALSE),
        style = function(value) {
          list(
            background = cell_color(value),
            color = if (!is.na(value) && value > 0 && val_max > val_min && (value - val_min) / (val_max - val_min) > 0.55) "white" else "#333"
          )
        },
        minWidth = 80
      )
    }),
    value_cols
  )

  # row-label column (technique/habitat names) needs real room too —
  # reactable's default column width was truncating longer names like
  # "Hydrologic restoration"
  col_defs[[row_var]] <- reactable::colDef(
    name = paste0(toupper(substring(row_var, 1, 1)), substring(row_var, 2)),
    minWidth = 190, sticky = "left", align = "left"
  )

  reactable::reactable(
    mat,
    columns = col_defs,
    defaultColDef = reactable::colDef(align = "center"),
    bordered = FALSE,
    highlight = TRUE,
    resizable = TRUE,
    wrap = FALSE,
    pagination = FALSE,
    class = "hotspot-table",
    style = list(fontSize = "0.85rem")
  )
}

# ---- companion color-scale legend for hotspot_table_fun ------------------
# reactable has no built-in colorbar legend, so this builds a small CSS
# gradient bar with min/max labels to place alongside/above the table.
# Recomputes the same min/max hotspot_table_fun uses internally — pass
# it the exact same data/row_var/col_var/colors so the two stay in sync.
hotspot_legend_fun <- function(data, row_var, col_var, legend_label = "Number of projects",
                                 low_color = "#f7fcf0", mid_color = "#7fcdbb", high_color = "#0c2c84") {

  data[[row_var]][is.na(data[[row_var]])] <- "Unspecified"
  data[[col_var]][is.na(data[[col_var]])] <- "Unspecified"

  counts <- data %>% dplyr::count(.data[[row_var]], .data[[col_var]], name = "n")
  nonzero_vals <- counts$n[counts$n > 0]
  val_min <- if (length(nonzero_vals) > 0) min(nonzero_vals, na.rm = TRUE) else 0
  val_max <- if (nrow(counts) > 0) max(counts$n, na.rm = TRUE) else 0

  htmltools::div(
    style = "display: flex; align-items: center; gap: 0.5rem; font-size: 0.8rem; color: #444; margin-bottom: 0.5rem;",
    htmltools::span(legend_label, style = "font-weight: 600;"),
    htmltools::span(val_min),
    htmltools::div(style = paste0(
      "width: 160px; height: 14px; border-radius: 3px; background: linear-gradient(to right, ",
      low_color, ", ", mid_color, ", ", high_color, ");"
    )),
    htmltools::span(val_max)
  )
}
