library(shiny)
library(bslib)
library(dplyr)

# source data/helpers
source(here::here('R/global.R'))
source(here::here('R/funcs.R'))

# a single feature card for the About > Using the Dashboard grid.
# `icon_name` is any Bootstrap Icons name (browse them at
# https://icons.getbootstrap.com) — passed straight to bsicons::bs_icon().
# `button_id` is the actionButton's input id (handled in the server via
# updateNavbarPage), and `target_value` is the value of the nav_panel
# tab it should switch to (e.g. "data-view", must match that panel's
# own `value = ...`).
feature_card <- function(icon_name, title, text, accent_class, button_id, target_value) {
  div(
    class = paste("feature-card", accent_class),
    div(class = "feature-icon", bsicons::bs_icon(icon_name, size = "1.6rem")),
    h4(title),
    p(text),
    actionButton(button_id, "Open", class = "btn-sm feature-open-btn")
  )
}

# the banner shown at the top of the About subtabs and Contribute.
# Centralized here so the subtitle wording stays consistent everywhere
# it's used — change it once and it updates on every tab that calls it.
fer_banner <- function(subtitle = "Interactive Dashboard") {
  div(
    class = 'fer-banner',
    h1("Florida Estuarine Restoration Database"),
    div(class = 'subtitle', subtitle)
  )
}

# a titled row of partner/funder logos. `logos` is a plain character
# vector of filenames living in www/ (e.g. "logo_gei.png") — the row
# wraps to additional lines automatically via CSS flex-wrap as the
# window narrows, so no fixed row/column count needs to be set here.
logo_section <- function(title, logos) {
  div(
    h4(title, class = 'logo-section-title'),
    div(
      class = 'logo-row',
      lapply(logos, function(f) tags$img(src = f, class = 'partner-logo'))
    )
  )
}

# bolds the first sentence of a plain-text description, leaving the
# rest as-is — used by chart_card() below so every chart's description
# gets this treatment automatically, rather than hand-editing ~30
# individual description strings across the app
bold_first_sentence <- function(text) {
  # find the first sentence-ending punctuation followed by a space (or
  # the end of the string, if there's only one sentence)
  m <- regexpr("^.*?[.!?](?=\\s|$)", text, perl = TRUE)
  if (m[1] == -1) {
    return(shiny::HTML(paste0("<b>", text, "</b>")))
  }
  split_at <- attr(m, "match.length")
  first_sentence <- substr(text, 1, split_at)
  rest <- substr(text, split_at + 1, nchar(text))
  shiny::HTML(paste0("<b>", first_sentence, "</b>", rest))
}

# a chart card: title, a short description underneath it, then the plot
# itself. `output` is the plotlyOutput()/plotOutput() call for that chart.
chart_card <- function(title, description, output) {
  card(
    card_header(title),
    div(class = 'chart-desc', bold_first_sentence(description)),
    output
  )
}

# the "Reset selection(s)" button at the top of a filter box. It starts out
# disabled (grayed out); the server enables it as soon as any filter in
# that box differs from its starting value (see the RESET BUTTONS section
# in server()).
reset_selection_button <- function(id) {
  shinyjs::disabled(
    actionButton(
      # (the icon goes in the label, since actionButton()'s own `icon`
      # argument only accepts Shiny's icon(), not a bsicons SVG)
      id, tagList(bsicons::bs_icon("arrow-counterclockwise"), " Reset selection(s)"),
      class = "btn-sm btn-outline-secondary filter-reset-btn",
      title = "Put every filter in this box back to its starting value"
    )
  )
}

# TRUE when what's selected is the same set of values as the default —
# order doesn't matter, and NULL (nothing selected) counts as an empty set
same_selection <- function(selected, default) {
  setequal(as.character(selected), as.character(default))
}

# italic "Last updated: ..." line shown beneath the description at the
# top of each Data View / Map View subtab (the date itself is set once,
# via LAST_UPDATED in global.R)
last_updated_note <- function() {
  tags$span(class = "last-updated", tags$em(paste0("Last updated: ", LAST_UPDATED)))
}

# a small "i" icon that reveals `content` (can include HTML, e.g. a
# link) on hover — pure CSS, no JS/Bootstrap-popover initialization
# needed, so it works reliably without any extra setup. Used to tuck
# a subtab's "Note: ..." caveat behind a compact icon instead of
# always showing the full text inline.
info_tooltip <- function(content) {
  tags$span(
    class = 'info-tooltip-wrapper',
    bsicons::bs_icon("info-circle", class = 'info-tooltip-icon'),
    tags$span(class = 'info-tooltip-content', content)
  )
}

# the full two-column logo footer (Funding and Support + Working Group
# Member Affiliations), wrapped as one function so it's a single call
# on every About subtab — edit the filenames here once, and it updates
# everywhere it's used.
fer_logo_footer <- function() {
  div(
    class = 'logo-partners-section',
    layout_columns(
      col_widths = c(4, 8),
      logo_section("Funding and Support",
                   c("logo_gei.png", "logo_nceas.png", "logo_restore.png")),
      logo_section("Working Group Member Affiliations",
                   c("logo_tbep.png", "logo_seagrant.png", "logo_fwc.jpg",
                     "logo_noaa.png", "logo_uf.png", "logo_usf.png",
                     "logo_ucf.png", "logo_usa.png"))
    )
  )
}


# Full-page "loading" screen, shown from the very first render of the page
# until the dashboard is ready to use. It's plain HTML + CSS — and its CSS
# is INLINE rather than in styles.css, which is a separate request that
# could arrive after the first paint — so it appears (and its spinner
# animates) without waiting for Shiny or any JavaScript to start.
#
# It's removed on Shiny's first "idle" event, i.e. once the server has
# finished computing everything the initial view needs, so the page is
# genuinely ready, not merely connected. Safety nets: a reassurance
# message after 12 s, a hard stop after 120 s so a visitor is never left
# behind it, and it steps aside if the connection drops so Shiny's own
# "disconnected" message is visible instead.
#
# Note it can only appear once the server has SENT the page. Time the
# server spends starting up before that (a "cold start", e.g. the first
# visit after the app was idle or restarted) can't be covered from inside
# the app — see the Shiny Server settings in the notes.
loading_screen <- tagList(
  tags$head(
    tags$style(HTML("
      #fer-loading {
        position: fixed; top: 0; right: 0; bottom: 0; left: 0; z-index: 99999;
        background: #ffffff;
        display: flex; flex-direction: column; align-items: center; justify-content: center;
        gap: 1.1rem; padding: 1.5rem; text-align: center;
        transition: opacity 0.4s ease;
      }
      #fer-loading.fer-loading-done { opacity: 0; pointer-events: none; }
      #fer-loading .fer-loading-title { color: #035172; font-size: 1.6rem; font-weight: 600; }
      #fer-loading .fer-spinner {
        width: 3rem; height: 3rem; border-radius: 50%;
        border: 0.3rem solid #cfe0e6; border-top-color: #035172;
        animation: fer-spin 1s linear infinite;
      }
      #fer-loading .fer-loading-note { color: #495057; font-size: 1rem; min-height: 1.5em; }
      @keyframes fer-spin { to { transform: rotate(360deg); } }
      /* visitors whose device asks for reduced motion get no spinning */
      @media (prefers-reduced-motion: reduce) { #fer-loading .fer-spinner { animation: none; border-top-color: #cfe0e6; } }
    ")),
    tags$script(HTML("
      (function () {
        var done = false;
        function hide() {
          if (done) return; done = true;
          var e = document.getElementById('fer-loading'); if (!e) return;
          e.classList.add('fer-loading-done');                          // fade out...
          setTimeout(function () { e.style.display = 'none'; }, 500);   // ...then drop it from the page entirely
        }
        setTimeout(function () {
          var n = document.getElementById('fer-loading-note');
          if (!done && n) n.textContent = 'Still loading \u2014 the first visit can take a little longer. Thank you for your patience.';
        }, 12000);
        setTimeout(hide, 120000);
        function setup() {
          jQuery(document).one('shiny:idle', hide);                     // everything the first view needs is computed
          jQuery(document).on('shiny:disconnected', hide);              // let Shiny's own message show
        }
        if (window.jQuery) { setup(); }
        else { var wait = setInterval(function () { if (window.jQuery) { clearInterval(wait); setup(); } }, 10); }
      })();
    "))
  ),
  div(
    id = "fer-loading", role = "status", `aria-live` = "polite",
    div(class = "fer-loading-title", "Florida Estuarine Restoration Database"),
    div(class = "fer-spinner", `aria-hidden` = "true"),
    div(id = "fer-loading-note", class = "fer-loading-note", "Loading the dashboard\u2026")
  ),
  # without JavaScript the app can't run, and nothing would ever remove the screen
  tags$noscript(tags$style(HTML("#fer-loading { display: none; }")))
)

ui <- page_navbar(
  # the title doubles as a link back to the About page (handled by the
  # goto_about_home observer in server(), same pattern as the other
  # in-app links)
  title = tags$a(
    "Florida Estuarine Restoration Database",
    href = "#",
    class = "title-link",
    onclick = "Shiny.setInputValue('goto_about_home', Math.random()); return false;"
  ),
  window_title = "Florida Estuarine Restoration Database",
  id = "main-nav",
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    shinyjs::useShinyjs(),
    loading_screen,
    # Show the full menu bar only from 1,200 px up (Bootstrap's default is
    # 992 px, but the bar needs about 1,250 px, so between 992 and 1,200 it
    # wrapped onto two rows). Below that, the menu stays behind the menu
    # button, as on tablets and phones. Done by switching the navbar's
    # "navbar-expand-*" class to the xl one, which is Bootstrap's own
    # mechanism for choosing this breakpoint. Runs before the page is
    # visible: the loading screen is still up at this point.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        var nav = document.querySelector('nav.navbar');
        if (!nav) return;
        ['sm', 'md', 'lg'].forEach(function (s) { nav.classList.remove('navbar-expand-' + s); });
        nav.classList.add('navbar-expand-xl');
      });

      // When the menu is a dropdown (phones, tablets, small laptops), close
      // it once a page has been chosen. Otherwise it stays open over the
      // content until the visitor taps the menu button a second time.
      // (Presses the button itself, so it works however Bootstrap tracks
      // the open state; does nothing when the full menu bar is showing.)
      document.addEventListener('click', function (e) {
        if (!e.target.closest) return;
        if (!e.target.closest('.navbar-collapse .navbar-nav a[data-value], .navbar-brand a')) return;
        var button = document.querySelector('.navbar-toggle, .navbar-toggler');
        var menu = document.querySelector('.navbar-collapse');
        if (button && button.offsetParent !== null && menu &&
            (menu.classList.contains('show') || menu.classList.contains('in'))) {
          button.click();
        }
      });
    "))
  ),
  
  # GEI and TBEP logos, placed between the title and the nav tabs
  nav_item(
    div(
      style = "display: flex; align-items: center; gap: 8px; margin-right: 24px;",
      tags$img(src = "logo_gei.png", height = "30px"),
      tags$img(src = "logo_tbep.png", height = "30px")
    )
  ),

  # ------------------------------------------------------------------
  # ABOUT
  # ------------------------------------------------------------------
  nav_panel(
    title = "About",
    value = 'about',
    class = 'scroll-tab',
    navset_card_underline(
      id = "about-nav",
      full_screen = TRUE,
      height = '100%',

      nav_panel(
        title = "Using the Dashboard",
        class = 'card-scroll',
        fer_banner(),
        shiny::includeMarkdown('www/overview.md'),
        div(
          class = 'feature-grid',
          feature_card("bar-chart-line", "Data View",
                       "View patterns and trends within Florida’s estuarine restoration efforts. View broad-level summaries of project characteristics over time, or take a deep dive into the type of habitats, restoration techniques, and reported funding and costs associated with restoration in different estuarine regions across the state.",
                       "feature-data", "goto_data_view", "data-view"),
          feature_card("geo-alt", "Map View",
                       "Track the geographic footprint of Florida’s estuarine restoration efforts. Explore maps showcasing the locations of individual habitat restoration projects, or choose to summarize these restoration projects by estuary or county. Customize your map by year, habitat, and restoration technique.",
                       "feature-map", "goto_map_view", "map-view"),
          feature_card("cloud-arrow-down", "Download Data",
                       "Want more control? Want additional project information? Here you can download the entire database for your own complete exploration and use. Be sure you check out the accompanying metadata to learn more about what the data contains and how best to use and interpret the data.",
                       "feature-download", "goto_download_data", "download-data"),
          feature_card("pencil-square", "Contribute",
                       "Do you have a restoration project that isn’t captured in the database? Did you discover an error in one of the existing projects? Let us know! Fill out the contact form with information about the project at hand to help contribute to a more comprehensive and accurate database.",
                       "feature-contribute", "goto_contribute", "contribute")
        ),
        fer_logo_footer()
      ),
      nav_panel(
        title = "About the Data",
        class = 'card-scroll',
        fer_banner(),
        shiny::includeMarkdown('www/about_data.md'),
        fer_logo_footer()
      ),
      nav_panel(
        title = "Citation and Sources",
        class = 'card-scroll',
        fer_banner(),
        shiny::includeMarkdown('www/citation.md'),
        fer_logo_footer()
      ),
      nav_panel(
        title = "Contact",
        class = 'card-scroll',
        fer_banner(),
        shiny::includeMarkdown('www/contact.md'),
        fer_logo_footer()
      )
    )
  ),

  # ------------------------------------------------------------------
  # DATA VIEW
  # ------------------------------------------------------------------
  nav_panel(
    title = "Data View",
    value = 'data-view',
    class = 'scroll-tab',

    conditionalPanel(
      condition = "input['dv-nav'] != 'Database Overview'",
      card(
        class = 'map-filter-card',
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            div(style = "height: 25px;", "Select year range:"),
            sliderInput('yrsel_dv', NULL, min = yrmin, max = yrmax,
                        value = c(yrmin, yrmax), step = 1, sep = '', width = '95%')
          ),
          div(
            div(style = "height: 25px;", "Select estuarine region(s):"),
            shinyWidgets::pickerInput(
              'regionsel_dv', NULL, choices = regions, selected = regions,
              multiple = TRUE, width = '100%',
              options = list(`actions-box` = TRUE, size = 10)
            )
          ),
          div(
            div(style = "height: 25px;", "Select habitat(s):"),
            shinyWidgets::pickerInput(
              'habitatsel_dv', NULL, choices = habitats, selected = habitats,
              multiple = TRUE, width = '100%',
              options = list(`actions-box` = TRUE, size = 10)
            )
          ),
          div(
            div(style = "height: 25px;", "Select restoration technique(s):"),
            shinyWidgets::pickerInput(
              'techsel_dv', NULL, choices = techniques, selected = techniques,
              multiple = TRUE, width = '100%',
              options = list(`actions-box` = TRUE, size = 10)
            )
          )
        ),
        # footer row: the "include projects with no date" checkbox (directly under the
        # year slider, at left) and this box's reset button (at right)
        div(
          class = 'filter-footer-row',
          checkboxInput('include_no_year', "Include projects with no date", value = TRUE),
          reset_selection_button('reset_dv')
        )
      )
    ),

    navset_card_underline(
      id = "dv-nav",
      full_screen = TRUE,

      nav_panel(
        title = "Database Overview",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          shiny::HTML(
            "These data summaries reflect the characteristics of all projects included in the Florida Estuarine Restoration Database. It includes summaries of important data characteristics used as filtering criteria for the remainder of the data visualizations used in this dashboard. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>."
          ),
          last_updated_note()
        ),

        layout_columns(
          col_widths = c(6, 6),
          chart_card("Type of Activity",
                     "Original categorical classification from the NEP data source describing the type of activity for a given project. Projects from sources other than NEP are unknown. Projects listed as \"Maintenance\" are excluded in the dashboard data visualizations.",
                     plotly::plotlyOutput('ov_activity', height = '360px')),
          chart_card("Project Status",
                     "Current project status reported by the original data source.",
                     plotly::plotlyOutput('ov_status', height = '360px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Habitat Types",
                     "The total number of restoration projects occurring in each habitat included in the database. Projects in the following habitats are excluded in the dashboard data visualizations: upland, upland and freshwater, coastal upland, freshwater wetland, other, and unknown.",
                     plotly::plotlyOutput('ov_habitat', height = '480px')),
          chart_card("Restoration Techniques",
                     "The total number of restoration projects using each restoration technique in the database. Projects can use multiple techniques. Projects that exclusively use the following techniques are excluded in the dashboard data visualizations: species control.",
                     plotly::plotlyOutput('ov_technique', height = '480px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Temporal Record",
                     "Number of projects in which a completion year is available.",
                     plotly::plotlyOutput('ov_temporal', height = '360px')),
          chart_card("Project Cost",
                     "Number of projects in which reported project cost information is available. Costs are missing for some projects, so cost summaries throughout the dashboard likely underestimate true costs.",
                     plotly::plotlyOutput('ov_cost', height = '360px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Project Size",
                     "Number of projects in which data is available regarding the size of the activity. Size can be captured either in area units (acres) or length units (miles), depending on the project.",
                     plotly::plotlyOutput('ov_size', height = '360px')),
          chart_card("Primary Data Source",
                     "Number of projects compiled from each original data source. This only captures the primary data source of a project. Additional sources used to fill in missing information for a single project can be viewed in the full database.",
                     plotly::plotlyOutput('ov_source', height = '360px'))
        )
      ),
      nav_panel(
        title = "Restored Habitats",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          p(
            "Explore this section to learn more about the habitat characteristics of restoration projects included in the database.",
            info_tooltip(shiny::HTML("Note: These data summaries have been pre-filtered to best reflect true estuarine habitat restoration efforts by excluding certain habitats, activities, and restoration techniques. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>.")),
            last_updated_note()
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          chart_card("Restored Habitats (Frequency)",
                     "The total number of restoration projects occurring in each habitat.",
                     plotly::plotlyOutput('rh_freq', height = '480px')),
          chart_card("Restored Habitats (Representation)",
                     "The proportion of all restoration projects occurring in each habitat. Unlabeled habitats represent less than 5% of all projects.",
                     plotOutput('rh_pie', height = '480px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Temporal Trends in Projects",
                     "The number of restoration projects completed in a given year for each habitat. Excludes projects without a recorded completion date.",
                     plotly::plotlyOutput('rh_temporal_n', height = '480px')),
          chart_card("Temporal Trends in Representation",
                     "The proportion of all restoration projects completed in a given year for each habitat. Excludes projects without a recorded completion date.",
                     plotly::plotlyOutput('rh_temporal_pct', height = '480px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Area Restored",
                     "The total number of acres restored across all areal restoration projects for each habitat. Excludes projects without a recorded area of restoration.",
                     plotly::plotlyOutput('rh_area', height = '400px')),
          chart_card("Miles Restored",
                     "The total number of miles restored across all linear restoration projects for each habitat. Excludes projects without a recorded length of restoration.",
                     plotly::plotlyOutput('rh_miles', height = '400px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Cumulative Temporal Footprint (Acreage)",
                     "Comparison of the cumulative number of all restoration projects (left axis) and the cumulative area of all restoration projects (right axis) over time for all selected habitats. Excludes projects without a recorded completion date and projects without a recorded area of restoration.",
                     plotly::plotlyOutput('rh_cumulative_acres', height = '360px')),
          chart_card("Cumulative Temporal Footprint (Mileage)",
                     "Comparison of the cumulative number of all restoration projects (left axis) and the cumulative length of all restoration projects (right axis) over time for all selected habitats. Excludes projects without a recorded completion date and projects without a recorded length of restoration.",
                     plotly::plotlyOutput('rh_cumulative_miles', height = '360px'))
        )
      ),
      nav_panel(
        title = "Restoration Techniques",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          p(
            "Explore this section to learn more about the techniques being used to restore habitats in the database.",
            info_tooltip(shiny::HTML("Note: These data summaries have been pre-filtered to best reflect true estuarine habitat restoration efforts by excluding certain habitats, activities, and restoration techniques. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>.")),
            last_updated_note()
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          chart_card("Restoration Techniques (Frequency)",
                     "The total number of restoration projects using each technique. Projects using multiple techniques are represented within each technique's total.",
                     plotly::plotlyOutput('rt_freq', height = '480px')),
          chart_card("Restoration Techniques (Representation)",
                     "The proportion of all restoration project records using each technique. Percentage is based on all project entries, including individual projects using multiple techniques. Unlabeled techniques represent less than 5% of all projects.",
                     plotOutput('rt_pie', height = '480px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Temporal Trends in Projects",
                     "The number of restoration projects completed in a given year using each technique. Excludes projects without a recorded completion date. Projects using multiple techniques are represented within each technique's total.",
                     plotly::plotlyOutput('rt_temporal_n', height = '480px')),
          chart_card("Temporal Trends in Representation",
                     "The proportion of all restoration projects completed in a given year using each technique. Excludes projects without a recorded completion date. Percentage is based on all project entries, including individual projects using multiple techniques.",
                     plotly::plotlyOutput('rt_temporal_pct', height = '480px'))
        ),
        layout_columns(
          col_widths = c(12),
          chart_card("Application of Restoration Techniques to Habitats",
                     "Heatmap showing the number of projects applying each restoration technique to a given habitat. Rows and columns are sorted according to their overall abundance in the database.",
                     tagList(
                       uiOutput('rt_hotspot_legend'),
                       reactable::reactableOutput('rt_hotspot')
                     ))
        )
      ),
      nav_panel(
        title = "Restoration Dollars",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          p(
            "Explore this section to learn more about the reported funding and costs of restoration projects in the database. Only costs that were reported are included, so the amounts shown likely underestimate true restoration costs.",
            info_tooltip(shiny::HTML("Note: These data summaries have been pre-filtered to best reflect true estuarine habitat restoration efforts by excluding certain habitats, activities, and restoration techniques. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>.")),
            last_updated_note()
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          chart_card("Restoration Funding (Frequency)",
                     "Comparison of the total number of restoration projects funded (left axis) and the total reported cost of those projects (right axis) by different sectors. For projects with funding from multiple sources, only the main funding source is used. Projects without reported costs are excluded from the cost totals, so these amounts likely underestimate true costs.",
                     plotly::plotlyOutput('rd_funding_freq', height = '380px')),
          chart_card("Restoration Funding (Representation)",
                     "The proportion of all reported restoration costs funded by different sectors. Percentage is based on the main funding source for all projects with reported costs. Unlabeled sectors represent less than 5% of all reported project costs.",
                     plotOutput('rd_funding_pie', height = '480px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Temporal Trends in Funding",
                     "The total reported cost of restoration projects completed in a given year (left axis) and the cumulative reported cost over time (right axis). Excludes projects without a recorded completion date or reported cost, so these amounts likely underestimate true costs.",
                     plotly::plotlyOutput('rd_funding_trend', height = '380px')),
          chart_card("Temporal Trends in Funding Source",
                     "The proportion of all reported restoration costs in a given year funded by different sectors. Excludes projects without a recorded completion date or reported cost. Percentage is based on the main funding source for a given project.",
                     plotly::plotlyOutput('rd_temporal_pct', height = '480px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          chart_card("Habitat Funding Distribution (Number of Projects)",
                     "The share of a funding source's total number of restoration projects allocated to different habitats. Sectors and habitats making up less than 5% of all projects are excluded for easier viewing.",
                     plotOutput('rd_alluvial_projects', height = '520px')),
          chart_card("Habitat Funding Distribution (Reported Costs)",
                     "The share of a funding source's total reported restoration costs allocated to different habitats. Excludes projects without reported costs. Sectors and habitats making up less than 5% of all reported project costs are excluded for easier viewing.",
                     plotOutput('rd_alluvial', height = '520px'))
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # MAP VIEW
  # ------------------------------------------------------------------
  nav_panel(
    title = "Map View",
    value = 'map-view',
    class = 'scroll-tab',

    card(
      class = 'map-filter-card',
      layout_columns(
        col_widths = c(4, 4, 4),
        div(
          div(style = "height: 25px;", "Select year range:"),
          sliderInput('yrsel_mv', NULL, min = yrmin, max = yrmax,
                      value = c(yrmin, yrmax), step = 1, sep = '', width = '95%')
        ),
        div(
          div(style = "height: 25px;", "Select habitat(s):"),
          shinyWidgets::pickerInput(
            'habitatsel_mv', NULL, choices = habitats, selected = habitats,
            multiple = TRUE, width = '100%',
            options = list(`actions-box` = TRUE, size = 10)
          )
        ),
        div(
          div(style = "height: 25px;", "Select restoration technique(s):"),
          shinyWidgets::pickerInput(
            'techsel_mv', NULL, choices = techniques, selected = techniques,
            multiple = TRUE, width = '100%',
            options = list(`actions-box` = TRUE, size = 10)
          )
        )
      ),
      # footer row: the "include projects with no date" checkbox (directly under the
      # year slider, at left) and this box's reset button (at right)
      div(
        class = 'filter-footer-row',
        checkboxInput('include_no_year_mv', "Include projects with no date", value = TRUE),
        reset_selection_button('reset_mv')
      )
    ),

    navset_card_underline(
      full_screen = TRUE,

      nav_panel(
        title = "Project Sites",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          p(
            "Explore this section to view the location of restoration projects included in the database. Choose how you would like to color and scale the size of the points. If scaling by size or reported cost, projects without this data will be dropped from the map.",
            info_tooltip(shiny::HTML("Note: These data summaries have been pre-filtered to best reflect true estuarine habitat restoration efforts by excluding certain habitats, activities, and restoration techniques. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>.")),
            last_updated_note()
          )
        ),

        layout_columns(
          col_widths = c(3, 3, 6),
          selectInput('colorby_mv', "Color points by:",
                      choices = c("None (default)", "Habitat", "Restoration technique", "Activity"),
                      selected = "None (default)"),
          selectInput('scaleby_mv', "Scale points by:",
                      choices = c("None (default)", "Project size (acres)", "Project size (miles)", "Reported project cost ($)"),
                      selected = "None (default)"),
          div()
        ),
        leaflet::leafletOutput('sitemap', height = '500px')
      ),
      nav_panel(
        title = "Regions",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          p(
            "Explore this section to compare characteristics of habitat restoration projects across Florida's estuarine regions.",
            info_tooltip(shiny::HTML("Note: These data summaries have been pre-filtered to best reflect true estuarine habitat restoration efforts by excluding certain habitats, activities, and restoration techniques. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>.")),
            last_updated_note()
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Restoration Effort"), leaflet::leafletOutput('regionmap_projects', height = '380px')),
          card(card_header("Reported Restoration Costs"), leaflet::leafletOutput('regionmap_cost', height = '380px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Restoration Footprint (Area)"), leaflet::leafletOutput('regionmap_acres', height = '380px')),
          card(card_header("Restoration Footprint (Length)"), leaflet::leafletOutput('regionmap_miles', height = '380px'))
        )
      ),
      nav_panel(
        title = "Counties",

        div(
          class = 'chart-desc',
          style = 'font-size: 0.9rem; margin-bottom: 1rem;',
          p(
            "Explore this section to compare characteristics of habitat restoration projects across Florida's counties.",
            info_tooltip(shiny::HTML("Note: These data summaries have been pre-filtered to best reflect true estuarine habitat restoration efforts by excluding certain habitats, activities, and restoration techniques. For more information about the use of these filters and our recommendations for analyzing these data, go to <a href=\"#\" onclick=\"Shiny.setInputValue('goto_about_data', Math.random()); return false;\">About the Data</a>.")),
            last_updated_note()
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Restoration Effort"), leaflet::leafletOutput('countymap_projects', height = '380px')),
          card(card_header("Reported Restoration Costs"), leaflet::leafletOutput('countymap_cost', height = '380px'))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Restoration Footprint (Area)"), leaflet::leafletOutput('countymap_acres', height = '380px')),
          card(card_header("Restoration Footprint (Length)"), leaflet::leafletOutput('countymap_miles', height = '380px'))
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # DOWNLOAD DATA
  # ------------------------------------------------------------------
  nav_panel(
    title = "Download Data",
    value = 'download-data',
    class = 'scroll-tab',

    layout_sidebar(
      sidebar = sidebar(
        title = "Metadata",
        width = 320,
        open = TRUE,
        class = 'card-scroll',
        shiny::includeMarkdown('www/metadata.md')
      ),

      h4("Florida Estuarine Restoration Database"),

      # search + filters for the records below. NOTHING here changes the
      # table (or what the download button exports) until "Apply" is
      # clicked. An empty dropdown shows "All records" and means "don't
      # filter on this", so the default is every record.
      card(
        class = 'map-filter-card',
        fill = FALSE,
        div(
          div(style = "height: 25px;", "Search by Project Name"),
          textInput('dl_search', NULL, placeholder = "Enter all or part of a project name", width = '100%')
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            div(style = "height: 25px;", "Select year range:"),
            sliderInput('dl_years', NULL, min = dl_years[1], max = dl_years[2],
                        value = dl_years, step = 1, sep = '', width = '95%')
          ),
          div(
            div(style = "height: 25px;", "Select estuarine region(s):"),
            shinyWidgets::pickerInput(
              'dl_regions', NULL, choices = dl_regions, selected = NULL,
              multiple = TRUE, width = '100%',
              options = shinyWidgets::pickerOptions(noneSelectedText = "All records", size = 10)
            )
          ),
          div(
            div(style = "height: 25px;", "Select habitat(s):"),
            shinyWidgets::pickerInput(
              'dl_habitats', NULL, choices = dl_habitats, selected = NULL,
              multiple = TRUE, width = '100%',
              options = shinyWidgets::pickerOptions(noneSelectedText = "All records", size = 10)
            )
          ),
          div(
            div(style = "height: 25px;", "Select restoration technique(s):"),
            shinyWidgets::pickerInput(
              'dl_techniques', NULL, choices = dl_techniques, selected = NULL,
              multiple = TRUE, width = '100%',
              options = shinyWidgets::pickerOptions(noneSelectedText = "All records", size = 10)
            )
          )
        ),
        # footer row: the "include projects with no date" checkbox at left,
        # Apply + Reset at right (same pattern as the other pages' filter boxes)
        div(
          class = 'filter-footer-row',
          checkboxInput('dl_no_year', "Include projects with no date", value = TRUE),
          div(
            class = 'filter-footer-buttons',
            actionButton('dl_apply', tagList(bsicons::bs_icon("funnel"), " Apply"),
                         class = "btn-sm btn-primary",
                         title = "Show (and download) only the records that meet the search and filters above"),
            reset_selection_button('dl_reset')
          )
        )
      ),

      # pressing Enter in the search box does the same as clicking Apply.
      # The typed text is sent first (a "change" event sends it right away
      # rather than after typing's usual short delay), then the Apply event.
      tags$script(HTML("
        $(document).on('keyup', '#dl_search', function(e) {
          if (e.key === 'Enter') {
            $(this).trigger('change');
            Shiny.setInputValue('dl_apply_enter', Math.random(), {priority: 'event'});
          }
        });
      ")),

      reactable::reactableOutput('dltable'),

      div(class = 'dl-note', textOutput('dl_note')),

      shinyWidgets::downloadBttn('dwnld', paste0('Download Database (v', DB_VERSION, ')'), style = 'simple',
                                  block = TRUE, color = 'success')
    )
  ),

  # ------------------------------------------------------------------
  # CONTRIBUTE
  # ------------------------------------------------------------------
  nav_panel(
    title = "Contribute",
    value = 'contribute',
    class = 'scroll-tab',

    navset_card_underline(
      full_screen = TRUE,
      height = '100%',

      nav_panel(
        title = "Submit a Project",
        class = 'card-scroll fer-contrib-pane',
        fer_banner(),
        # text and form: side by side on wide screens, stacked on narrow ones
        # (see ".fer-contrib-cols" in styles.css)
        div(
          class = "fer-contrib-cols",
          div(class = "fer-contrib-text", shiny::includeMarkdown('www/submit_project.md')),
          div(class = "fer-contrib-form", google_form_embed(FORM_SUBMIT_URL, FORM_SUBMIT_HEIGHT, "Submit a Project form"))
        )
      ),
      nav_panel(
        title = "Correct a Project",
        class = 'card-scroll fer-contrib-pane',
        fer_banner(),
        div(
          class = "fer-contrib-cols",
          div(
            class = "fer-contrib-text",
            h2("Correct a Project"),
            p("Spot an error in a restoration project in the existing database? Let us know using the form below. We require vetting of all proposed corrections and updates, so a member of our team will contact you to discuss these changes further. Before submitting a correction, please read through the entire project information in the full database. Copy the exact project name from the database into the form so we can easily identify which project this pertains to.")
          ),
          div(class = "fer-contrib-form", google_form_embed(FORM_CORRECT_URL, FORM_CORRECT_HEIGHT, "Correct a Project form"))
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/GEI-Restoration-Fisheries/fl-estuarine-restoration-dashboard",
      target = "_blank",
      class = "source-code-link",
      style = "display: flex; align-items: center; gap: 6px;",
      tags$img(src = "logo_git.png", height = "20px"),
      "Source Code"
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$goto_about_home, {
    # lands on the About page's first subtab, even if another About
    # subtab was open when the link was clicked
    updateNavbarPage(session, inputId = "main-nav", selected = "about")
    updateNavbarPage(session, inputId = "about-nav", selected = "Using the Dashboard")
  })
  observeEvent(input$goto_data_view, {
    updateNavbarPage(session, inputId = "main-nav", selected = "data-view")
  })
  observeEvent(input$goto_map_view, {
    updateNavbarPage(session, inputId = "main-nav", selected = "map-view")
  })
  observeEvent(input$goto_download_data, {
    updateNavbarPage(session, inputId = "main-nav", selected = "download-data")
  })
  observeEvent(input$goto_contribute, {
    updateNavbarPage(session, inputId = "main-nav", selected = "contribute")
  })
  observeEvent(input$goto_citation, {
    # this link lives on a different top-level tab than its destination,
    # and its destination is itself a SUBTAB nested inside "About" — so
    # both levels need to be switched together for the click to actually
    # land somewhere visible.
    updateNavbarPage(session, inputId = "main-nav", selected = "about")
    updateNavbarPage(session, inputId = "about-nav", selected = "Citation and Sources")
  })
  observeEvent(input$goto_about_data, {
    updateNavbarPage(session, inputId = "main-nav", selected = "about")
    updateNavbarPage(session, inputId = "about-nav", selected = "About the Data")
  })
  
  # ==================================================================
  # DATABASE OVERVIEW
  # Uses the FULL, unfiltered `projects` table always — no year/region
  # filtering applies here (unlike every other Data View subtab), since
  # the whole point of this subtab is showing what the pre-filter
  # criteria (used everywhere else) actually exclude.
  # ==================================================================

  output$ov_activity <- plotly::renderPlotly(
    category_count_fun(projects, "nep_activity_name", xlab = "", angle_labels = FALSE, dedupe_by_project = TRUE, fill_color = "#035172")
  )

  output$ov_status <- plotly::renderPlotly(
    category_count_fun(projects, "project_status", xlab = "", angle_labels = FALSE, dedupe_by_project = TRUE, fill_color = "#035172")
  )

  # full, unfiltered data — includes every habitat/technique, even the
  # ones excluded from the rest of the dashboard (Upland, Species
  # control, etc.), matching how Restored Habitats'/Restoration
  # Techniques' own Frequency charts are built, just without the
  # working_data_fun() pre-filter applied. No dedupe_by_project here —
  # habitat/technique are the breakdown dimension itself, so each row
  # (i.e. each habitat/technique a project touches) is correctly
  # represented on its own, same reasoning as everywhere else in the
  # dashboard that breaks down by these two fields.
  output$ov_habitat <- plotly::renderPlotly(
    category_count_fun(projects, "habitat", xlab = "", palette = habitat_colors, angle_labels = TRUE)
  )

  output$ov_technique <- plotly::renderPlotly(
    category_count_fun(projects, "technique", xlab = "", explode_col = "technique", angle_labels = TRUE, palette = technique_colors)
  )

  output$ov_temporal <- plotly::renderPlotly({
    d <- projects %>%
      dplyr::mutate(year_avail = ifelse(is.na(year), "Unavailable", "Available"))
    category_count_fun(d, "year_avail", xlab = "", angle_labels = FALSE, dedupe_by_project = TRUE, fill_color = "#035172")
  })

  output$ov_cost <- plotly::renderPlotly({
    d <- projects %>%
      dplyr::mutate(cost_avail = ifelse(is.na(cost_dollars), "Unavailable", "Available"))
    category_count_fun(d, "cost_avail", xlab = "", angle_labels = FALSE, dedupe_by_project = TRUE, fill_color = "#035172")
  })

  output$ov_size <- plotly::renderPlotly({
    d <- projects %>%
      dplyr::mutate(size_avail = ifelse(!is.na(area_acres) | !is.na(length_miles), "Available", "Unavailable"))
    category_count_fun(d, "size_avail", xlab = "", angle_labels = FALSE, dedupe_by_project = TRUE, fill_color = "#035172")
  })

  output$ov_source <- plotly::renderPlotly(
    category_count_fun(projects, "source", xlab = "", explode_col = "source", angle_labels = TRUE, dedupe_by_project = TRUE, fill_color = "#035172")
  )

  # ==================================================================
  # DATA VIEW
  # ==================================================================

  # "Include projects with no date" (default checked) —
  # when checked, the year slider is locked to the full range and
  # disabled, since it isn't meaningfully narrowing anything; unchecking
  # it unlocks the slider so a specific range can be selected. Every
  # chart that reads input$yrsel_dv downstream (via X_data_year())
  # naturally shows the full timeline while this is checked, with no
  # special-casing needed there.
  observeEvent(input$include_no_year, {
    if (isTRUE(input$include_no_year)) {
      updateSliderInput(session, 'yrsel_dv', value = c(yrmin, yrmax))
      shinyjs::disable('yrsel_dv')
    } else {
      shinyjs::enable('yrsel_dv')
    }
  }, ignoreInit = FALSE)

  # ==================================================================
  # "RESET SELECTION(S)" BUTTONS
  # One per filter box (Data View, Map View). Each button is grayed out
  # and unclickable while every filter in its box is at its starting
  # value, becomes clickable as soon as any one of them differs, and
  # puts that box's filters back to their starting values when clicked.
  # The starting values are the same ones the filter inputs are created
  # with in the UI: the full year range, "include projects with no date"
  # checked, and everything selected in each dropdown.
  # ==================================================================

  dv_filters_changed <- reactive({
    !isTRUE(input$include_no_year) ||
      !same_selection(input$yrsel_dv,      c(yrmin, yrmax)) ||
      !same_selection(input$regionsel_dv,  regions) ||
      !same_selection(input$habitatsel_dv, habitats) ||
      !same_selection(input$techsel_dv,    techniques)
  })

  mv_filters_changed <- reactive({
    !isTRUE(input$include_no_year_mv) ||
      !same_selection(input$yrsel_mv,      c(yrmin, yrmax)) ||
      !same_selection(input$habitatsel_mv, habitats) ||
      !same_selection(input$techsel_mv,    techniques)
  })

  observe(shinyjs::toggleState('reset_dv', condition = dv_filters_changed()))
  observe(shinyjs::toggleState('reset_mv', condition = mv_filters_changed()))

  observeEvent(input$reset_dv, {
    # re-checking the box also re-locks the slider (observer above)
    updateCheckboxInput(session, 'include_no_year', value = TRUE)
    updateSliderInput(session, 'yrsel_dv', value = c(yrmin, yrmax))
    shinyWidgets::updatePickerInput(session, 'regionsel_dv',  selected = regions)
    shinyWidgets::updatePickerInput(session, 'habitatsel_dv', selected = habitats)
    shinyWidgets::updatePickerInput(session, 'techsel_dv',    selected = techniques)
  })

  observeEvent(input$reset_mv, {
    updateCheckboxInput(session, 'include_no_year_mv', value = TRUE)
    updateSliderInput(session, 'yrsel_mv', value = c(yrmin, yrmax))
    shinyWidgets::updatePickerInput(session, 'habitatsel_mv', selected = habitats)
    shinyWidgets::updatePickerInput(session, 'techsel_mv',    selected = techniques)
  })

  # ==================================================================
  # RESTORED HABITATS
  # Builds on working_data_fun()'s pre-filter (activity type, project
  # status, geographic bounds — see global.R) plus the shared region
  # filter and this subtab's OWN habitat-select dropdown. rh_data()
  # deliberately does NOT apply the year range slider — that's added
  # separately in rh_data_year() for the charts that respect it.
  # ==================================================================

  rh_data <- reactive({
    req(input$habitatsel_dv)
    working_data_fun(projects) %>%
      dplyr::filter(
        region %in% input$regionsel_dv,
        habitat %in% input$habitatsel_dv
      )
  })

  rh_data_year <- reactive({
    rh_data() %>%
      dplyr::filter(year >= input$yrsel_dv[1], year <= input$yrsel_dv[2])
  })

  output$rh_freq <- plotly::renderPlotly(
    category_count_fun(
      if (isTRUE(input$include_no_year)) rh_data() else rh_data_year(),
      "habitat", xlab = "", palette = habitat_colors
    )
  )

  output$rh_pie <- shiny::renderPlot(
    donut_fun(
      if (isTRUE(input$include_no_year)) rh_data() else rh_data_year(),
      "habitat", palette = habitat_colors, label_threshold = 0.05
    )
  )

  output$rh_temporal_n <- plotly::renderPlotly(
    stacked_bar_fun(rh_data_year(), "habitat", xvar = "year", palette = habitat_colors, xrange = input$yrsel_dv)
  )

  output$rh_temporal_pct <- plotly::renderPlotly(
    stacked_bar_fun(rh_data_year(), "habitat", xvar = "year", palette = habitat_colors, pct = TRUE, xrange = input$yrsel_dv)
  )

  output$rh_area <- plotly::renderPlotly(
    habitat_metric_bar_fun(
      if (isTRUE(input$include_no_year)) rh_data() else rh_data_year(),
      "habitat", "area_acres", ylab = "Acres restored", palette = habitat_colors, hover_suffix = " acres"
    )
  )

  output$rh_miles <- plotly::renderPlotly(
    habitat_metric_bar_fun(
      if (isTRUE(input$include_no_year)) rh_data() else rh_data_year(),
      "habitat", "length_miles", ylab = "Miles restored", palette = habitat_colors, hover_suffix = " miles"
    )
  )

  output$rh_cumulative_acres <- plotly::renderPlotly(
    cumulative_line_fun(rh_data_year(), "area_acres", line_label = "Cumulative acres restored", xrange = input$yrsel_dv)
  )

  output$rh_cumulative_miles <- plotly::renderPlotly(
    cumulative_line_fun(rh_data_year(), "length_miles", line_label = "Cumulative miles restored", xrange = input$yrsel_dv)
  )

  # ==================================================================
  # RESTORATION TECHNIQUES
  # Builds on the same pre-filter (working_data_fun) + region + habitat
  # baseline as Restored Habitats, then EXPLODES the multi-value
  # technique field (so a multi-technique project counts toward each
  # technique it lists) and filters to the selected technique(s). As
  # with Restored Habitats, rt_data() deliberately excludes the year
  # range filter — that's layered on separately in rt_data_year() for
  # the charts that respect it.
  # ==================================================================

  rt_base <- reactive({
    req(input$habitatsel_dv)
    working_data_fun(projects) %>%
      dplyr::filter(
        region %in% input$regionsel_dv,
        habitat %in% input$habitatsel_dv
      )
  })

  rt_data <- reactive({
    req(input$techsel_dv)
    explode_multi(rt_base(), "technique") %>%
      dplyr::filter(technique %in% input$techsel_dv)
  })

  rt_data_year <- reactive({
    rt_data() %>%
      dplyr::filter(year >= input$yrsel_dv[1], year <= input$yrsel_dv[2])
  })

  output$rt_freq <- plotly::renderPlotly(
    category_count_fun(
      if (isTRUE(input$include_no_year)) rt_data() else rt_data_year(),
      "technique", xlab = "", palette = technique_colors
    )
  )

  output$rt_pie <- shiny::renderPlot(
    donut_fun(
      if (isTRUE(input$include_no_year)) rt_data() else rt_data_year(),
      "technique", label_threshold = 0.05, palette = technique_colors
    )
  )

  output$rt_temporal_n <- plotly::renderPlotly(
    stacked_bar_fun(rt_data_year(), "technique", xvar = "year", palette = technique_colors, xrange = input$yrsel_dv)
  )

  output$rt_temporal_pct <- plotly::renderPlotly(
    stacked_bar_fun(rt_data_year(), "technique", xvar = "year", pct = TRUE, palette = technique_colors, xrange = input$yrsel_dv)
  )

  output$rt_hotspot <- reactable::renderReactable(
    hotspot_table_fun(
      if (isTRUE(input$include_no_year)) rt_data() else rt_data_year(),
      "technique", "habitat"
    )
  )

  output$rt_hotspot_legend <- renderUI(
    hotspot_legend_fun(
      if (isTRUE(input$include_no_year)) rt_data() else rt_data_year(),
      "technique", "habitat", legend_label = "Number of projects"
    )
  )

  # ==================================================================
  # RESTORATION DOLLARS
  # Same pre-filter + region + habitat baseline as the other subtabs,
  # but technique filtering here uses multi_match() rather than
  # exploding — these charts SUM dollars, and exploding (duplicating a
  # multi-technique project into one row per technique) would double-
  # count that project's cost. Exploding is only correct when the
  # chart itself breaks down BY technique (Restoration Techniques'
  # own charts) — it's not a general-purpose "apply the technique
  # filter" tool.
  # ==================================================================

  rd_data <- reactive({
    req(input$habitatsel_dv, input$techsel_dv)
    working_data_fun(projects) %>%
      dplyr::filter(
        region %in% input$regionsel_dv,
        habitat %in% input$habitatsel_dv,
        multi_match(technique, input$techsel_dv)
      )
  })

  rd_data_year <- reactive({
    rd_data() %>%
      dplyr::filter(year >= input$yrsel_dv[1], year <= input$yrsel_dv[2])
  })

  output$rd_funding_freq <- plotly::renderPlotly(
    footprint_plot_fun(
      if (isTRUE(input$include_no_year)) rd_data() else rd_data_year(),
      "cost_dollars",
      bar_label = "Number of projects",
      line_label = "Reported project cost ($)",
      xvar = "funding_category",
      dedupe_sumvar = TRUE,
      palette = funding_colors,
      line_color = "black"
    )
  )

  output$rd_funding_pie <- shiny::renderPlot(
    donut_fun(
      if (isTRUE(input$include_no_year)) rd_data() else rd_data_year(),
      "funding_category", sumvar = "cost_dollars", label_threshold = 0.05,
      dedupe_by_project = TRUE, palette = funding_colors
    )
  )

  output$rd_funding_trend <- plotly::renderPlotly(
    cost_trend_fun(rd_data_year(), "cost_dollars", xrange = input$yrsel_dv)
  )

  output$rd_temporal_pct <- plotly::renderPlotly(
    stacked_bar_fun(rd_data_year(), "funding_category", xvar = "year",
                     sumvar = "cost_dollars", pct = TRUE, ylab = "Proportion of reported costs",
                     dedupe_by_project = TRUE, palette = funding_colors,
                     xrange = input$yrsel_dv)
  )

  output$rd_alluvial <- shiny::renderPlot(
    alluvial_fun(
      if (isTRUE(input$include_no_year)) rd_data() else rd_data_year(),
      "funding_category", "habitat", "cost_dollars",
      source_label = "Funding Sector", target_label = "Habitat Type",
      palette = funding_colors
    )
  )

  output$rd_alluvial_projects <- shiny::renderPlot(
    # reuses alluvial_fun unchanged — it just sums whatever column it's
    # given, so a constant "1 per row" column makes that sum equal a
    # project COUNT instead of a dollar total
    alluvial_fun(
      (if (isTRUE(input$include_no_year)) rd_data() else rd_data_year()) %>%
        dplyr::mutate(.project_count = 1),
      "funding_category", "habitat", ".project_count",
      source_label = "Funding Sector", target_label = "Habitat Type",
      palette = funding_colors,
      label_overrides = c(
        "Tidal tributary/creek/river" = "Tidal tributary",
        "Submerged aquatic vegetation" = "SAV",
        "Mixed-partner" = "Mixed",
        "NGO/Nonprofit" = "Nonprofit"
      )
    )
  )

  # ==================================================================
  # MAP VIEW
  # ==================================================================

  # mirrors the Data View toggle — locks the slider to the full range
  # and disables it while checked, same reasoning as there
  observeEvent(input$include_no_year_mv, {
    if (isTRUE(input$include_no_year_mv)) {
      updateSliderInput(session, 'yrsel_mv', value = c(yrmin, yrmax))
      shinyjs::disable('yrsel_mv')
    } else {
      shinyjs::enable('yrsel_mv')
    }
  }, ignoreInit = FALSE)

  mv_data <- reactive({
    req(input$habitatsel_mv, input$techsel_mv, project_points)
    data <- working_data_fun(project_points) %>%
      dplyr::filter(
        habitat %in% input$habitatsel_mv,
        # technique can list multiple values in one row (e.g. "Species
        # control, Planting") — multi_match() matches if ANY selected
        # value is present, counting the row once either way
        multi_match(technique, input$techsel_mv)
      )
    if (isTRUE(input$include_no_year_mv)) {
      data
    } else {
      data %>% dplyr::filter(!is.na(year), year >= input$yrsel_mv[1], year <= input$yrsel_mv[2])
    }
  })

  # rendered exactly once — no reactive inputs referenced here at all,
  # so this never re-runs and never resets the map's view. The initial
  # view is fit to the FULL, unfiltered dataset's extent (not whatever
  # happens to be selected at the moment), giving a sensible Florida-
  # wide starting point regardless of the default filter selections.
  output$sitemap <- leaflet::renderLeaflet({
    bbox <- sf::st_bbox(project_points)
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
      leaflet::fitBounds(
        lng1 = unname(bbox["xmin"]), lat1 = unname(bbox["ymin"]),
        lng2 = unname(bbox["xmax"]), lat2 = unname(bbox["ymax"])
      )
  })
  # by default, Shiny defers rendering outputs in tabs that aren't
  # currently visible — since the observe() block below uses
  # leafletProxy() (which needs the widget to already exist client-
  # side), that deferral meant the FIRST update could fire before the
  # map ever reached the browser and get silently dropped, leaving no
  # points until a later selection change re-triggered the observer.
  # This forces the base map to render immediately on session start
  # regardless of which tab is active.
  outputOptions(output, "sitemap", suspendWhenHidden = FALSE)

  # everything that changes with the filters/dropdowns happens here
  # instead, via leafletProxy() updating the EXISTING map object rather
  # than rebuilding/re-rendering it — this is what keeps the user's
  # current pan/zoom intact when a selection changes, rather than
  # snapping back to fit the newly-filtered data's extent every time
  observe({
    data <- mv_data()

    # rows missing the SELECTED scaling variable are dropped from the
    # map entirely, rather than shown as a separate "No data" bucket —
    # 0 counts as missing too, for all three metrics
    if (input$scaleby_mv == "Project size (acres)") {
      data <- data %>% dplyr::filter(!is.na(area_acres), area_acres > 0)
    } else if (input$scaleby_mv == "Project size (miles)") {
      data <- data %>% dplyr::filter(!is.na(length_miles), length_miles > 0)
    } else if (input$scaleby_mv == "Reported project cost ($)") {
      data <- data %>% dplyr::filter(!is.na(cost_dollars), cost_dollars > 0)
    }

    proxy <- leaflet::leafletProxy("sitemap") %>%
      leaflet::clearMarkers() %>%
      leaflet::clearControls()

    if (nrow(data) == 0) return(NULL)

    colorvar <- switch(input$colorby_mv,
      "Habitat" = data$habitat,
      # multiple techniques per project are common (e.g. "Species
      # control, Planting"), which would otherwise create dozens of
      # near-one-off combination categories — not visually useful for
      # color-coding. Single-technique projects keep their real
      # technique color; anything with more than one collapses into a
      # shared "Multiple techniques" category instead. The exact list
      # is still visible in each point's popup.
      "Restoration technique" = ifelse(
        is.na(data$technique), "Unknown",
        ifelse(grepl(",", data$technique), "Multiple techniques", data$technique)
      ),
      "Activity" = ifelse(is.na(data$nep_activity_name), "Unknown", data$nep_activity_name),
      NULL
    )
    pal <- switch(input$colorby_mv,
      "Habitat" = named_map_palette_fun(colorvar, habitat_colors),
      # "Multiple techniques" is a synthetic bucket that only exists on this map
      "Restoration technique" = named_map_palette_fun(colorvar, c(technique_colors, "Multiple techniques" = "#FF3B9D")),
      "Activity" = named_map_palette_fun(colorvar, activity_colors),
      NULL
    )
    marker_color <- if (!is.null(pal)) pal(colorvar) else "#3182bd"

    # all 3 metrics now use the SAME quartile-based approach — each
    # point's own field is split into 4 bins by its own quartiles
    # (computed from the real, currently-filtered data), giving 4
    # meaningful, data-driven size categories instead of either a
    # fixed pre-defined bucket or a raw continuous scale
    qr <- NULL
    if (input$scaleby_mv == "Project size (acres)") {
      sizevar <- data$area_acres
      qr <- quartile_radius_fun(sizevar)
    } else if (input$scaleby_mv == "Project size (miles)") {
      sizevar <- data$length_miles
      qr <- quartile_radius_fun(sizevar)
    } else if (input$scaleby_mv == "Reported project cost ($)") {
      sizevar <- data$cost_dollars
      qr <- quartile_radius_fun(sizevar)
    } else {
      sizevar <- NULL
    }
    # use the SAME radius function for both the markers and the legend
    # swatches, so what's on the map always matches what the legend says
    radius <- if (!is.null(qr)) qr$radius_fun(sizevar) else 6

    # "Unknown" throughout the popup for missing habitat/technique/year;
    # a separate, more descriptive fallback specifically for project name
    fmt_or_unknown <- function(x) ifelse(is.na(x) | (is.character(x) & !nzchar(x)), "Unknown", as.character(x))
    project_name_display <- ifelse(
      is.na(data$project_name) | !nzchar(data$project_name),
      "No project name documented", data$project_name
    )

    # whichever scaling metric is selected also gets added to the popup
    # (rows missing it are already dropped from the map at this point,
    # so no "Unknown" fallback is needed here specifically)
    scale_info <- if (input$scaleby_mv == "Project size (acres)") {
      paste0("<br>Acres Restored: ", format(round(data$area_acres, 2), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = 2))
    } else if (input$scaleby_mv == "Project size (miles)") {
      paste0("<br>Miles Restored: ", format(round(data$length_miles, 2), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = 2))
    } else if (input$scaleby_mv == "Reported project cost ($)") {
      paste0("<br>Reported Project Cost: $", format(round(data$cost_dollars), big.mark = ",", scientific = FALSE, trim = TRUE))
    } else {
      ""
    }

    popup_txt <- paste0(
      "<b>", project_name_display, "</b><br>",
      "Habitat: ", fmt_or_unknown(data$habitat), "<br>",
      "Technique(s): ", fmt_or_unknown(data$technique), "<br>",
      "Year Completed: ", fmt_or_unknown(data$year),
      scale_info
    )

    # data is already a proper sf POINT object (project_points, loaded
    # from the real shapefile in global.R) — no more on-the-fly
    # Lat/Lon-to-sf conversion needed here
    proxy <- proxy %>%
      leaflet::addCircleMarkers(
        data = data,
        radius = radius, color = "#000000", weight = 1, opacity = 1,
        fillColor = marker_color, stroke = TRUE,
        fillOpacity = 0.8, popup = popup_txt
      )

    # color legend (categorical) — independent of the size legend below,
    # so either or both can be showing depending on the two dropdowns
    if (!is.null(pal)) {
      proxy <- proxy %>% leaflet::addLegend(pal = pal, values = colorvar,
                                             title = input$colorby_mv, position = "bottomright")
    }

    # size legend (custom HTML control, since leaflet::addLegend() only
    # supports color) — placed at topright so it doesn't collide with
    # the color legend at bottomright when both are showing
    if (!is.null(qr)) {
      if (input$scaleby_mv == "Reported project cost ($)") {
        legend_html <- quartile_size_legend_fun(qr, title = input$scaleby_mv, prefix = "$")
      } else if (input$scaleby_mv == "Project size (acres)") {
        legend_html <- quartile_size_legend_fun(qr, title = input$scaleby_mv, suffix = " ac", digits = 2)
      } else {
        legend_html <- quartile_size_legend_fun(qr, title = input$scaleby_mv, suffix = " mi", digits = 2)
      }
      proxy <- proxy %>% leaflet::addControl(
        html = legend_html, position = "topright", className = "size-legend-control"
      )
    }
  })

  # rendered exactly once — see the Project Sites map above for why
  # (leafletProxy() needs the base widget to already exist client-side
  # before it can update it; suspendWhenHidden below covers the same
  # "not yet visited this tab" timing issue)
  # one map per metric, shown side by side, for BOTH Regions and
  # Counties — this single function covers both (bounds/name_field/
  # group_field/multi_value differ between them, everything else is
  # identical). Factored into a function (not a for loop) specifically
  # to avoid the classic R closure bug where a loop variable ends up
  # shared/wrong across all the reactive callbacks it creates — each
  # call here gets its own freshly-scoped arguments instead.
  setup_polygon_map <- function(output_id, bounds, name_field, group_field, multi_value,
                                 metric, legend_title, unit_suffix) {

    output[[output_id]] <- leaflet::renderLeaflet({
      bbox <- if (!is.null(bounds)) sf::st_bbox(bounds) else c(xmin = -87.7, ymin = 24.4, xmax = -79.8, ymax = 31.1)
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet::fitBounds(
          lng1 = unname(bbox["xmin"]), lat1 = unname(bbox["ymin"]),
          lng2 = unname(bbox["xmax"]), lat2 = unname(bbox["ymax"])
        )
    })
    outputOptions(output, output_id, suspendWhenHidden = FALSE)

    observe({
      # the final database gives each project a single primary county, so
      # the county maps' multi_value=TRUE (explode a comma-separated cell
      # so a project counts toward EACH county it lists) has nothing to
      # do on the current data — it's kept as a safety net in case a
      # points file ever lists several counties in one cell; region is
      # single-valued, no exploding needed. polygon_summary_fun() handles
      # the dedupe-before-summing step internally for the cost metric
      # either way.
      agg <- polygon_summary_fun(mv_data(), group_field, metric, multi_value = multi_value)
      choro <- choropleth_fun(bounds, name_field, agg, group_field, "value")

      proxy <- leaflet::leafletProxy(output_id) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls()

      if (is.null(choro)) return(NULL)

      # same 3-stop scale as the Restoration Technique x Habitat
      # hotspot table (pale mint -> teal -> navy), for visual consistency
      scale_colors <- c("#f7fcf0", "#7fcdbb", "#0c2c84")

      # when NO polygon has any data (e.g. a habitat x technique
      # combination with no projects), there's no range to build a color
      # scale from — a placeholder domain keeps colorNumeric() from
      # erroring (which, inside an observe(), would end the whole
      # session); every polygon then just shows as 0 in the "no data"
      # gray, and no legend is drawn (see below)
      legend_vals <- choro$.joinval[!is.na(choro$.joinval)]
      pal <- leaflet::colorNumeric(
        scale_colors,
        domain = if (length(legend_vals) > 0) choro$.joinval else c(0, 1),
        na.color = "#eeeeee"
      )

      if (metric == "Restoration costs") {
        value_fmt <- paste0("$", format(round(choro$.joinval), big.mark = ",", scientific = FALSE, trim = TRUE))
      } else if (metric == "Restoration projects") {
        value_fmt <- format(round(choro$.joinval), big.mark = ",", scientific = FALSE, trim = TRUE)
      } else {
        value_fmt <- format(round(choro$.joinval, 2), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = 2)
      }
      value_fmt <- paste0(value_fmt, unit_suffix)

      # NA here always means zero projects matched the current filters
      # for this polygon (a polygon with no matching rows simply never
      # appears in polygon_summary_fun()'s output, so the join leaves
      # it NA) — a genuine confirmed zero across every metric, not
      # ambiguous missing data, so it's shown as "0" rather than a
      # vague "No data" (applies to all 4 metrics, not just project count)
      value_fmt[is.na(choro$.joinval)] <- if (metric == "Restoration costs") "$0" else paste0("0", unit_suffix)

      # HTML labels (bold region/county name, bigger font) need each
      # label wrapped individually in htmltools::HTML() — a single
      # HTML() call around the whole vector doesn't render correctly
      # per-polygon
      label_txt <- lapply(
        paste0("<b>", choro[[name_field]], "</b>: ", value_fmt),
        htmltools::HTML
      )

      # legend: highest value on TOP, lowest on the BOTTOM (leaflet's own
      # numeric legend runs the other way, lowest at the top). It's built
      # from the NEGATED values with a matching reversed palette, then
      # the labels are negated back — so every value keeps its exact
      # color and its exact position on the bar, just flipped. The map
      # itself is still colored by `pal` above, unchanged.
      legend_pal <- if (length(legend_vals) > 0) {
        leaflet::colorNumeric(scale_colors, domain = -legend_vals, reverse = TRUE)
      }

      map_out <- proxy %>%
        leaflet::addPolygons(
          data = choro,
          fillColor = ~pal(.joinval), fillOpacity = 0.95,
          color = "black", weight = 1,
          label = label_txt,
          labelOptions = leaflet::labelOptions(style = list("font-size" = "14px"))
        )

      if (length(legend_vals) > 0) {
        map_out %>%
          leaflet::addLegend(pal = legend_pal, values = -legend_vals,
                              title = legend_title, position = "bottomleft",
                              labFormat = leaflet::labelFormat(transform = function(x) -x),
                              className = "info legend polygon-legend")
      }
    })
  }

  setup_polygon_map("regionmap_projects", region_bounds, REGION_NAME_FIELD, "region", FALSE,
                     "Restoration projects", "Total number of projects", " projects")
  setup_polygon_map("regionmap_cost", region_bounds, REGION_NAME_FIELD, "region", FALSE,
                    "Restoration costs", "Total reported cost ($)", "")
  setup_polygon_map("regionmap_acres", region_bounds, REGION_NAME_FIELD, "region", FALSE,
                     "Restoration footprint (area)", "Total acres restored", " acres")
  setup_polygon_map("regionmap_miles", region_bounds, REGION_NAME_FIELD, "region", FALSE,
                     "Restoration footprint (length)", "Total miles restored", " miles")

  setup_polygon_map("countymap_projects", county_bounds, COUNTY_NAME_FIELD, "county", TRUE,
                     "Restoration projects", "Total number of projects", " projects")
  setup_polygon_map("countymap_cost", county_bounds, COUNTY_NAME_FIELD, "county", TRUE,
                    "Restoration costs", "Total reported cost ($)", "")
  setup_polygon_map("countymap_acres", county_bounds, COUNTY_NAME_FIELD, "county", TRUE,
                     "Restoration footprint (area)", "Total acres restored", " acres")
  setup_polygon_map("countymap_miles", county_bounds, COUNTY_NAME_FIELD, "county", TRUE,
                     "Restoration footprint (length)", "Total miles restored", " miles")

  # ==================================================================
  # DOWNLOAD DATA
  # ==================================================================

  # The search box + filters above the preview table. The inputs are only
  # READ when "Apply" is clicked (or Enter is pressed in the search box),
  # into dl_applied(); the table, the record count note, and the download
  # all follow dl_applied(), never the live inputs. Defaults = everything:
  # no search text, empty dropdowns (shown as "All records"), and
  # "include projects with no date" checked.
  dl_default <- list(
    search = "", all_years = TRUE, years = dl_years,
    regions = character(0), habitats = character(0), techniques = character(0)
  )

  # what's currently typed/selected, applied or not
  dl_pending <- reactive({
    list(
      search     = if (is.null(input$dl_search)) "" else input$dl_search,
      all_years  = is.null(input$dl_no_year) || isTRUE(input$dl_no_year),
      years      = if (is.null(input$dl_years)) dl_years else input$dl_years,
      regions    = as.character(input$dl_regions),
      habitats   = as.character(input$dl_habitats),
      techniques = as.character(input$dl_techniques)
    )
  })

  # are two sets of criteria equivalent? (order and capitalization of the
  # search words don't matter; the year range is ignored while "include
  # projects with no date" is checked, since it isn't used then)
  dl_same <- function(a, b) {
    norm <- function(x) list(
      search     = paste(strsplit(tolower(trimws(x$search)), "\\s+")[[1]], collapse = " "),
      years      = if (isTRUE(x$all_years)) NULL else as.numeric(x$years),
      regions    = sort(unique(x$regions)),
      habitats   = sort(unique(x$habitats)),
      techniques = sort(unique(x$techniques))
    )
    identical(norm(a), norm(b))
  }

  dl_applied  <- reactiveVal(dl_default)
  dl_filtered <- reactive(filter_download_records(download_data, dl_applied()))

  observeEvent(list(input$dl_apply, input$dl_apply_enter), {
    dl_applied(dl_pending())
  }, ignoreInit = TRUE)

  # same year-slider lock as the other pages' filter boxes
  observeEvent(input$dl_no_year, {
    if (isTRUE(input$dl_no_year)) {
      updateSliderInput(session, 'dl_years', value = dl_years)
      shinyjs::disable('dl_years')
    } else {
      shinyjs::enable('dl_years')
    }
  }, ignoreInit = FALSE)

  # Reset is grayed out when there's nothing to reset — i.e. when neither
  # the current selections NOR the applied table are different from the
  # defaults. Clicking it clears every selection AND puts the table back
  # to showing every record (rather than leaving a filtered table behind
  # selections that no longer show it).
  observe(shinyjs::toggleState(
    'dl_reset',
    condition = !dl_same(dl_pending(), dl_default) || !dl_same(dl_applied(), dl_default)
  ))

  observeEvent(input$dl_reset, {
    updateTextInput(session, 'dl_search', value = "")
    updateCheckboxInput(session, 'dl_no_year', value = TRUE)
    updateSliderInput(session, 'dl_years', value = dl_years)
    shinyWidgets::updatePickerInput(session, 'dl_regions',    selected = character(0))
    shinyWidgets::updatePickerInput(session, 'dl_habitats',   selected = character(0))
    shinyWidgets::updatePickerInput(session, 'dl_techniques', selected = character(0))
    dl_applied(dl_default)
  })

  output$dltable <- reactable::renderReactable({
    req(download_data)
    d <- dl_filtered()
    reactable::reactable(
      d, pagination = TRUE, defaultPageSize = 10,
      # keep the "1-3 of 3 records" footer visible even when everything fits
      # on one page (reactable hides it then); nothing to show for no results
      showPagination = nrow(d) > 0,
      striped = TRUE, highlight = TRUE,
      defaultColDef = reactable::colDef(
        minWidth = 160,
        headerStyle = list(whiteSpace = "nowrap"),
        style = list(whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis")
      ),
      language = reactable::reactableLang(
        noData = "No records found.",
        pageInfo = "{rowStart}\u2013{rowEnd} of {rows} records"
      )
    )
  })

  # says exactly what the download button will export
  output$dl_note <- renderText({
    req(download_data)
    n <- nrow(dl_filtered()); total <- nrow(download_data)
    fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
    if (n == total) {
      paste0("Click the button below to download all ", fmt(total), " records.")
    } else if (n == 0) {
      "No records meet the applied search and filters, so there is nothing to download."
    } else {
      paste0("Click the button below to download ", fmt(n), " of ", fmt(total), " records that meet the applied search and filter criteria.")
    }
  })

  # nothing to download when no records match
  observe(shinyjs::toggleState('dwnld', condition = isTRUE(nrow(dl_filtered()) > 0)))

  # What the visitor's file is called, without the extension:
  # fl_estuarine_restoration_database_v<version> for the whole database
  # (see DOWNLOAD_NAME in global.R), or ..._subset when the applied
  # search/filters leave fewer records (judged by what the download will
  # actually contain, so a filter that happens to match every record still
  # counts as the full database).
  dl_stem <- function() {
    is_subset <- !is.null(download_data) && nrow(dl_filtered()) < nrow(download_data)
    paste0(DOWNLOAD_NAME, if (is_subset) "_subset")
  }

  # The download is a ZIP holding the data file (all records, or just the
  # filtered ones) and the metadata workbook (METADATA_FILE in global.R),
  # so the two always travel together. If this machine can't make ZIP
  # files (see ZIP_OK in global.R) it falls back to the plain CSV, and if
  # the metadata file is missing the ZIP simply holds the data alone.
  # The CSV is written by write_csv_utf8() so accents and dashes in
  # project names survive on servers running in a non-UTF-8 locale.
  output$dwnld <- downloadHandler(
    filename = function() paste0(dl_stem(), if (ZIP_OK) ".zip" else ".csv"),
    content = function(file) {
      if (!ZIP_OK) {
        write_csv_utf8(dl_filtered(), file)
        return(invisible(NULL))
      }
      work <- tempfile("fer_download_")
      dir.create(work)
      on.exit(unlink(work, recursive = TRUE), add = TRUE)

      write_csv_utf8(dl_filtered(), file.path(work, paste0(dl_stem(), ".csv")))
      if (file.exists(METADATA_FILE)) {
        file.copy(METADATA_FILE, file.path(work, basename(METADATA_FILE)))
      }
      make_zip(file, list.files(work, full.names = TRUE))
    }
  )


  # ==================================================================
  # CONTRIBUTE
  # ==================================================================
  # Nothing to do here: the two Contribute forms are embedded Google Forms
  # (see google_form_embed() in funcs.R and the FORM_* values in global.R).
  # Saving submissions, the email notification and spam handling are all
  # Google's.
}

shinyApp(ui, server)
