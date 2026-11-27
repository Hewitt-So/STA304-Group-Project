# =====================================================================
# STA304H5F (Fall 2026) - Group 30
# Interactive Shiny app: Group vs. Individual Study Preferences
# Reads the cleaned survey data from Dataset_clean.csv.
# =====================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------- data
# Load the team's cleaned survey file.
d <- read.csv("Dataset_clean.csv", check.names = FALSE, na.strings = c("NA", "N/A", ""))

# Normalize headers and character values from the source export.
names(d) <- trimws(names(d))
d[] <- lapply(d, function(x) if (is.character(x)) trimws(x) else x)

# Recode to readable levels.
lvl_method <- c(Group = "G", Individual = "I", `No preference` = "N")
lvl_yn     <- c(Yes = "Y", No = "N")
ethnicity_choices <- c(
  "South Asian" = "ethnicity_south_asian",
  "Chinese" = "ethnicity_chinese",
  "White" = "ethnicity_white",
  "Southeast Asian" = "ethnicity_southeast_asian",
  "Latin American" = "ethnicity_latin_american",
  "West Asian" = "ethnicity_west_asian",
  "Filipino" = "ethnicity_filipino",
  "Black" = "ethnicity_black",
  "Taiwanese" = "ethnicity_taiwanese"
)

d <- d |>
  mutate(
    preferred_method = factor(preferred_method, levels = lvl_method,
                              labels = names(lvl_method)),
    gender     = factor(gender, levels = c(M = "M", F = "F", O = "O"),
                        labels = c("Male", "Female", "Other")),
    campus     = factor(campus, levels = lvl_yn, labels = names(lvl_yn)),
    domestic   = factor(domestic, levels = lvl_yn, labels = names(lvl_yn)),
    across(c(learning_new_concepts, solving_problems, readings, reviewing),
           ~ factor(.x, levels = lvl_method, labels = names(lvl_method))),
    # cgpa holds the midpoint of the chosen GPA range; "N/A" -> missing
    cgpa            = suppressWarnings(as.numeric(cgpa)),
    know_well       = as.numeric(know_well),
    group_difficult = as.numeric(group_difficult)
  ) |>
  droplevels()

N_TOTAL <- nrow(d)

# ---------------------------------------------------------- style tokens
TEAL   <- "#0F766E"   # accent
INK    <- "#111827"   # near-black text
MUTED  <- "#6B7280"   # secondary text
LINE   <- "#E5E7EB"   # hairlines
SOFT   <- "#F8FAFC"   # very light panel tint
GRID   <- "#F1F5F9"   # plot gridlines

app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = INK,
  primary = TEAL,
  base_font = font_collection("Inter", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
  heading_font = font_collection("Georgia", "Cambria", "serif")
)

app_css <- '
  body { color: #1F2937; background: #F4F8F7; }

  /* --- centered content column --- */
  .page-wrap { max-width: 1200px; margin: 0 auto; padding: 1.4rem 1.2rem 3rem; }

  /* --- header --- */
  .page-header { background: #113B3A; color: #FFFFFF; border-radius: 16px;
                 padding: clamp(1.35rem, 4vw, 2.75rem); margin-bottom: 1.2rem; }
  .page-kicker { color: #B5E8D9; font-weight: 650; font-size: .78rem;
                 letter-spacing: .14em; text-transform: uppercase; }
  .page-title { color: #FFFFFF; font-weight: 700; font-size: clamp(2rem, 5vw, 3.3rem);
                letter-spacing: -.025em; margin: .35rem 0 .55rem; }
  .page-sub { color: #E0F2EE; max-width: 64ch; margin-bottom: 0; }
  .filters-title { font-size: 1rem; font-weight: 700; margin: 0 0 .55rem; }
  .sample-note { margin: -.55rem 0 1.2rem; font-size: .9rem; color: #475569; }

  /* --- control bars --- */
  .control-strip { display: flex; flex-wrap: wrap; gap: .8rem 1.4rem;
                   align-items: flex-end;
                   background: #FFFFFF; border: 1px solid #E5E7EB;
                   border-radius: 12px; box-shadow: 0 1px 2px rgba(16,24,40,.05);
                   padding: .95rem 1.1rem; margin: .4rem 0 1.5rem; }
  .control-strip .form-group { margin-bottom: 0; }
  .control-strip .ctrl { flex: 1 1 175px; min-width: 0; }
  .control-strip .ctrl-narrow { flex: 0 0 auto; }
  .control-strip .n-label { color: #475569; font-size: .9rem; padding-bottom: .35rem; }
  #chart-controls { display: grid; grid-template-columns: repeat(auto-fit, minmax(min(220px, 100%), 1fr)); }
  .control-inline { display: flex; flex-wrap: wrap; gap: .8rem 1.6rem; }
  .control-inline .form-group { margin-bottom: 0; }
  .control-inline .ctrl { flex: 1 1 190px; min-width: 0; }
  .chart-options { max-width: 640px; }
  .chart-options .form-switch { padding-top: .35rem; }
  .colour-options { margin: .9rem 0 .4rem; }
  .colour-options h3 { font-size: .94rem; font-weight: 650; margin-bottom: .55rem; }
  .colour-swatches { display: flex; flex-wrap: wrap; gap: .65rem; }
  .colour-swatch { display: flex; align-items: center; gap: .5rem; border: 1px solid #D8E3E0;
                   border-radius: 8px; padding: .3rem .55rem; min-width: 0; }
  .colour-swatch label { font-size: .87rem; margin: 0; }
  .colour-swatch input { width: 2.3rem; height: 1.85rem; padding: .14rem; cursor: pointer; }
  .data-browser { min-width: 0; }
  .data-browser table.dataTable th, .data-browser table.dataTable td { white-space: nowrap; }
  .data-browser .dataTables_filter { margin-bottom: .55rem; }
  .axis-controls { display: flex; flex-wrap: wrap; align-items: flex-end;
                   gap: .85rem 1.5rem; margin: .65rem 0 .9rem; }
  .axis-range { flex: 1 1 225px; min-width: 0; max-width: 310px; }
  .axis-range legend { font-size: .94rem; font-weight: 650; margin-bottom: .3rem; }
  .axis-fields { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: .6rem; }
  .axis-fields .form-group { margin-bottom: 0; }
  .axis-fields label { font-size: .88rem; }
  .axis-help { margin: .9rem 0 .35rem; color: #475569; font-size: .9rem; }
  .axis-status { color: #475569; font-size: .88rem; }
  .axis-status:empty { display: none; }

  /* --- inputs & buttons --- */
  .form-select, .form-control, .btn { border-radius: 8px; }
  .form-select:focus, .form-control:focus {
    border-color: #0F766E; box-shadow: 0 0 0 .18rem rgba(15,118,110,.15); }
  .btn { font-weight: 600; }
  .irs--shiny .irs-bar { background: #0F766E; border-top: #0F766E; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background: #0F766E; }

  /* --- pill tabs --- */
  .nav-pills { margin: .3rem 0 1.1rem; border-bottom: 1px solid #CBD5D1;
               padding-bottom: .6rem; gap: .25rem; }
  .nav-pills .nav-link { color: #334155; border-radius: 999px; padding: .42rem 1rem; }
  .nav-pills .nav-link.active { background: #0F766E; }
  .nav-pills .nav-link:hover:not(.active) { color: #0F766E; background: #F8FAFC; }

  /* --- section headings --- */
  .panel { background: #FFFFFF; border: 1px solid #D8E3E0; border-radius: 14px;
           padding: clamp(.75rem, 2.5vw, 1.5rem); margin: 0 0 1.1rem; min-width: 0; }
  .section-title { font-size: 1.28rem; font-weight: 700; color: #111827;
                   margin: 0 0 .75rem; }
  .panel p:last-child, .panel ul:last-child { margin-bottom: 0; }
  .plot-note { color: #475569; font-size: .88rem; min-height: 1.35em; }
  .results-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr));
                  gap: 1rem; }
  .results-grid .panel { margin-bottom: 0; }
  .shiny-output-error-validation { color: #475569; }
  .shiny-plot-output img { max-width: 100%; height: auto !important; }
  #plot1 { height: clamp(340px, 52vw, 480px) !important; }
  @media (max-width: 700px) {
    .results-grid { grid-template-columns: minmax(0, 1fr); }
    .control-strip .ctrl:not(.ctrl-narrow) { flex-basis: 125px; }
    .nav-pills .nav-link { padding: .42rem .7rem; }
  }
  p, li { max-width: 72ch; color: #374151; }
  .btn-primary { background: #0F766E; border-color: #0F766E; }
'

plot_theme <- theme_minimal(base_size = 13.5) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = GRID, linewidth = 0.45),
    panel.grid.minor = element_blank(),
    text        = element_text(colour = "#1F2937"),
    axis.text   = element_text(colour = "#475569"),
    axis.title  = element_text(colour = INK, face = "bold", size = 11),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(8, 10, 8, 7),
    strip.background = element_rect(fill = SOFT, colour = LINE),
    strip.text  = element_text(colour = "#374151", face = "bold"),
    legend.position = "bottom"
  )

# --------------------------------------------------------- plot config
plot_vars <- c(
  "Overall study preference"          = "preferred_method",
  "Learning new concepts"             = "learning_new_concepts",
  "Solving practice problems"         = "solving_problems",
  "Assigned readings"                 = "readings",
  "Reviewing for tests/exams"         = "reviewing",
  "One-way commute time (minutes)"    = "commute_time",
  "Need to know study partners well (1-7)" = "know_well",
  "Group study for difficult topics (1-7)" = "group_difficult",
  "Approximate CGPA"                  = "cgpa",
  "Gender"                            = "gender",
  "Lived on campus"                   = "campus",
  "Domestic student"                  = "domestic",
  "Ethnicity"                         = "ethnicity"
)
numeric_vars <- c("commute_time", "know_well", "group_difficult", "cgpa")
ordinal_vars <- c("know_well", "group_difficult")
numeric_choices <- plot_vars[plot_vars %in% numeric_vars]
group_choices <- plot_vars[!plot_vars %in% numeric_vars]
chart_types <- c("Bar chart" = "bar", "Histogram" = "histogram",
                 "Boxplot" = "boxplot", "Scatterplot" = "scatter",
                 "Linear regression" = "regression", "100% stacked bar" = "stacked")

plot_label <- function(v) names(plot_vars)[match(v, unname(plot_vars))]
scatter_choices <- function(y) numeric_choices[numeric_choices != y]
stack_choices <- function(x) group_choices[!group_choices %in% c(x, "ethnicity")]

category_data <- function(df, variable) {
  if (variable == "ethnicity") {
    return(df |>
      pivot_longer(all_of(unname(ethnicity_choices)), names_to = "ethnicity_code",
                   values_to = "ethnicity_selected") |>
      filter(ethnicity_selected == 1) |>
      mutate(category = factor(ethnicity_code, levels = unname(ethnicity_choices),
                               labels = names(ethnicity_choices))))
  }
  if (variable == "commute_time") {
    return(mutate(df, category = cut(commute_time, breaks = pretty(d$commute_time, n = 6),
                                     include.lowest = TRUE)))
  }
  if (variable == "cgpa") {
    return(mutate(df, category = factor(cgpa, levels = c(1, 2.345, 2.995, 3.495, 3.85),
      labels = c("Below 2.00", "2.00-2.69", "2.70-3.29", "3.30-3.69", "3.70-4.00"))))
  }
  mutate(df, category = .data[[variable]])
}

activity_vars  <- c("learning_new_concepts", "solving_problems", "readings", "reviewing")
activity_choices <- plot_vars[unname(plot_vars) %in% activity_vars]
method_levels <- names(lvl_method)
method_vars <- c("preferred_method", activity_vars)

clean_table <- function(rows) {
  flags <- as.matrix(rows[unname(ethnicity_choices)])
  rows$ethnicity <- vapply(seq_len(nrow(rows)), function(i) {
    paste(names(ethnicity_choices)[which(flags[i, ] == 1)], collapse = ", ")
  }, character(1))
  fields <- c("preferred_method", "gender", "ethnicity", "commute_time",
              "campus", "domestic", "cgpa", activity_vars, ordinal_vars)
  table <- rows[fields]
  names(table) <- plot_label(fields)
  table
}

palettes <- c(
  "Okabe-Ito (colourblind-friendly)" = "okabe",
  "Teal" = "pine",
  "Viridis (colourblind-friendly)"   = "viridis",
  "Set2" = "set2",
  "Ocean" = "ocean",
  "Earth tones" = "earth",
  "Berry" = "berry",
  "Custom colours" = "custom"
)
pine <- c("#0F766E", "#F59E0B", "#64748B", "#DC2626", "#84CC16", "#3B82F6", "#A855F7",
          "#DB2777", "#9A3412")
okabe <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442",
           "#543889", "#606060")

palette_colors <- function(pal) {
  switch(pal,
    pine = pine,
    okabe = okabe,
    viridis = substr(viridisLite::viridis(9), 1, 7),
    set2 = c(RColorBrewer::brewer.pal(8, "Set2"), "#6B7393"),
    ocean = c("#005F73", "#0A9396", "#3986B4", "#2B527D", "#62C7B5", "#797CAA", "#275C54", "#80A5CC", "#4C6676"),
    earth = c("#854D32", "#526644", "#BC8A41", "#9C5C6C", "#647A86", "#C26F4E", "#808153", "#542F2C", "#C9AA80"),
    berry = c("#632A64", "#AA4074", "#58429A", "#DC6985", "#782D55", "#9D70B5", "#C34957", "#3F4A93", "#8B6576"),
    custom = okabe
  )
}
method_colors <- function(colors) {
  setNames(colors[seq_along(method_levels)], method_levels)
}
chosen_colour <- function(value, fallback) {
  if (isTRUE(grepl("^#[0-9a-fA-F]{6}$", value))) value else fallback
}
format_pct <- scales::label_percent(accuracy = 0.1)
p_text <- function(x) format.pval(x, digits = 3, eps = .001)
wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  p <- k / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(lower = max(0, centre - half), upper = min(1, centre + half))
}

# ------------------------------------------------------ layout helpers
ctrl_strip <- function(...) div(class = "control-strip", ...)
ctrl <- function(..., narrow = FALSE) {
  div(class = if (narrow) "ctrl ctrl-narrow" else "ctrl", ...)
}
panel <- function(title, ...) {
  tags$section(class = "panel", h2(title, class = "section-title"), ...)
}

colour_swatch <- function(id, label, value) {
  div(class = "colour-swatch",
      tags$label(`for` = id, label),
      tags$input(id = id, type = "color", value = value,
                 class = "form-control form-control-color", `data-chart-colour` = "true"))
}

axis_bound_input <- function(id, label) {
  htmltools::tagQuery(numericInput(id, label, value = NA_real_, width = "100%"))$
    find("input")$removeAttrs("value")$addAttrs(placeholder = "Auto", step = "any")$allTags()
}

axis_fields <- function(axis, title) {
  tags$fieldset(class = "axis-range", tags$legend(title),
    div(class = "axis-fields",
      axis_bound_input(paste0(axis, "_min"), paste(toupper(axis), "minimum")),
      axis_bound_input(paste0(axis, "_max"), paste(toupper(axis), "maximum"))
    )
  )
}

plot_window <- function(values, defaults, axis, unit) {
  if (all(is.na(values))) return(list())
  set <- values[!is.na(values)]
  problem <- if (any(!is.finite(set))) "Enter finite axis limits."
    else if (unit == "count" && any(set < 0 | set != floor(set))) {
      "Count limits must be nonnegative whole numbers."
    } else if (unit == "percent" && any(set < 0 | set > 100)) {
      "Percentage limits must be between 0 and 100."
    }
  if (!is.null(problem)) return(list(message = paste(axis, "axis.", problem)))

  if (unit == "percent") values <- values / 100
  bounds <- replace(values, is.na(values), defaults[is.na(values)])
  if (bounds[1] >= bounds[2]) {
    return(list(message = paste(axis, "minimum must be below the maximum. Adjust or clear one of the limits.")))
  }
  list(bounds = bounds)
}

# ------------------------------------------------------------------ ui
ui <- page_fluid(
  title = "How STA304 students study",
  theme = app_theme,
  lang = "en",
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML('
      document.addEventListener("change", function(event) {
        const picker = event.target;
        if (picker instanceof HTMLInputElement && picker.hasAttribute("data-chart-colour")) {
          Shiny.setInputValue(picker.id, picker.value);
        }
      });
    '))
  ),
  tags$main(class = "page-wrap",
    tags$header(class = "page-header",
      div(class = "page-kicker", "STA304H5F / Fall 2026 / Group 30"),
      h1("How STA304 students study", class = "page-title"),
      p("Explore when survey respondents prefer studying in a group or on their own.",
        class = "page-sub")
    ),
    h2("Filter responses", class = "filters-title"),
    ctrl_strip(
      ctrl(selectInput("gender", "Gender", c("All", levels(d$gender)), width = "100%")),
      ctrl(selectInput("campus", "Lived on campus", c("All", "Yes", "No"), width = "100%")),
      ctrl(selectInput("domestic", "Domestic student", c("All", "Yes", "No"), width = "100%")),
      ctrl(selectInput("ethnicity", "Ethnicity", c(All = "All", ethnicity_choices), width = "100%")),
      ctrl(actionButton("reset", "Reset filters", class = "btn-outline-secondary btn-sm"),
           narrow = TRUE),
      ctrl(div(class = "n-label", role = "status", textOutput("n_showing")), narrow = TRUE)
    ),
    p("The survey was voluntary. Results describe the respondents in the current selection.",
      class = "sample-note"),
    navset_pill(
      id = "view",
      nav_panel(
        "Explore", value = "explore",
        ctrl_strip(
          id = "chart-controls",
          ctrl(selectInput("plot_type", "Chart", chart_types, width = "100%")),
          conditionalPanel("['bar', 'stacked'].includes(input.plot_type)",
            class = "ctrl",
            selectInput("plot_var", "Variable", plot_vars, width = "100%")
          ),
          conditionalPanel("input.plot_type === 'histogram'",
            class = "ctrl",
            selectInput("hist_var", "X-axis variable", numeric_choices, width = "100%")
          ),
          conditionalPanel("input.plot_type === 'boxplot'",
            class = "ctrl",
            selectInput("box_group", "Group by", group_choices, width = "100%")
          ),
          conditionalPanel("['scatter', 'regression'].includes(input.plot_type)",
            class = "ctrl",
            selectInput("scatter_x", "X-axis variable", scatter_choices("commute_time"),
                        selected = "cgpa", width = "100%")
          ),
          conditionalPanel("['boxplot', 'scatter', 'regression'].includes(input.plot_type)",
            class = "ctrl",
            selectInput("plot_y", "Y-axis variable", numeric_choices, width = "100%")
          ),
          conditionalPanel("input.plot_type === 'stacked'",
            class = "ctrl",
            selectInput("stack_fill", "Fill by", stack_choices("preferred_method"),
                        selected = "gender", width = "100%")
          )
        ),
        panel("Chart settings",
          div(class = "control-inline chart-options",
            ctrl(selectInput("palette", "Colour palette", palettes, width = "100%")),
            conditionalPanel("input.plot_type === 'bar'", class = "ctrl",
              input_switch("different_bars", "Different colours for each bar")
            ),
            conditionalPanel("input.plot_type === 'histogram'",
              class = "ctrl",
              sliderInput("bins", "Histogram bins", min = 2, max = 40,
                          value = 12, step = 1, width = "100%")
            )
          ),
          conditionalPanel("input.palette === 'custom' && !(input.plot_type === 'bar' && input.different_bars)",
            class = "colour-options",
            div(class = "colour-swatches",
              colour_swatch("explore_colour1", "Primary colour", okabe[1]),
              conditionalPanel(paste0(
                "['stacked', 'scatter', 'regression'].includes(input.plot_type)",
                " || (input.plot_type === 'boxplot' && ",
                jsonlite::toJSON(method_vars), ".includes(input.box_group))"
              ),
                div(class = "colour-swatches",
                  colour_swatch("explore_colour2", "Second colour", okabe[2]),
                  colour_swatch("explore_colour3", "Third colour", okabe[3])
                )
              ),
              conditionalPanel("input.plot_type === 'regression'",
                colour_swatch("line_colour", "Regression line", INK)
              )
            )
          ),
          conditionalPanel("input.palette === 'custom' && input.plot_type === 'bar' && input.different_bars",
            class = "colour-options", uiOutput("bar_colour_inputs")
          ),
          p("Set a minimum or maximum to zoom. Leave a field blank to use the automatic limit.",
            class = "axis-help"),
          div(class = "axis-controls",
            conditionalPanel("output.x_axis_available === 'yes'", class = "axis-range",
                             axis_fields("x", "X-axis limits")),
            axis_fields("y", textOutput("y_axis_title", inline = TRUE)),
            actionButton("reset_axes", "Reset scales", class = "btn-outline-secondary btn-sm")
          ),
          textOutput("scale_note", container = function(...) div(class = "axis-status", role = "status", ...))
        ),
        panel("Response chart", plotOutput("plot1", height = "480px")),
        panel("What the responses show", uiOutput("interp1"))
      ),
      nav_panel(
        "Activities", value = "activities",
        ctrl_strip(
          ctrl(selectInput("palette2", "Colour palette", palettes, width = "100%")),
          ctrl(selectInput("activity_group", "Overall study preference",
                           c("All", levels(d$preferred_method)),
                           width = "100%"))
        ),
        conditionalPanel("input.palette2 === 'custom'",
          panel("Activity colours",
            div(class = "colour-swatches",
              colour_swatch("activity_colour1", "Group", okabe[1]),
              colour_swatch("activity_colour2", "Individual", okabe[2]),
              colour_swatch("activity_colour3", "No preference", okabe[3])
            )
          )
        ),
        panel("When students choose group study",
              plotOutput("plot_act_all", height = "340px"), uiOutput("interp_act"),
              p("Whiskers show 95% Wilson intervals for each share. The same students answered multiple activity questions.",
                class = "plot-note")),
        panel("How students answered each activity question",
              div(class = "table-responsive", tableOutput("activity_table"))),
        panel("Compare overall preferences",
              p("Each panel groups respondents by their overall answer. Each bar shows how those students answered one activity question."),
              plotOutput("plot_act_by", height = "480px"))
      ),
      nav_panel(
        "Statistical tests", value = "tests",
        p("These exploratory tests and models use the selected survey responses."),
        div(class = "results-grid",
          panel("Overall preference", uiOutput("prop_test")),
          panel("Overall and activity preferences",
                selectInput("test_activity", "Activity", activity_choices,
                            selected = "solving_problems", width = "100%"),
                uiOutput("chi_test")),
          panel("Linear regression",
                selectInput("lm_y", "Outcome (Y)", numeric_choices[numeric_choices %in% ordinal_vars],
                            selected = "group_difficult", width = "100%"),
                uiOutput("lm_test")),
          panel("Odds of preferring group study", uiOutput("logit_test"))
        )
      ),
      nav_panel(
        "Data", value = "data",
        panel("Cleaned survey responses",
          p(paste("Search responses or select a column heading to sort.",
                  "Scroll sideways to see all columns. The demographic filters above also apply here.")),
          div(class = "data-browser", DT::DTOutput("clean_data"))
        )
      ),
      nav_panel(
        "About", value = "about",
        panel("How we collected the data",
              p(sprintf(paste("Group 30 posted a Google Form on the STA304H5F Piazza page at the University of Toronto Mississauga.",
                              "We received %d responses in a class of roughly 300 students. Respondents could enter a draw for a $20 gift card."),
                        N_TOTAL)),
              p("Students volunteered after seeing the Piazza post. Students who visited Piazza more often or were especially interested in study habits may be over-represented.")),
        panel("How to interpret the data",
              tags$ul(
                tags$li("Approximate CGPA uses the midpoints of self-reported ranges. ",
                        sum(is.na(d$cgpa)), " respondents did not report a CGPA."),
                tags$li("Activity questions allow three answers. Small counts for No preference can make comparisons imprecise."),
                tags$li("The survey asked about choosing group study for difficult topics on a 1 to 7 scale. This rating is about study preferences for difficult material."),
                tags$li("Respondents could select multiple ethnicities. The filter includes anyone who selected that ethnicity. Ethnicity charts count each person in every group they selected."),
                tags$li("The cleaned survey file omits student IDs.")
              ))
      )
    )
  )
)

server <- function(input, output, session) {
  no_matches <- "No students match these filters. Broaden or reset the filters to see results."
  no_responses <- "No usable responses are available for this selection."

  d_f <- reactive({
    req(input$gender, input$campus, input$domestic, input$ethnicity)
    df <- d
    if (input$gender != "All") df <- filter(df, gender == input$gender)
    if (input$campus != "All") df <- filter(df, campus == input$campus)
    if (input$domestic != "All") df <- filter(df, domestic == input$domestic)
    if (input$ethnicity != "All") df <- filter(df, .data[[input$ethnicity]] == 1)
    df
  })

  output$clean_data <- DT::renderDT({
    DT::datatable(clean_table(d_f()), rownames = FALSE, escape = TRUE, class = "compact stripe hover",
      options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, N_TOTAL),
        order = list(), language = list(emptyTable = "No responses match the selected filters.")))
  }, server = FALSE)

  palette_for <- function(palette, prefix) {
    palette <- req(palette)
    colors <- palette_colors(palette)
    if (palette == "custom") {
      for (i in seq_along(method_levels)) {
        colors[i] <- chosen_colour(input[[paste0(prefix, i)]], colors[i])
      }
    }
    colors
  }
  explore_colors <- reactive(palette_for(input$palette, "explore_colour"))
  activity_colors <- reactive(palette_for(input$palette2, "activity_colour"))

  bar_categories <- reactive({
    category_data(d, req(input$plot_var)) |>
      filter(!is.na(category)) |>
      distinct(category) |>
      arrange(category) |>
      pull(category) |>
      as.character()
  })
  bar_picker_ids <- reactive({
    paste0("bar_colour_", req(input$plot_var), "_", seq_along(bar_categories()))
  })
  bar_colors <- reactive({
    labels <- bar_categories()
    colors <- explore_colors()[seq_along(labels)]
    if (input$palette == "custom") {
      ids <- bar_picker_ids()
      colors <- vapply(seq_along(ids), function(i) chosen_colour(input[[ids[i]]], colors[i]),
                       character(1))
    }
    setNames(colors, labels)
  })

  output$bar_colour_inputs <- renderUI({
    req(input$plot_type == "bar", input$palette == "custom", isTRUE(input$different_bars))
    ids <- bar_picker_ids()
    colors <- isolate(bar_colors())
    tagList(
      h3("Bar colours"),
      div(class = "colour-swatches", lapply(seq_along(ids), function(i) {
        colour_swatch(ids[i], names(colors)[i], colors[i])
      }))
    )
  })

  observeEvent(input$reset, {
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "campus", selected = "All")
    updateSelectInput(session, "domestic", selected = "All")
    updateSelectInput(session, "ethnicity", selected = "All")
  })

  observeEvent(input$plot_y, {
    xs <- scatter_choices(input$plot_y)
    x <- if (isTRUE(input$scatter_x %in% xs)) input$scatter_x else xs[[1]]
    updateSelectInput(session, "scatter_x", choices = xs, selected = x)
  }, ignoreInit = TRUE)

  observeEvent(input$plot_var, {
    choices <- stack_choices(input$plot_var)
    fill <- if (isTRUE(input$stack_fill %in% choices)) input$stack_fill else choices[[1]]
    updateSelectInput(session, "stack_fill", choices = choices, selected = fill)
  }, ignoreInit = TRUE)

  chart_axes <- reactive({
    type <- req(input$plot_type)
    count <- type %in% c("bar", "histogram")
    y <- if (count) "count" else if (type == "stacked") "percent" else req(input$plot_y)
    x <- if (type %in% c("scatter", "regression")) {
      choices <- scatter_choices(y)
      if (isTRUE(input$scatter_x %in% choices)) input$scatter_x else choices[[1]]
    } else if (type == "histogram") {
      req(input$hist_var)
    } else if (type != "boxplot" && isTRUE(input$plot_var %in% ordinal_vars)) {
      input$plot_var
    }
    list(x = x, y = y)
  })

  output$x_axis_available <- renderText(if (is.null(chart_axes()$x)) "no" else "yes")
  outputOptions(output, "x_axis_available", suspendWhenHidden = FALSE)
  output$y_axis_title <- renderText({
    switch(chart_axes()$y, count = "Y-axis limits (counts)",
           percent = "Y-axis limits (0-100%)", "Y-axis limits")
  })
  outputOptions(output, "y_axis_title", suspendWhenHidden = FALSE)

  clear_axis <- function(axis) {
    updateNumericInput(session, paste0(axis, "_min"), value = "")
    updateNumericInput(session, paste0(axis, "_max"), value = "")
  }
  previous_axes <- reactiveVal(NULL)
  observeEvent(chart_axes(), {
    axes <- chart_axes()
    previous <- previous_axes()
    if (!is.null(previous)) {
      if (!identical(axes$x, previous$x)) clear_axis("x")
      if (!identical(axes$y, previous$y)) clear_axis("y")
    }
    previous_axes(axes)
  })
  observeEvent(input$reset_axes, {
    clear_axis("x")
    clear_axis("y")
  })

  output$n_showing <- renderText({
    paste0(format(nrow(d_f())), " of ", N_TOTAL, " responses shown")
  })

  explore <- reactive({
    df <- d_f()
    validate(need(nrow(df) > 0, no_matches))
    type <- req(input$plot_type)
    relation <- type %in% c("scatter", "regression")
    axes <- chart_axes()
    v <- if (type == "histogram") axes$x else if (relation || type == "boxplot") axes$y else req(input$plot_var)
    group <- if (type == "boxplot") req(input$box_group) else v
    columns <- c(v, group)
    if (relation) {
      x <- axes$x
      columns <- c(columns, x, "preferred_method")
    }
    if (type == "stacked") {
      choices <- stack_choices(v)
      fill <- if (isTRUE(input$stack_fill %in% choices)) input$stack_fill else choices[[1]]
      columns <- c(columns, fill)
    }
    columns <- setdiff(columns, "ethnicity")
    if (length(columns) > 0) df <- drop_na(df, all_of(columns))
    if (group == "ethnicity") {
      df <- filter(df, if_any(all_of(unname(ethnicity_choices)), ~ .x == 1))
    }
    validate(need(nrow(df) > 0, no_responses))
    colors <- explore_colors()
    first_col <- colors[1]
    method_col <- method_colors(colors)
    label <- plot_label(v)
    count_axis <- axes$y == "count"
    subtitle <- sprintf("%d responses", nrow(df))
    slope <- NULL
    box_stats <- NULL
    cnt <- NULL

    if (!relation && type != "histogram") {
      grouped <- category_data(df, group)
      group_label <- switch(group, commute_time = paste(label, "(grouped)"),
                            cgpa = "Self-reported CGPA range", plot_label(group))
      method_groups <- group %in% method_vars
    }

    if (type == "histogram") {
      p <- ggplot(df, aes(x = .data[[v]])) +
        geom_histogram(bins = req(input$bins), fill = first_col, colour = "white")
    } else if (type == "boxplot") {
      box_stats <- summarise(grouped, n = n(), median = median(.data[[v]]), .by = category) |>
        arrange(category)
      p <- ggplot(grouped, aes(x = category, y = .data[[v]], fill = category)) +
        geom_boxplot(width = 0.55, outlier.shape = NA, alpha = .78) +
        geom_point(position = position_jitter(width = .12, height = 0, seed = 30),
                   alpha = .45, colour = INK) +
        scale_fill_manual(values = if (method_groups) method_col else rep(first_col, nrow(box_stats)),
                          guide = "none") +
        labs(x = group_label, y = label)
    } else if (relation) {
      validate(need(nrow(df) >= 2, "A scatterplot needs at least two complete responses."))
      p <- ggplot(df, aes(x = .data[[x]], y = .data[[v]], colour = preferred_method)) +
        geom_point(alpha = .72, size = 2.5) +
        scale_colour_manual(values = method_col) +
        labs(x = plot_label(x), y = label, colour = "Overall preference")
      if (type == "regression") {
        validate(need(nrow(df) >= 3 && n_distinct(df[[x]]) > 1,
                      "A regression needs at least three complete responses and two distinct X values."))
        fit <- lm(y ~ x, data = data.frame(x = df[[x]], y = df[[v]]))
        slope <- unname(coef(fit)[2])
        ends <- range(df[[x]])
        fitted <- predict(fit, newdata = data.frame(x = ends))
        line_col <- if (input$palette == "custom") chosen_colour(input$line_colour, INK) else INK
        p <- p + annotate("segment", x = ends[1], xend = ends[2],
                          y = fitted[1], yend = fitted[2], colour = line_col, linewidth = .85)
      }
    } else if (type == "stacked") {
      breakdown <- count(grouped, category, response = .data[[fill]])
      fill_levels <- levels(d[[fill]])
      fill_colors <- setNames(colors[seq_along(fill_levels)], fill_levels)
      if (fill %in% method_vars) fill_colors <- method_col
      p <- ggplot(breakdown, aes(x = category, y = n, fill = response)) +
        geom_col(position = "fill", colour = "white", width = .7) +
        scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = 0)) +
        scale_fill_manual(values = fill_colors) +
        labs(x = group_label, y = "Share within each bar", fill = plot_label(fill))
    } else {
      cnt <- count(grouped, category)
      p <- ggplot(cnt, aes(x = category, y = n))
      if (isTRUE(input$different_bars)) {
        p <- p + geom_col(aes(fill = factor(category)), width = .68) +
          scale_fill_manual(values = bar_colors(), guide = "none")
      } else {
        p <- p + geom_col(fill = first_col, width = .68)
      }
      p <- p + geom_text(aes(label = n), vjust = -.55, size = 3.6)
    }

    if (count_axis) {
      p <- p + labs(x = if (type == "histogram") label else group_label, y = "Number of students")
    }
    if (!relation && type != "histogram" && is.factor(grouped$category)) {
      axis_labels <- if (group == "ethnicity") waiver() else scales::label_wrap(13)
      p <- p + scale_x_discrete(labels = axis_labels,
                                guide = guide_axis(angle = if (group == "ethnicity") 55 else 0))
    }
    if (v %in% ordinal_vars && type %in% c("bar", "stacked")) {
      p <- p + scale_x_continuous(breaks = 1:7, limits = c(.5, 7.5))
    }
    p <- p + labs(title = label, subtitle = subtitle) + plot_theme +
      theme(plot.title = element_text(hjust = 0, lineheight = .95))
    p$labels$title <- paste(strwrap(label, width = 34), collapse = "\n")
    p$labels$x <- paste(strwrap(p$labels$x, width = 34), collapse = "\n")
    if (!count_axis) {
      p$labels$y <- paste(strwrap(p$labels$y, width = 31), collapse = "\n")
    }
    list(plot = p, count_axis = count_axis, type = type, variable = v, data = df,
         counts = cnt, boxes = box_stats, slope = slope, group = group,
         comparison = if (relation) x else if (type == "stacked") fill else NULL)
  })

  base_plot <- reactive({
    chart <- explore()
    p <- chart$plot
    if (chart$count_axis) {
      p <- p + expand_limits(y = 0) +
        scale_y_continuous(expand = expansion(mult = c(0, .14)),
        breaks = function(limits) unique(round(scales::breaks_pretty(n = 5)(limits))))
    }
    p
  })

  axis_zoom <- reactive({
    axes <- chart_axes()
    bounds <- function(minimum, maximum) {
      vapply(list(minimum, maximum), function(value) if (length(value)) value else NA_real_, numeric(1))
    }
    x <- if (is.null(axes$x)) c(NA_real_, NA_real_) else bounds(input$x_min, input$x_max)
    y <- bounds(input$y_min, input$y_max)
    if (all(is.na(c(x, y)))) return(list())

    built <- ggplot_build(base_plot())
    defaults <- built$layout$panel_params[[1]]
    x_view <- plot_window(x, defaults$x.range, "X", "numeric")
    y_view <- plot_window(y, defaults$y.range, "Y", axes$y)
    message <- c(x_view$message, y_view$message)
    if (axes$y == "count" && length(y_view$bounds)) {
      counts <- built$data[[1]]$y
      if (any(counts < y_view$bounds[1] | counts > y_view$bounds[2], na.rm = TRUE)) {
        message <- c(message, "Some counts fall outside the selected Y limits.")
      }
    }
    list(x = x_view$bounds, y = y_view$bounds, message = message)
  })

  output$plot1 <- renderPlot({
    p <- base_plot()
    view <- axis_zoom()
    auto_x <- is.null(view$x)
    auto_y <- is.null(view$y)
    if (auto_x && auto_y) return(p)
    p + coord_cartesian(xlim = view$x, ylim = view$y,
                        expand = c(top = auto_y, bottom = auto_y, left = auto_x, right = auto_x))
  }, alt = "Chart of the selected survey variable. A written summary follows the chart.")

  output$scale_note <- renderText({
    paste(axis_zoom()$message, collapse = " ")
  })
  outputOptions(output, "scale_note", suspendWhenHidden = FALSE)

  output$interp1 <- renderUI({
    chart <- explore()
    df <- chart$data
    v <- chart$variable
    n <- nrow(df)
    missing <- nrow(d_f()) - n
    note <- if (missing > 0) p(sprintf("%d selected responses lack a value needed for this chart.", missing))
    overlap <- if (chart$group == "ethnicity") {
      p("Respondents appear in each ethnicity they selected, so the groups can overlap.")
    }
    comparison <- if (chart$type == "stacked") {
      p(sprintf("Each bar shows the share of students in each %s category.",
                tolower(plot_label(chart$comparison))))
    }
    if (chart$type == "boxplot") {
      boxes <- chart$boxes
      measure <- if (v == "cgpa") "approximate CGPA" else tolower(plot_label(v))
      return(tagList(
        p(sprintf("Median %s by %s.", measure, tolower(plot_label(chart$group)))),
        tags$ul(lapply(seq_len(nrow(boxes)), function(i) {
          tags$li(sprintf("%s has a median of %s (%d %s).", boxes$category[i],
                          round(boxes$median[i], 2), boxes$n[i],
                          if (boxes$n[i] == 1) "response" else "responses"))
        })),
        overlap, note
      ))
    }
    if (v %in% numeric_vars) {
      x <- df[[v]]
      mid <- quantile(x, c(.25, .75), names = FALSE)
      digits <- if (v == "cgpa") 2 else 1
      unit <- if (v == "commute_time") " minutes" else if (v %in% ordinal_vars) " out of 7" else ""
      slope_note <- if (chart$type == "regression") {
        p(sprintf("For a one-unit increase on the X-axis, the fitted line changes by %s on the Y-axis.",
                  format(signif(chart$slope, 3), trim = TRUE)))
      }
      return(tagList(
        p(sprintf("Among the %d responses used for this chart, the median is %s%s. The middle half of values runs from %s to %s%s.",
                  n, round(median(x), digits), unit, round(mid[1], digits), round(mid[2], digits), unit)),
        slope_note, comparison, note
      ))
    }
    counts <- if (is.null(chart$counts)) count(category_data(df, v), category) else chart$counts
    tagList(
      p(sprintf("The chart represents %d student responses.", n)),
      tags$ul(lapply(seq_len(nrow(counts)), function(i) {
        tags$li(sprintf("%s accounts for %d %s (%s).", counts$category[i], counts$n[i],
                        if (counts$n[i] == 1) "response" else "responses",
                        format_pct(counts$n[i] / n)))
      })),
      comparison, overlap, note
    )
  })

  activity_responses <- reactive({
    df <- d_f()
    validate(need(nrow(df) > 0, no_matches))
    df |>
      select(preferred_method, all_of(activity_vars)) |>
      pivot_longer(-preferred_method, names_to = "activity", values_to = "pref") |>
      filter(!is.na(pref))
  })

  activity_summary <- reactive({
    df <- activity_responses()
    choice <- req(input$activity_group)
    if (choice != "All") df <- filter(df, preferred_method == choice)
    validate(need(nrow(df) > 0, "This overall preference has no activity responses under the current filters."))
    df <- df |>
      summarise(group = sum(pref == "Group"), individual = sum(pref == "Individual"),
                neutral = sum(pref == "No preference"), n = n(), .by = activity) |>
      mutate(share = group / n,
             label = factor(plot_label(activity), levels = rev(names(activity_choices))))
    ci <- t(mapply(wilson_ci, df$group, df$n))
    mutate(df, lower = ci[, "lower"], upper = ci[, "upper"])
  })

  output$plot_act_all <- renderPlot({
    df <- activity_summary()
    col <- activity_colors()[1]
    ggplot(df, aes(x = share, y = label)) +
      geom_col(orientation = "y", fill = col, alpha = .45, width = .62) +
      geom_vline(xintercept = .5, linetype = "dashed", colour = MUTED) +
      geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y",
                    colour = col, linewidth = .8, width = .20) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .25),
                         labels = scales::label_percent()) +
      scale_y_discrete(labels = function(x) vapply(x, function(s) {
        paste(strwrap(s, width = 16), collapse = "\n")
      }, character(1))) +
      labs(x = "Share choosing group study", y = NULL) + plot_theme +
      theme(panel.grid.major.y = element_blank())
  }, alt = "Share choosing group study for each activity. The response counts are in the table.")

  output$interp_act <- renderUI({
    df <- activity_summary()
    span <- range(df$share)
    if (span[1] == span[2]) {
      return(p(sprintf("The group-study share is %s for every activity with responses in this selection.",
                       format_pct(span[1]))))
    }
    p(sprintf("Group study was most common for %s (%s) and least common for %s (%s).",
              paste(tolower(df$label[df$share == span[2]]), collapse = " and "), format_pct(span[2]),
              paste(tolower(df$label[df$share == span[1]]), collapse = " and "), format_pct(span[1])))
  })

  output$activity_table <- renderTable({
    activity_summary() |>
      transmute(Activity = as.character(label), Group = group, Individual = individual,
                `No preference` = neutral, Total = n, `Group share` = format_pct(share))
  }, striped = TRUE, spacing = "s", width = "100%", digits = 0)

  output$plot_act_by <- renderPlot({
    df <- activity_responses() |>
      filter(!is.na(preferred_method)) |>
      count(preferred_method, activity, pref) |>
      mutate(label = factor(plot_label(activity), levels = rev(names(activity_choices))))
    validate(need(nrow(df) > 0, no_responses))
    ggplot(df, aes(x = n, y = label, fill = pref)) +
      geom_col(position = "fill", width = .66, orientation = "y", colour = "white") +
      facet_wrap(vars(preferred_method), ncol = 1) +
      scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::label_percent(),
                         expand = expansion(mult = c(0, .01))) +
      scale_y_discrete(labels = c("Learning new concepts" = "New concepts",
        "Solving practice problems" = "Practice problems", "Assigned readings" = "Readings",
        "Reviewing for tests/exams" = "Test review")) +
      scale_fill_manual(values = method_colors(activity_colors())) +
      guides(fill = guide_legend(nrow = 2, order = 1)) +
      labs(x = "Share of respondents in each bar", y = NULL, fill = "Activity preference") +
      plot_theme +
      theme(panel.grid.major.y = element_blank(), panel.spacing = grid::unit(1, "lines"),
            legend.title.position = "top")
  }, alt = "Stacked bars showing the distribution of activity preferences by overall preference.")

  output$prop_test <- renderUI({
    df <- d_f()
    validate(need(nrow(df) > 0, no_matches))
    counts <- table(factor(df$preferred_method, levels = method_levels))
    n <- sum(counts[c("Group", "Individual")])
    validate(need(n > 0, "This selection has no group or individual preferences."))
    k <- counts[["Group"]]
    test <- binom.test(k, n, p = .5)
    reading <- if (test$p.value < .05) {
      paste("The responses provide evidence that the group-study share is",
            if (k / n < .5) "below 50%." else "above 50%.")
    } else {
      "The responses provide limited evidence of a difference from an even split."
    }
    tagList(
      p(sprintf(paste("%d of %d students (%s) preferred group study among those",
                      "who chose group or individual. %d selected No preference."),
                k, n, format_pct(k / n), counts[["No preference"]])),
      p(sprintf("%s The exact binomial test gives a p-value of %s. The 95%% confidence interval runs from %s to %s.",
                reading, p_text(test$p.value), format_pct(test$conf.int[1]), format_pct(test$conf.int[2])))
    )
  })

  output$chi_test <- renderUI({
    df <- d_f()
    validate(need(nrow(df) > 0, no_matches))
    v <- req(input$test_activity)
    x <- table(df[[v]], df$preferred_method)
    x <- x[rowSums(x) > 0, colSums(x) > 0, drop = FALSE]
    validate(need(all(dim(x) >= 2),
                  "Comparing preferences requires at least two observed categories for each question."))
    pearson <- suppressWarnings(chisq.test(x, correct = FALSE))
    sparse <- any(pearson$expected < 1) || mean(pearson$expected < 5) > .2
    test <- if (sparse) fisher.test(x) else pearson
    effect <- sqrt(as.numeric(pearson$statistic) / (sum(x) * (min(dim(x)) - 1)))
    group_rates <- if ("Group" %in% rownames(x)) prop.table(x, margin = 2)["Group", ] else
      setNames(rep(0, ncol(x)), colnames(x))
    reading <- if (test$p.value < .05) "The responses provide evidence of an association."
      else "The responses provide limited evidence of an association."
    method <- if (sparse) "We used Fisher's exact test because expected counts were sparse."
      else "We used Pearson's chi-squared test."
    tagList(
      p(sprintf("Among %d students with both answers, we compared overall preference with preference for %s.",
                sum(x), tolower(plot_label(v)))),
      p(sprintf("%s %s The p-value is %s and Cramer's V is %.2f on a scale from 0 to 1.",
                reading, method, p_text(test$p.value), effect)),
      tags$ul(lapply(names(group_rates), function(group) {
        tags$li(sprintf("Of students who chose %s overall, %s chose group study for this activity.",
                        tolower(group), format_pct(group_rates[[group]])))
      }))
    )
  })

  output$lm_test <- renderUI({
    response <- req(input$lm_y)
    df <- d_f() |>
      transmute(rating = .data[[response]], cgpa, commute_10 = commute_time / 10) |>
      drop_na()
    validate(need(nrow(df) >= 10,
                  "The linear model needs at least ten complete responses under these filters."))
    fit <- lm(rating ~ commute_10 + cgpa, data = df)
    validate(need(fit$rank == 3 && sd(df$rating) > 0,
                  "The selected responses have too little variation to estimate the linear model."))
    sm <- summary(fit)
    ci <- confint(fit)
    coefficient <- function(term, description) {
      b <- coef(fit)[[term]]
      change <- if (round(b, 2) == 0) "a change of less than 0.01 points"
        else sprintf("a %.2f-point %s", abs(b), if (b < 0) "decrease" else "increase")
      tags$li(sprintf("%s is associated with %s (95%% CI %.2f to %.2f, p-value %s).",
                      description, change, ci[term, 1], ci[term, 2], p_text(sm$coefficients[term, "Pr(>|t|)"])))
    }
    tagList(
      p(sprintf(paste("The linear model uses %d complete responses to estimate %s.",
                      "Commute time and CGPA together account for %s of the variation in the ratings in this selection."),
                nrow(df), tolower(plot_label(response)), format_pct(sm$r.squared))),
      tags$ul(coefficient("commute_10", "Ten more commute minutes at the same CGPA"),
              coefficient("cgpa", "One more CGPA point at the same commute time")),
      p("The model treats the steps on the rating scale as equally spaced.")
    )
  })

  output$logit_test <- renderUI({
    df <- d_f() |>
      filter(preferred_method %in% c("Group", "Individual")) |>
      transmute(group = as.integer(preferred_method == "Group"),
                commute_10 = commute_time / 10, group_difficult) |>
      drop_na()
    outcome_n <- tabulate(df$group + 1L, nbins = 2)
    validate(need(min(outcome_n) >= 5,
                  "The logistic model needs at least five complete responses for each preference."))
    warned <- FALSE
    fit <- withCallingHandlers(
      glm(group ~ commute_10 + group_difficult, data = df, family = binomial()),
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
    cf <- summary(fit)$coefficients
    validate(need(!warned && fit$converged && !fit$boundary && fit$rank == 3 &&
                    all(is.finite(cf)) && all(cf[, "Std. Error"] < 10),
                  "The logistic model cannot give stable estimates for this selection. Broaden the filters."))
    odds <- function(term, description) {
      b <- cf[term, "Estimate"]
      ci <- exp(b + c(-1, 1) * qnorm(.975) * cf[term, "Std. Error"])
      tags$li(sprintf("%s multiplies the estimated odds by %.2f (95%% CI %.2f to %.2f, p-value %s).",
                      description, exp(b), ci[1], ci[2], p_text(cf[term, "Pr(>|z|)"])))
    }
    tagList(
      p(sprintf("The logistic model uses %d complete responses (%d group and %d individual).",
                nrow(df), outcome_n[2], outcome_n[1])),
      tags$ul(odds("commute_10", "Ten more commute minutes at the same rating"),
              odds("group_difficult", "A one-point higher rating at the same commute time")),
      p("A multiplier above 1 corresponds to higher odds of preferring group study. These estimates describe associations in the survey.")
    )
  })
}

shinyApp(ui, server)
