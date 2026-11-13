mod_ds_ui <- function() {
  div(
    class = "domain-stack",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Subject disposition flow"),
      bslib::layout_sidebar(
        sidebar = domain_sidebar(
          selectizeInput(
            "ds_arm_select", "Arm", choices = NULL, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        ),
        div(
          class = "disposition-flow",
          plotly::plotlyOutput("flow_sankey", height = "100%")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Disposition by arm"),
      bslib::card_body(class = "domain-table-body", uiOutput("ds_arm_table"))
    )
  )
}

mod_ds_server <- function(dm_r, ds_r, output, input, session) {
  outcomes <- c("Not started", "Discontinued", "Completed")

  # Count each subject once, using the final disposition event by DSSEQ.
  disposition_counts <- reactive({
    arms <- dm_r() %>% dplyr::distinct(USUBJID, ARM)
    ds_r() %>%
      dplyr::filter(DSCAT == "DISPOSITION EVENT", !is.na(USUBJID)) %>%
      dplyr::group_by(USUBJID) %>%
      dplyr::slice_max(DSSEQ, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(arms, by = "USUBJID") %>%
      dplyr::mutate(
        ARM = dplyr::coalesce(ARM, "Unknown arm"),
        outcome = dplyr::case_when(
          DSDECOD == "COMPLETED" ~ outcomes[3],
          DSDECOD == "SCREEN FAILURE" | is.na(DSSTDY) ~ outcomes[1],
          TRUE ~ outcomes[2]
        ),
        reason = stringr::str_to_sentence(dplyr::coalesce(DSDECOD, "Unknown reason"))
      ) %>%
      dplyr::count(ARM, outcome, reason, name = "subjects")
  })

  arm_order <- reactive({
    arms <- sort(unique(disposition_counts()$ARM))
    c(intersect("Screen Failure", arms), setdiff(arms, "Screen Failure"))
  })

  observe({
    arms <- arm_order()
    updateSelectizeInput(session, "ds_arm_select", choices = arms, selected = arms)
  })

  selected_counts <- reactive({
    validate(need(nrow(disposition_counts()) > 0, "No disposition events recorded."))
    validate(need(length(input$ds_arm_select) > 0, "Select an arm."))
    dplyr::filter(disposition_counts(), ARM %in% input$ds_arm_select)
  })

  output$flow_sankey <- plotly::renderPlotly({
    counts <- selected_counts()

    links <- counts %>%
      dplyr::arrange(dplyr::desc(subjects), reason) %>%
      dplyr::group_by(ARM, outcome) %>%
      dplyr::summarise(
        n = sum(subjects),
        details = paste0(reason, ": ", subjects, collapse = "<br>"),
        .groups = "drop"
      )
    arms <- intersect(arm_order(), links$ARM)
    shown_outcomes <- intersect(outcomes, links$outcome)
    colors <- trial_arm_colors(arms)
    arm_totals <- tapply(links$n, links$ARM, sum)
    outcome_totals <- tapply(links$n, links$outcome, sum)
    labels <- c(arms, shown_outcomes)
    node_labels <- paste0(
      labels, " · ", c(arm_totals[arms], outcome_totals[shown_outcomes])
    )

    plotly::plot_ly(
      type = "sankey",
      arrangement = "fixed",
      node = list(
        label = node_labels,
        color = unname(c(colors, rep("#55687d", length(shown_outcomes)))),
        pad = 20, thickness = 14, line = list(width = 0),
        hovertemplate = "%{label} subjects<extra></extra>"
      ),
      link = list(
        source = match(links$ARM, arms) - 1L,
        target = length(arms) + match(links$outcome, shown_outcomes) - 1L,
        value = links$n,
        color = unname(scales::alpha(colors[links$ARM], 0.38)),
        label = paste0(
          "<b>", links$ARM, " → ", links$outcome, "</b><br>",
          links$n, " subjects<br><br>", links$details
        ),
        hovertemplate = "%{label}<extra></extra>"
      )
    ) %>%
      plotly::layout(
        font = list(family = "Arial, sans-serif", color = "#25364a", size = 13),
        margin = list(l = 14, r = 14, t = 14, b = 14),
        paper_bgcolor = "transparent",
        hoverlabel = list(
          align = "left",
          font = list(color = "#101c2b", size = 13)
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$ds_arm_table <- renderUI({
    counts <- selected_counts()
    arms <- intersect(arm_order(), counts$ARM)
    colors <- trial_arm_colors(arms)
    groups <- lapply(intersect(outcomes, counts$outcome), function(outcome) {
      rows <- counts[counts$outcome == outcome, ]
      totals <- vapply(arms, function(arm) sum(rows$subjects[rows$ARM == arm]), numeric(1))
      reasons <- setdiff(sort(unique(rows$reason)), outcome)

      tags$tbody(
        tags$tr(
          class = "disposition-total",
          tags$th(scope = "row", outcome),
          lapply(totals, function(n) tags$td(class = "number", n))
        ),
        lapply(reasons, function(reason) {
          matching <- rows[rows$reason == reason, ]
          n <- matching$subjects[match(arms, matching$ARM)]
          tags$tr(
            tags$th(scope = "row", class = "disposition-reason", reason),
            lapply(dplyr::coalesce(n, 0L), function(value) tags$td(class = "number", value))
          )
        })
      )
    })

    div(
      class = "domain-table-scroll", tabindex = "0", role = "region",
      `aria-label` = "Disposition by arm",
      tags$table(
        class = "domain-table disposition-table",
        `aria-label` = "Final disposition subject counts by arm",
        tags$thead(tags$tr(
          tags$th(scope = "col", "Final disposition"),
          lapply(arms, function(arm) {
            tags$th(
              scope = "col", class = "number",
              span(class = "arm-swatch", style = paste0("--arm-color:", colors[[arm]])),
              arm
            )
          })
        )),
        groups
      )
    )
  })
}
