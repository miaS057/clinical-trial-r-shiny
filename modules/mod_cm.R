mod_cm_ui <- function() {
  bslib::navset_card_tab(
    id = "cm_view",
    full_screen = TRUE,
    bslib::nav_panel(
      "Medication use",
      bslib::layout_sidebar(
        sidebar = domain_sidebar(
          selectizeInput("cmtrt_select", "Medication", choices = NULL),
          tags$p(class = "domain-note", "Starts on or after the subject’s reference start.")
        ),
        div(
          class = "medication-overview",
          plotly::plotlyOutput("donut_by_med", height = "290px"),
          uiOutput("donut_split_summary")
        )
      )
    ),
    bslib::nav_panel(
      "Subject timeline",
      div(
        class = "domain-stack",
        bslib::card(
          bslib::card_body(
            div(
              class = "subject-toolbar",
              selectizeInput("usubj_select", "Subject", choices = NULL),
              uiOutput("cm_subject_arm")
            ),
            uiOutput("cm_timeline_view")
          )
        ),
        bslib::card(
          bslib::card_header("Medication records"),
          bslib::card_body(class = "domain-table-body", uiOutput("cm_timeline_table"))
        )
      )
    )
  )
}

cm_full_date <- function(value) {
  value <- trimws(value)
  complete <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|[Tt ])", value)
  as.Date(ifelse(complete, substr(value, 1L, 10L), NA_character_), format = "%Y-%m-%d")
}

cm_month_position <- function(date) {
  12L * lubridate::year(date) + lubridate::month(date) - 1L +
    (lubridate::mday(date) - 1L) / as.integer(lubridate::days_in_month(date))
}

mod_cm_server <- function(dm_r, cm_r, output, input, session) {
  reference_labels <- c("Reference start", "Reference end")
  reference_colors <- c("Reference start" = "#b65300", "Reference end" = "#c22858")
  reference_types <- c("Reference start" = "dashed", "Reference end" = "dotted")

  subject_info <- reactive({
    dm_r() %>%
      dplyr::distinct(USUBJID, ARM, RFSTDTC, RFENDTC) %>%
      dplyr::mutate(
        reference_start = cm_full_date(RFSTDTC),
        reference_end = cm_full_date(RFENDTC)
      )
  })

  medication_records <- reactive({
    cm_r() %>%
      dplyr::mutate(
        CMTRT = stringr::str_squish(CMTRT),
        start_date = cm_full_date(CMSTDTC),
        end_date = cm_full_date(CMENDTC),
        ongoing = dplyr::coalesce(toupper(trimws(CMENRTPT)) == "ONGOING", FALSE) &
          (is.na(CMENDTC) | !nzchar(trimws(CMENDTC)))
      ) %>%
      dplyr::filter(!is.na(CMTRT), nzchar(CMTRT))
  })

  eligible_arms <- reactive({
    subject_info() %>%
      dplyr::filter(!is.na(reference_start), !is.na(ARM)) %>%
      dplyr::distinct(ARM) %>%
      dplyr::arrange(ARM)
  })

  post_reference <- reactive({
    medication_records() %>%
      dplyr::inner_join(
        dplyr::select(subject_info(), USUBJID, ARM, reference_start),
        by = "USUBJID"
      ) %>%
      dplyr::filter(!is.na(ARM), start_date >= reference_start) %>%
      dplyr::distinct(CMTRT, USUBJID, ARM)
  })

  observe({
    medications <- sort(unique(post_reference()$CMTRT))
    most_used <- post_reference() %>%
      dplyr::count(CMTRT, sort = TRUE) %>%
      dplyr::pull(CMTRT)
    updateSelectizeInput(
      session, "cmtrt_select", choices = medications,
      selected = head(most_used, 1), server = TRUE
    )
  })

  observe({
    subjects <- sort(unique(stats::na.omit(medication_records()$USUBJID)))
    with_dated_records <- intersect(subjects, post_reference()$USUBJID)
    updateSelectizeInput(
      session, "usubj_select", choices = subjects,
      selected = head(c(with_dated_records, subjects), 1), server = TRUE
    )
  })

  medication_counts <- reactive({
    validate(need(nrow(post_reference()) > 0, "No medication starts on or after reference start."))
    validate(need(input$cmtrt_select, "Select a medication."))
    counts <- post_reference() %>%
      dplyr::filter(CMTRT == input$cmtrt_select) %>%
      dplyr::count(ARM, name = "subjects")
    arms <- eligible_arms() %>%
      dplyr::left_join(counts, by = "ARM") %>%
      dplyr::mutate(subjects = dplyr::coalesce(subjects, 0L))
    total <- sum(arms$subjects)
    validate(need(total > 0, "No subjects for this medication."))
    dplyr::mutate(arms, share = subjects / total)
  })

  output$donut_by_med <- plotly::renderPlotly({
    arms <- medication_counts()
    visible <- dplyr::filter(arms, subjects > 0)
    colors <- trial_arm_colors(visible$ARM)

    plotly::plot_ly(
      visible, type = "pie", labels = ~ARM, values = ~subjects, hole = 0.71,
      sort = FALSE, rotation = 0, direction = "clockwise",
      textinfo = "none", showlegend = FALSE,
      marker = list(colors = unname(colors), line = list(color = "#ffffff", width = 3)),
      hovertemplate = "<b>%{label}</b><br>%{value} subjects · %{percent}<extra></extra>"
    ) %>%
      plotly::layout(
        annotations = list(list(
          text = paste0("<b>", sum(arms$subjects), "</b><br>subjects"),
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(color = "#23364b", size = 16)
        )),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        paper_bgcolor = "transparent",
        hoverlabel = list(align = "left", font = list(color = "#101c2b", size = 13))
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$donut_split_summary <- renderUI({
    arms <- medication_counts()
    colors <- trial_arm_colors(arms$ARM)
    tags$table(
      class = "domain-table medication-table",
      `aria-label` = "Subjects with the selected medication by arm",
      tags$thead(tags$tr(
        tags$th(scope = "col", "Arm"),
        tags$th(scope = "col", class = "number", "Subjects"),
        tags$th(scope = "col", class = "number", "Share of users")
      )),
      tags$tbody(lapply(seq_len(nrow(arms)), function(i) {
        tags$tr(
          tags$th(
            scope = "row",
            span(class = "arm-swatch", style = paste0("--arm-color:", colors[[arms$ARM[i]]])),
            arms$ARM[i]
          ),
          tags$td(class = "number", arms$subjects[i]),
          tags$td(class = "number", scales::percent(arms$share[i], accuracy = 0.1))
        )
      }))
    )
  })

  selected_subject <- reactive({
    req(input$usubj_select)
    dplyr::filter(subject_info(), USUBJID == input$usubj_select) %>%
      dplyr::slice_head(n = 1L)
  })

  subject_medications <- reactive({
    req(input$usubj_select)
    medication_records() %>%
      dplyr::filter(USUBJID == input$usubj_select) %>%
      dplyr::mutate(
        Start = dplyr::coalesce(format(start_date), na_if(trimws(CMSTDTC), ""), "—"),
        End = dplyr::case_when(
          ongoing ~ "Ongoing",
          TRUE ~ dplyr::coalesce(format(end_date), na_if(trimws(CMENDTC), ""), "—")
        )
      ) %>%
      dplyr::distinct(CMTRT, Start, End, start_date, end_date, ongoing) %>%
      dplyr::arrange(start_date, end_date, CMTRT, Start, End)
  })

  dated_medications <- reactive({
    dplyr::filter(subject_medications(), !is.na(start_date))
  })

  subject_reference <- reactive({
    subject <- selected_subject()
    if (!nrow(subject)) return(data.frame(label = character(), date = as.Date(character())))
    data.frame(
      label = reference_labels,
      date = c(subject$reference_start, subject$reference_end)
    ) %>%
      dplyr::filter(!is.na(date))
  })

  output$cm_subject_arm <- renderUI({
    subject <- selected_subject()
    if (nrow(subject) && !is.na(subject$ARM)) {
      tags$p(class = "subject-arm", subject$ARM)
    }
  })

  output$cm_timeline_view <- renderUI({
    meds <- dated_medications()
    if (!nrow(meds)) {
      return(tags$p(class = "domain-note", "No complete medication start dates."))
    }

    refs <- subject_reference()
    div(
      div(
        class = "timeline-key",
        lapply(seq_len(nrow(refs)), function(i) {
          label <- refs$label[i]
          span(
            span(
              class = paste("timeline-key-line", reference_types[[label]]),
              style = paste0("--line-color:", reference_colors[[label]])
            ),
            paste(label, format(refs$date[i], "%d %b %Y"))
          )
        }),
        if (any(meds$ongoing)) span(span(class = "timeline-key-ongoing"), "Ongoing"),
        if (any(is.na(meds$end_date) & !meds$ongoing)) {
          span(span(class = "timeline-key-arrow", "○"), "End date incomplete")
        }
      ),
      div(
        class = "timeline-scroll", tabindex = "0", role = "region",
        `aria-label` = "Medication timeline",
        plotOutput("cm_timeline", height = max(275, 34 * dplyr::n_distinct(meds$CMTRT) + 60))
      )
    )
  })

  output$cm_timeline <- renderPlot({
    meds <- dated_medications()
    req(nrow(meds) > 0)
    refs <- subject_reference()

    meds <- dplyr::mutate(
      meds,
      medication = factor(CMTRT, levels = rev(unique(CMTRT))),
      start = cm_month_position(start_date),
      end = cm_month_position(end_date)
    )
    refs$position <- cm_month_position(refs$date)
    limits <- range(c(meds$start, meds$end, refs$position), na.rm = TRUE)
    limits <- c(floor(limits[1]) - 0.25, ceiling(limits[2]) + 0.55)
    first_month <- ceiling(limits[1])
    last_month <- floor(limits[2])
    month_step <- max(1, ceiling((last_month - first_month + 1) / 8))
    months <- seq(first_month, last_month, by = month_step)
    completed <- dplyr::filter(meds, !ongoing, end > start)
    point_records <- dplyr::filter(meds, !ongoing, is.na(end) | end <= start)
    ongoing <- dplyr::filter(meds, ongoing)

    ggplot2::ggplot(meds, ggplot2::aes(x = start, y = medication)) +
      ggplot2::geom_vline(
        data = refs, ggplot2::aes(xintercept = position, color = label, linetype = label),
        linewidth = 1, show.legend = FALSE
      ) +
      ggplot2::geom_segment(
        data = completed, ggplot2::aes(xend = end, yend = medication),
        color = "#3376a6", linewidth = 2.4, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = ongoing, ggplot2::aes(yend = medication),
        xend = limits[2] - 0.16, color = "#3376a6", linewidth = 1.2, lineend = "round",
        arrow = grid::arrow(angle = 28, length = grid::unit(2.5, "mm"), type = "closed")
      ) +
      ggplot2::geom_point(
        data = point_records, ggplot2::aes(shape = is.na(end)),
        color = "#3376a6", size = 2.6, show.legend = FALSE
      ) +
      ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 1)) +
      ggplot2::scale_color_manual(values = reference_colors) +
      ggplot2::scale_linetype_manual(values = reference_types) +
      ggplot2::scale_x_continuous(
        limits = limits, breaks = months,
        labels = sprintf("%d-%02d", months %/% 12L, months %% 12L + 1L),
        expand = ggplot2::expansion()
      ) +
      ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, 34)) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(color = "#e5ebf2"),
        panel.grid.major.y = ggplot2::element_line(color = "#eff3f7"),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "#36475b", size = 11),
        plot.margin = ggplot2::margin(4, 10, 6, 2)
      )
  }, width = function() max(560, session$clientData$output_cm_timeline_width %||% 560),
  execOnResize = TRUE,
  alt = "Medication intervals by start date, with markers for the subject’s reference dates.")

  output$cm_timeline_table <- renderUI({
    records <- subject_medications()
    validate(need(nrow(records) > 0, "No medications recorded."))
    plotted <- unique(dated_medications()$CMTRT)
    records <- records %>%
      dplyr::mutate(
        plot_order = match(CMTRT, plotted),
        complete_start = !is.na(start_date)
      ) %>%
      dplyr::arrange(dplyr::desc(complete_start), plot_order, CMTRT, Start, End)

    div(
      if (any(!records$complete_start)) {
        tags$p(class = "domain-note table-note", "Records without a complete start date appear only here.")
      },
      div(
        class = "domain-table-scroll timeline-records",
        tabindex = "0", role = "region", `aria-label` = "Medication records",
        tags$table(
          class = "domain-table",
          tags$thead(tags$tr(
            tags$th(scope = "col", "Medication"),
            tags$th(scope = "col", "Start"),
            tags$th(scope = "col", "End")
          )),
          tags$tbody(lapply(seq_len(nrow(records)), function(i) {
            tags$tr(
              tags$th(scope = "row", records$CMTRT[i]),
              tags$td(class = "date-cell", records$Start[i]),
              tags$td(class = "date-cell", records$End[i])
            )
          }))
        )
      )
    )
  })
}
