# Page for ds visuals
# pass in data needed from the server

mod_dm_ui <- function() {
  tagList(
    
  )
}


mod_ds_server <- function(dm_r, ds_r) {
  dm <- reactive({
    req(dm_r())
    validate(need(is.data.frame(dm_r()), "DM must be a data.frame"))
    dm_r()
  })
  
  # All the unique USUBJIDS
  dm_arm <- dm %>% dplyr::distinct(USUBJID, ARM, ARMCD)
  
  ds <- reactive({
    req(ds_r())
    validate(need(is.data.frame(ds_r()), "DS must be a data.frame"))
    ds_r()
  })
  
  # This is all the disposition events (added arm arm)
  dsdecod_ids <- ds %>%
    dplyr::filter(DSCAT == "DISPOSITION EVENT") %>%
    dplyr::select(USUBJID, DSDECOD, DSTERM, DSSTDY, DSSEQ) %>% 
    dplyr::left_join(dm_arm, by = "USUBJID") %>%
    group_by(DSDECOD)
  
  
  # Every subjects final ds event (how far they got) 
  # NOTE: Use to calculate the percentages of completion
  # NOTE: Make sure to use this for the flow chart
  usub_final_ds <- dsdecod_ids %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::slice_max(order_by = DSSEQ, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # How many of each event there were (sorted by ARM)
  diff_ds_events <- usub_final_ds %>%
    group_by(ARM, DSDECOD) %>%
    summarise(
      n_subjects = n_distinct(USUBJID, na.rm = TRUE),
      .groups = "drop"
      )
    
  # Use final events
  # This will make the buckets for the flow chart but will have lots of raw data
  flow_chart_buckets <- usub_final_ds %>%
    dplyr::mutate(
      bucket = case_when(
        DSDECOD == "COMPLETED" ~ "Completed",
        DSDECOD == "SCREEN FAILURE" | is.na(DSSTDY) ~ "Not Started",
        TRUE ~ "Stopped in Middle"
      )
    )
  
  # This gets the counts at each bucket for the ARM and then the ds event
  flow_bucket_counts <- flow_chart_buckets %>%
    group_by(bucket, ARM, DSDECOD) %>%
    summarize(
      subject_count = n_distinct(USUBJID),
      .groups = "drop"
    )

}
  


