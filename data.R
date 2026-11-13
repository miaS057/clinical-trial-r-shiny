sdtm_labels <- c(
  dm = "Demographics (DM)", ds = "Disposition (DS)",
  ae = "Adverse events (AE)", cm = "Medications (CM)"
)

get_dm <- function(){
  dm <- read.csv("data/dm.csv")
  return(dm)
}

get_ds <- function(){
  ds <- read.csv("data/ds.csv")
  return(ds)
}

get_ae <- function(){
  ae <- read.csv("data/ae.csv")
  return(ae)
}

get_cm <- function(){
  cm <- read.csv("data/cm.csv")
  return(cm)
}

sdtm_import_error <- function(message) {
  stop(errorCondition(message, class = "sdtm_import_error"))
}

read_sdtm_domain <- function(file, domain) {
  columns <- list(
    dm = c("SITEID", "AGE", "SEX", "RACE", "ARM", "ACTARM", "COUNTRY", "RFSTDTC", "RFENDTC"),
    ds = c("DSCAT", "DSDECOD", "DSSEQ", "DSSTDY"),
    ae = c("AEDECOD", "AEBODSYS", "AESEV", "AEREL", "AESTDTC", "AEENDTC"),
    cm = c("CMTRT", "CMSTDTC", "CMENDTC")
  )
  label <- toupper(domain)
  if (nrow(file) != 1L) sdtm_import_error(paste("Choose one file for", label))
  format <- tolower(tools::file_ext(file$name))
  if (!format %in% c("csv", "xpt")) sdtm_import_error(paste(label, "must be a CSV or SAS XPT file."))

  table <- tryCatch(
    if (format == "xpt") {
      as.data.frame(haven::read_xpt(file$datapath))
    } else {
      utils::read.csv(file$datapath, colClasses = "character", check.names = FALSE)
    },
    error = function(error) sdtm_import_error(paste("Could not read the", label, "file."))
  )

  names(table) <- toupper(trimws(names(table)))
  if (anyDuplicated(names(table))) sdtm_import_error(paste(label, "has duplicate column names."))
  if ("DOMAIN" %in% names(table)) {
    file_domains <- unique(toupper(trimws(as.character(table$DOMAIN))))
    file_domains <- setdiff(file_domains, c(NA_character_, ""))
    if (length(setdiff(file_domains, label))) {
      sdtm_import_error(paste("Expected", label, "but found", paste(file_domains, collapse = ", ")))
    }
  }

  required <- c("STUDYID", "USUBJID", columns[[domain]])
  missing <- setdiff(required, names(table))
  if (length(missing)) {
    sdtm_import_error(paste0(label, " is missing ", paste(missing, collapse = ", "), "."))
  }
  if (domain == "cm" && !"CMENRTPT" %in% names(table)) table$CMENRTPT <- NA_character_

  used <- c(required, intersect("CMENRTPT", names(table)))
  table <- table[used]
  table[] <- lapply(table, function(value) {
    value <- trimws(as.character(value))
    value[!nzchar(value)] <- NA_character_
    value
  })
  for (column in intersect(c("AGE", "DSSEQ", "DSSTDY"), names(table))) {
    numeric_value <- suppressWarnings(as.numeric(table[[column]]))
    if (any(!is.finite(numeric_value) & !is.na(table[[column]]))) {
      sdtm_import_error(paste0(label, " column ", column, " must contain numbers or missing values."))
    }
    table[[column]] <- numeric_value
  }
  if (anyNA(table$STUDYID) || anyNA(table$USUBJID)) {
    sdtm_import_error(paste(label, "has records without a study or subject ID."))
  }
  if (length(unique(table$STUDYID)) > 1L) {
    sdtm_import_error(paste(label, "contains records from more than one study."))
  }
  if (domain == "dm" && (!nrow(table) || anyDuplicated(table$USUBJID))) {
    sdtm_import_error("DM must contain one record per subject.")
  }
  table
}

validate_sdtm_study <- function(tables) {
  dm <- tables$dm
  if (is.null(dm)) sdtm_import_error("Add a demographics (DM) file to load the study.")
  study_ids <- unique(unlist(lapply(tables, function(table) table$STUDYID), use.names = FALSE))
  if (length(study_ids) > 1L) sdtm_import_error("Selected files must belong to the same study.")

  for (domain in setdiff(names(tables), "dm")) {
    if (!all(tables[[domain]]$USUBJID %in% dm$USUBJID)) {
      sdtm_import_error(paste(toupper(domain), "contains subjects missing from DM."))
    }
  }
  tables
}
