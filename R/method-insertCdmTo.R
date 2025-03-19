#' @export
insertCdmTo.spark_cdm <- function(cdm, to) {
  con <- getCon(to)
  cdmSchema <- cdmSchema(to)
  writeSchema <- writeSchema(to)
  writePrefix <- writePrefix(to)

  achillesSchema <- NULL
  cohorts <- character()
  other <- character()
  for (nm in names(cdm)) {
    x <- dplyr::collect(cdm[[nm]])
    cl <- class(x)
    if ("achilles_table" %in% cl) {
      achilles <- writeSchema
    }
    if (!any(c("achilles_table", "omop_table", "cohort_table") %in% cl)) {
      other <- c(other, nm)
    }
    # omop tables in cdm schema, otherwise in write schema with prefix
    if ("omop_table" %in% cl) {
      sparkInsertTable(
        con = con,
        schema = cdmSchema,
        prefix = NULL,
        name = nm,
        value = x
      )
    } else {
      insertTable(
        cdm = to,
        name = nm,
        table = x,
        overwrite = TRUE
      )
    }
    if ("cohort_table" %in% cl) {
      cohorts <- c(cohorts, nm)
      insertTable(cdm = to, name = paste0(nm, "_set"), table = attr(x, "cohort_set"), overwrite = TRUE)
      insertTable(cdm = to, name = paste0(nm, "_attrition"), table = attr(x, "cohort_attrition"), overwrite = TRUE)
      insertTable(cdm = to, name = paste0(nm, "_codelist"), table = attr(x, "cohort_codelist"), overwrite = TRUE)
    }
  }

  newCdm <- cdmFromSpark(
    con = con,
    cdmSchema = cdmSchema,
    writeSchema = writeSchema,
    achillesSchema = achillesSchema,
    cohortTables = cohorts,
    cdmVersion = omopgenerics::cdmVersion(cdm),
    cdmName = omopgenerics::cdmName(cdm),
    .softValidation = FALSE,
    writePrefix = writePrefix
  )

  # newCdm <- omopgenerics::readSourceTable(cdm = newCdm, name = other)

  return(newCdm)
}
