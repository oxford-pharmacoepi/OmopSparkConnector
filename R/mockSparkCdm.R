#' creates a cdm reference to local spark OMOP CDM tables
#'
#' @param path A directory for files
#'
#' @return A cdm reference with synthetic data in a local spark connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mockSparkCdm()
#' }
#'
mockSparkCdm <- function(path) {
  folder <- path
  working_config <- sparklyr::spark_config()
  working_config$spark.sql.warehouse.dir <- folder
  con <- sparklyr::spark_connect(
    master = "local",
    config = working_config
  )
  DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS omop"))
  DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS results"))
  src <- sparkSource(
    con = con,
    cdmSchema = "omop",
    writeSchema = "results",
    writePrefix = "my_study_"
  )

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence() |>
    omock::mockCohort()

  cdm <- insertCdmTo(cdm_local, src)

  cdm <- cdmFromSpark(
    con = con,
    cdmSchema = "omop",
    writeSchema = "results",
    cdmName = "mock local spark",
    writePrefix = "my_study_"
  )

  return(cdm)
}

