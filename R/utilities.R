cdmSchema <- function(src) {
  if (inherits(src, "cdm_reference")) {
    return(attr(omopgenerics::cdmSource(src), "cdm_schema"))
  } else {
    attr(src, "cdm_schema")
  }
}
writeSchema <- function(src) {
  if (inherits(src, "cdm_reference")) {
    return(attr(omopgenerics::cdmSource(src), "write_schema"))
  } else {
    attr(src, "write_schema")
  }
}
writePrefix <- function(src) {
  if (inherits(src, "cdm_reference")) {
    return(attr(omopgenerics::cdmSource(src), "write_prefix"))
  } else {
    attr(src, "write_prefix")
  }
}
getCon <- function(src) {
  if (inherits(src, "cdm_reference")) {
    return(attr(omopgenerics::cdmSource(src), "con"))
  } else {
    attr(src, "con")
  }
}

getWriteTableName <- function(writeSchema, prefix, name) {
  if (is.null(prefix)) {
    tbl_name <- paste0(writeSchema, ".", name)
  } else {
    tbl_name <- paste0(writeSchema, ".", prefix, name)
  }
  tbl_name
}

validateConnection <- function(con, call = parent.frame()) {
  if (!inherits(con, "spark_connection")) {
    cli::cli_abort(c(x = "{.arg con} must a {.cls spark_connection} object."), call = call)
  }
  if (!sparklyr::connection_is_open(con)) {
    cli::cli_abort(c(x = "{.arg con} connection is closed, please provide an open connection."), call = call)
  }
  return(con)
}
