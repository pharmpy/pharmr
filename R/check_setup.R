#' @title
#' Checks setup of Pharmpy/pharmr 
#' 
#' @description
#' Checks if everything is setup correctly. The following things are checked:
#' * If Python is installed and has correct version
#' * If Pharmpy is available
#' * If Pharmpy and pharmr version matches
#' 
#' @export
check_setup <- function() {
  cli::cli_h3("Checking Python setup")
  py_okay <- check_python()
  if (py_okay) {
    cli::cli_h3("Checking Pharmpy and pharmr")
    pharmpy_okay <- check_pharmpy()
  }
}

check_python <- function() {
  py <- reticulate::py_discover_config()
  if (is.null(py)) {
    print_status('fail', 'No Python found')
    return(FALSE)
  }
  print_status('success', 'Python found:', additional_info=py$python)
  py_version <- as.character(py$version)
  if (utils::compareVersion(py_version, '3.11') == -1) {
    print_status('fail', 'Version not supported:', additional_info=py_version)
    return(FALSE)
  }
  print_status('success', 'Version supported:', additional_info=py_version)
  return(TRUE)
}

check_pharmpy <- function() {
  pharmpy_avail <- reticulate::py_module_available('pharmpy')
  if (isFALSE(pharmpy_avail)) {
    print_status('fail', 'Could not find Pharmpy installation')
    return(FALSE)
  }
  pharmpy_version <- reticulate::py_to_r(pharmpy$`__version__`)
  print_status('success', 'Pharmpy found:', additional_info=pharmpy_version)
  pharmr_version <- as.character(packageVersion('pharmr'))
  if (utils::compareVersion(pharmpy_version, pharmr_version) != 0) {
    print_status('fail', 'pharmr version does not match:', additional_info=pharmr_version)
    return(FALSE)
  }
  print_status('success', 'pharmr version matches:', additional_info=pharmr_version)
  return(TRUE)
}

print_status <- function(type, msg, additional_info=NULL) {
  if (type == 'success') {
    cli::cli_alert_success(msg)
  }
  else if (type == 'fail') {
    cli::cli_alert_danger(msg)
  }
  else {
    stop('Unknown status type')
  }
  if (!is.null(additional_info)) {
    cat('  ')
    cli::cli_alert(additional_info)
  }
}
