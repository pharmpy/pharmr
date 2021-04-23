context("test-docs")

test_that("pharmr documentation has been rendered correctly", {
  skip_on_cran()
  
  dir_path <- system.file('man', package='pharmr')
  
  if (dir_path == '') {
    system_path <- system.file(package='pharmr')
    dir_path <- gsub('pharmr.Rcheck\\/pharmr', 'pharmr.Rcheck\\/00_pkg_src\\/pharmr\\/man', system_path)
  }

  add_iiv_path <- paste(dir_path, 'add_iiv.Rd', sep='/')
  help_lines <- readLines(add_iiv_path)
  help_text <- paste(help_lines, collapse=" ")
  
  expect_match(help_text, "add_iiv\\(model, list_of_parameters, expression, operation='\\*', eta_names=NULL\\)")
  expect_match(help_text, 'list_of_parameters')
  expect_match(help_text, 'character, vector. Name/names of parameter to add new IIVs to.')
  
  set_init_ests_path <- paste(dir_path, 'set_initial_estimates.Rd', sep='/')
  help_lines <- readLines(set_init_ests_path)
  help_text <- paste(help_lines, collapse=" ")
  
  expect_match(help_text, "set_initial_estimates\\(model, inits\\)")
  expect_match(help_text, 'A list of parameter')
})
