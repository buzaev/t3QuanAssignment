getFactorTables = function(ds) {
  if (!is.data.frame(ds)) {stop("Input must be a data frame.")}
  factor_vars = names(ds)[sapply(ds, is.factor)]
  if (length(factor_vars) == 0) {return(data.frame(variable = character(), frequencyTable = character()))}
  
  result = data.frame(
    variable = factor_vars,
    frequencyTable = sapply(factor_vars, function(var) {
      freq_table = table(ds[[var]]) 
      total = sum(freq_table)  
      paste(names(freq_table), ": ",freq_table, paste0(" (", round(100 * freq_table / total, 2), "\\%)"), "",sep = "", collapse = ", ")}),
    stringsAsFactors = FALSE  
  )
  return(result)
}
