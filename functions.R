# function to find missing value
missing_val = function(x){
  count_na = c()
  var_na = c()
  per_na = c()
  
  for (i in 1:ncol(x)) {
    if(sum(is.na(x[i])) > 0){
      count = sum(is.na(x[i]))
      varname = colnames(x[i])
      per = round(count/nrow(x) * 100, 2)
      
      count_na = c(count_na, count)
      var_na = c(var_na, varname)
      per_na = c(per_na, per)
    }
  }
  
  missing.df = data.frame(varname = var_na, count=count_na, per=per_na)
  
  return(missing.df)
}