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

# find title using name column
getTitle = function(x){
  title = c()
  
  for (i in 1:length(x)) {
    m = str_split_fixed(x[i], pattern = ",", n = 2)
    t = str_split_fixed(m[2], pattern = ". ", n = 2)
    
    title = c(title, t[1])
  }
  
  return(title)
}

# Embarked - replace empty value to "S"

