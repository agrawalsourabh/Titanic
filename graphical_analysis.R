# plot missing data
ggplot(data = missing.df, aes(x = varname, y = per)) +
  geom_line( aes(group = 1), col = "red") +
  ggtitle("Missing value") +
  xlab("Variable Name") +
  ylab("Percentage (%)") +
  geom_label(label = paste(missing.df$per, "%"))

# SURVIVED
ggplot(data = our.train, mapping = aes(x = Survived, fill=Survived)) +
  geom_bar() +
  ggtitle("Survived") +
  stat_count(aes(label = ..count..), geom = "label")
  
# Pclass
ggplot(data = our.train, mapping = aes(x = Pclass, fill=Survived)) +
  geom_bar() +
  ggtitle("PClass") +
  stat_count(aes(label = ..count..), geom = "label")

# Sex
ggplot(data = our.train, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar() +
  ggtitle("Sex") +
  stat_count(aes(label = ..count..), geom = "label")