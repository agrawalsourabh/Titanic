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

# TITLE
# creating word cloud
title = data.frame(table(our.data$title))
wordcloud(title$Var1, freq = title$Freq, colors = brewer.pal(8, "Accent"), random.order = T)

# bar graph for missing value as per title
ggplot(data = missing_age_title.df, mapping = aes(x = reorder(title, Perc), y = Perc)) +
  geom_bar(fill = "#ed8661", stat = "identity") +
  coord_flip() +
  geom_label(label = paste(missing_age_title.df$Perc, "%")) +
  ggtitle("Age missing value") +
  xlab("Title") +
  ylab("Percentage (%)")

# word cloud for missing_age_title
wordcloud(missing_age_title.df$title, freq = missing_age_title.df$Age, 
          colors = brewer.pal(8, "Accent"), random.order = T)

