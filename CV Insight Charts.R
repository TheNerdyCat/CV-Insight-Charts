

## CV insight charts

setwd("D:/R")




# Software - Scatter Plot ------------------------------------------------

library(ggplot2)
library(extrafont)

# Create df
scatter_data <- data.frame(software = c(rep("R", 5),
                                        rep("Python", 5),
                                        rep("MySQL", 5),
                                        rep("Apache Spark", 5),
                                        rep("Excel", 5),
                                        rep("MongoDB", 5),
                                        rep("Tableau", 5)),
                           rating = rep(c(1,2,3,4,5), 7),
                           colour = c(rep("A", 5),
                                      c(rep("A", 4), "B"),
                                      c(rep("A", 3), rep("B", 2)),
                                      c(rep("A", 2), rep("B", 3)),
                                      rep("A", 5),
                                      c(rep("A", 2), rep("B", 3)), 
                                      c(rep("A", 3), rep("B", 2))))

# Reverse order of software so alphabetical
scatter_data$software <- factor(scatter_data$software,
                                levels=rev(levels(scatter_data$software)))
# Plot

g <- ggplot(scatter_data, aes(rating, software)) 
g <- g + geom_point(aes(colour = colour), size = 14)
g <- g + scale_x_continuous(limits = c(0.85,5.1))    
g <- g + scale_color_manual(values = c("#1485A4", "#8F9394"))     # Specify colours of points
g <- g + labs(x = NULL, y = NULL)                                 # Remove axis titles
g <- g + theme(panel.grid = element_blank())                      # Remove gridlines
g <- g + theme(axis.line = element_blank())
g <- g + theme(axis.ticks = element_blank())                      # Remove tick marks
g <- g + theme(axis.text.x = element_blank())                     # Remove x-axis labels for
g <- g + theme(legend.position =  "none")                         # Remove Legend
g <- g + theme(text = element_text(size = 30, family = "Century Gothic")) # Customise font of text
g <- g + theme(plot.background = element_rect(fill = "#D1E7F6"))  # Change background colour of plot
g <- g + theme(panel.background = element_rect(fill = "#D1E7F6")) # Change background colour of panel


# Hard Skills - Polar Chart -----------------------------------------------

library(ggplot2)
library(dplyr)
library(extrafont)


# Create df
polar_data <- data.frame(x = c("Quality\nAssurance",
                               "Data\nManagement",
                               "Machine\nLearning",
                               "Data\nVisualisation",
                               "Data\nAnalysis"),
                         y = c(2,3,4,5,1))



# Plot

g <- ggplot(polar_data, aes(factor(x, levels = x), y))
g <- g + geom_bar(width = 0.95,stat = "identity", fill = "#1485A4", colour = "#FFFFFF")
g <- g + coord_polar()
g <- g + scale_y_continuous(limits = c(-3,7))
g <- g + annotate("text", x = 5, y = 5, label = "Quality\nAssurance",   size = 7, family = "Century Gothic")
g <- g + annotate("text", x = 1, y = 5, label = "Data\nManagement",    size = 7, family = "Century Gothic")
g <- g + annotate("text", x = 2, y = 6, label = "Machine\nLearning",  size = 7, family = "Century Gothic")
g <- g + annotate("text", x = 3, y = 6, label = "Data\nVisualisation", size = 7, family = "Century Gothic")
g <- g + annotate("text", x = 4, y = 7, label = "Data\nAnalysis",      size = 7, family = "Century Gothic")
g <- g + theme(panel.grid =  element_blank())
g <- g + theme(axis.text.y = element_blank())
g <- g + theme(axis.ticks =  element_blank())
g <- g + theme(axis.text.x = element_blank())
g <- g + theme(plot.background =  element_rect(fill = "#D1E7F6"))
g <- g + theme(panel.background = element_rect(fill = "#D1E7F6"))
g <- g + theme(legend.position = "none")



# Soft Skills -Word Cloud -------------------------------------------------

library(xml2)
library(tm)
library(SnowballC)
library(wordcloud)
library(extrafont)

# Read the text file from internet
filePath <- "Words for Edward.txt"
text <- readLines(filePath)

# Convert 
docs <- Corpus(VectorSource(text))

# Remove unrecoginsable punctuation, replace with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Create term document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE) # Arrange in order of decreasing freq
d <- data.frame(word = names(v),freq = v)
head(d, 10)

# PLot
set.seed(1230)

png("Edward Wordcloud.png",
    units = "in", width = 6, height = 7, res = 1000)
par(bg="#D1E7F6")
wordcloud(words = d$word, 
          freq = d$freq, 
          min.freq = 1, 
          random.order = FALSE, 
          rot.per = 0, 
          colors = c("#1485A4", "#1485A4", "#1485A4", "#1485A4", "#1485A4", "#1485A4"),
          family = "Century Gothic")
dev.off()



