library(wordcloud2)
library(tm)
library(readr)
library(dplyr)
library(extrafont)

medium_csv = read_csv("medium_data.csv") %>%
  filter(publication == 'UX Collective')

medium.corpus = Corpus(VectorSource(medium_csv$title))

removeHTML = function(text){
  text = gsub(pattern = '<.+\\">', '', text)
  text = gsub(pattern = '</.+>', '', text)
  return(text)
}

medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART"))

tdm = TermDocumentMatrix(medium.corpus) %>%
  as.matrix()
words = sort(rowSums(tdm), decreasing = TRUE)
df = data.frame(word = names(words), freq = words)

df = df %>%
  filter(nchar(as.character(word)) > 2,
         word != "don'")

uxc.colors = c("#fefefe", "#f4f2a8", "#030303")
uxc.background = "#00ccff"

wordcloud2(df,
           color = rep_len(uxc.colors, nrow(df)),
           backgroundColor = uxc.background,
           fontFamily ="Lucida Sans",
           size = 2,
           minSize = 5,
           rotateRatio = 0)
           