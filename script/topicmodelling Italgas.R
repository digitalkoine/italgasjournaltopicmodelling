# Carica i pacchetti necessari
#it Carica i pacchetti necessari per l'analisi del testo e la visualizzazione
#en Load required packages for text analysis and visualization
library(mallet)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(plotly)

### Definizione delle variabili
#it Definisce il numero di argomenti e di iterazioni
#en Define the number of topics and iterations
num_topics <- 10
num_iterations <- 200

# Lista di stopwords italiane
#it Elenca le parole comuni da ignorare nel testo (stopwords)
#en List common words to ignore in the text (stopwords)
italian_stopwords <- c(
  "di", "a", "da", "in", "il", "la", "e", "che", "con", "per", "non", "una",
  "un", "le", "si", "ma", "anche", "come", "più", "o", "al", "tra", "della",
  "gli", "dei", "io", "tu", "lui", "lei", "noi", "voi", "loro", "mi", "ti",
  "ci", "vi", "lo", "la", "li", "le", "ne", "del", "alla", "delle", "dei",
  "sul", "sulla", "sui", "nelle", "questo", "questa", "quello", "quella",
  "questi", "quelle", "sono", "ho", "hai", "ha", "abbiamo", "avete",
  "hanno", "era", "stato", "essere", "fare", "fatto", "è", "i", "l", "tutto", "c", "poi", "se"
)

# Crea un file temporaneo di stopwords
#it Scrive le stopwords in un file temporaneo
#en Write stopwords to a temporary file
stopwords_file <- tempfile(fileext = ".txt")
writeLines(italian_stopwords, stopwords_file)

# Preparazione del corpus
#it Inizializza un vettore per memorizzare i testi
#en Initialize a vector to store texts
my_texts <- character()

# Ottieni la lista dei file di testo dal corpus
#it Elenca i file di testo dal corpus specificato
#en List text files from the specified corpus
file_list <- list.files("corpus_italgas", full.names = TRUE)

# Funzione di pre-processing del testo
#it Definisce una funzione per la pulizia del testo
#en Define a function for text cleaning
preprocess_text <- function(text) {
  text <- tolower(text)                        #it Converti in minuscolo
  text <- gsub("[[:punct:]]", " ", text)       #it Rimuovi la punteggiatura
  text <- gsub("[0-9]", " ", text)             #it Rimuovi i numeri
  text <- gsub("\\s+", " ", text)              #it Rimuovi spazi extra
  return(text)
}

# Loop per leggere e processare ogni file
#it Legge e processa ogni file di testo
#en Read and process each text file
for (i in 1:length(file_list)) {
  
  # Legge il file di testo
  loaded_file <- readLines(file_list[i], warn = FALSE)
  loaded_file <- paste(loaded_file, collapse = "\n")
  
  # Pre-processamento del testo
  cleaned_text <- preprocess_text(loaded_file)
  
  # Tokenizzazione
  #it Divide il testo in parole
  #en Split the text into words
  tokenized_text <- unlist(strsplit(cleaned_text, "\\W"))
  tokenized_text <- tokenized_text[which(tokenized_text != "")]
  
  # Rimuovi le stopwords
  tokenized_text <- tokenized_text[!tokenized_text %in% italian_stopwords]
  
  # Assegna un nome al testo
  text_name <- gsub(pattern = "corpus_italgas/|.txt", replacement = "", x = file_list[i])
  
  tokenized_text <- paste(tokenized_text, collapse = " ")
  
  my_texts <- c(my_texts, tokenized_text)
  names(my_texts)[i] <- text_name
  
  print(paste("File processato:", i, "/", length(file_list)))
}

# Importazione dei testi per il topic modeling
#it Importa i testi per il topic modeling con Mallet
#en Import texts for topic modeling using Mallet
text.instances <- mallet.import(my_texts, stopwords_file, names(my_texts))

# Creazione del modello LDA
#it Crea il modello LDA specificando i parametri
#en Create the LDA model specifying parameters
topic.model <- MalletLDA(num.topics = num_topics, alpha.sum = 1, beta = 0.1)

# Caricamento dei documenti nel modello
topic.model$loadDocuments(text.instances)

# Ottimizzazione del modello
topic.model$setAlphaOptimization(20, 50)

# Addestramento del modello
topic.model$train(num_iterations)

# Estrazione delle probabilità
#it Estrae le probabilità argomento-documento e parola-argomento
#en Extract topic-document and word-topic probabilities
doc.topics <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)

# Estrazione delle parole principali per ogni argomento
top_words <- data.frame()
firstwords <- character()
for (i in 1:num_topics) {
  words.per.topic <- mallet.top.words(topic.model, topic.words[i,], 20)
  words.per.topic$topic <- i
  top_words <- rbind(top_words, words.per.topic)
  firstwords[i] <- paste(words.per.topic$term[1:5], collapse = " ")
}

# Visualizzazione dei risultati
#it Crea una wordcloud e un grafico a barre
#en Create a wordcloud and a bar chart
p1 <- ggplot(top_words, aes(label = term, size = weight)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~topic)
p1
ggsave(p1, "Topics_wordcloud.png")

p2 <- top_words %>%
  mutate(term = reorder_within(term, weight, topic)) %>%
  ggplot(aes(weight, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
p2
ggsave(p2, "Topics_barchart.png")

### Heatmap
#it Crea una heatmap interattiva con Plotly
#en Create an interactive heatmap using Plotly
doc.topics <- as.data.frame(doc.topics)
heatmap_plot <- plot_ly(
  x = colnames(doc.topics),
  y = rownames(doc.topics),
  z = as.matrix(doc.topics),
  type = "heatmap",
  colors = colorRamp(c("white", "blue", "red"))
)
heatmap_plot
htmlwidgets::saveWidget(heatmap_plot, "heatmap_interactive.html")

# Heatmap semplificata
#it Raggruppa i documenti e calcola la media per gruppo
#en Group documents and calculate the mean for each group
groups_tmp <- sapply(strsplit(rownames(doc.topics), "_"), function(x) paste(x[1:3], collapse = "_"))
doc.topics$group <- groups_tmp
doc.topics.simple <- doc.topics %>%
  group_by(group) %>%
  summarise(across(everything(), mean))

png("heatmap_simple.png")
heatmap(as.matrix(doc.topics.simple[-1]))
dev.off()
