# Carica i pacchetti necessari
library(mallet)
library(tidyverse)
library(tidytext)
library(ggwordcloud)

### Definizione delle variabili
num_topics <- 25      # Numero di argomenti
num_iterations <- 200 # Numero di iterazioni
# len_split <- 10000    # Lunghezza dei testi divisi

# Lista di stopwords italiane
italian_stopwords <- c(
  "di", "a", "da", "in", "il", "la", "e", "che", "con", "per", "non", "una",
  "un", "le", "si", "ma", "anche", "come", "più", "o", "al", "tra", "della",
  "gli", "dei", "io", "tu", "lui", "lei", "noi", "voi", "loro", "mi", "ti",
  "ci", "vi", "lo", "la", "li", "le", "ne", "del", "alla", "delle", "dei",
  "sul", "sulla", "sui", "nelle", "da", "dai", "questo", "questa", "quello",
  "quella", "questi", "quelle", "sono", "ho", "hai", "ha", "abbiamo", "avete",
  "hanno", "era", "stato", "essere", "fare", "fatto", "è", "i", "l", "tutto", 
  "c", "poi", "se", "italgas", "nel", "dell", "ed", "ad", "it", "d", "suo", "sua",
  "dal", "fu", "nella", "può", "cui", "molto", "nella", "nel", "dal", "due", "the",
  "of", "due", "all", "de", "degli", "dove", "miliardi", "milioni", "gruppo", "società", 
  "gas", "rete", "metano", "sia", "modo", "così", "anni", "ancora", "perché", "qui",
  "modo", "così", "to", "and", "Titolo", "Sottotitolo", "Testo", "dell", "nei", "quando",
  "rig", "bib", "cubi", "metri", "nell", "uno", "san", "ai", "chi",
  "prima", "tutti", "quel", "sempre", "aveva", "dalla", "su", "s", "dopo", "oggi"
)

# Crea un file temporaneo di stopwords italiane
stopwords_file <- tempfile(fileext = ".txt")
writeLines(italian_stopwords, stopwords_file)

# Preparazione del corpus
my_texts <- character()

# Ottieni la lista dei file di testo dal nuovo corpus
file_list <- list.files("corpus_italgas", full.names = TRUE)

# Funzione di pre-processing del testo
preprocess_text <- function(text) {
  text <- tolower(text)                        # Converti in minuscolo
  text <- gsub("[[:punct:]]", " ", text)       # Rimuovi la punteggiatura
  text <- gsub("[0-9]", " ", text)             # Rimuovi i numeri
  text <- gsub("\\s+", " ", text)              # Rimuovi spazi extra
  return(text)
}

# Loop per leggere e processare ogni file nel corpus
for (i in 1:length(file_list)) {
  
  # Legge il file di testo
  loaded_file <- readLines(file_list[i], warn = FALSE)
  loaded_file <- paste(loaded_file, collapse = "\n")
  
  # Pre-processamento del testo
  cleaned_text <- preprocess_text(loaded_file)
  
  # Tokenizzazione del testo (suddivisione in parole)
  tokenized_text <- unlist(strsplit(cleaned_text, "\\W"))
  tokenized_text <- tokenized_text[which(tokenized_text != "")]
  
  # Rimuovi le stopwords italiane
  tokenized_text <- tokenized_text[!tokenized_text %in% italian_stopwords]
  
  # Estrae il nome del file per identificare il documento
  text_name <- gsub(pattern = "corpus_italgas/|.txt", replacement = "", x = file_list[i])
  
  tokenized_text <- paste(tokenized_text, collapse = " ")
  
  my_texts <- c(my_texts, tokenized_text)
  
  names(my_texts)[i] <- text_name
  
  print(paste("File processato:", i, "/", length(file_list)))
}

# Importazione dei testi per il topic modeling
text.instances <- mallet.import(
  text.array = my_texts, 
  stoplist = stopwords_file, # Passa il file temporaneo di stopwords italiane
  id.array = names(my_texts)
)

# Creazione del modello LDA
topic.model <- MalletLDA(num.topics = num_topics, alpha.sum = 1, beta = 0.1)

# Caricamento dei documenti nel modello
topic.model$loadDocuments(text.instances)

# Ottimizzazione del modello
topic.model$setAlphaOptimization(20, 50)

# Addestramento del modello
topic.model$train(num_iterations)

# Estrazione delle probabilità argomento-documento e parola-argomento
doc.topics <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)

# Estrazione delle parole principali per ogni argomento
top_words <- data.frame()
firstwords <- character()
for (i in 1:num_topics) {
  words.per.topic <- mallet.top.words(topic.model, word.weights = topic.words[i,], num.top.words = 20)
  words.per.topic$topic <- i
  top_words <- rbind(top_words, words.per.topic)
  firstwords[i] <- paste(words.per.topic$term[1:5], collapse = " ")
}

# Visualizza la tabella delle parole principali
View(top_words)

# Mostra le prime cinque parole per ogni argomento
names(firstwords) <- paste("Topic", 1:length(firstwords))
firstwords

### Wordcloud (nuvola di parole)

# Creazione del wordcloud
p1 <- ggplot(top_words, aes(label = term, size = weight)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~topic)

# Mostra il grafico
p1

# Salva il wordcloud
ggsave(p1, filename = "Topics_wordcloud.png", scale = 1.5)

# wordcloud
library(wordcloud2)

# Preparazione dei dati per wordcloud2
top_words_for_wc <- top_words %>%
  group_by(term) %>%
  summarise(weight = sum(weight))

# Creazione della wordcloud interattiva
wordcloud2(data = top_words_for_wc, size = 0.5, color = "random-light", backgroundColor = "black")



### Visualizzazione con grafico a barre

p2 <- top_words %>%
  mutate(term = reorder_within(term, weight, topic)) %>%
  ggplot(aes(weight, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Mostra il grafico
p2

# Salva il grafico a barre
ggsave(p2, filename = "Topics_barchart.png", scale = 2)

### Creazione di una heatmap

colnames(doc.topics) <- firstwords
rownames(doc.topics) <- names(my_texts)

# Visualizza e salva la heatmap
# Visualizza e salva la heatmap senza clustering
png(filename = "heatmap_no_clustering.png", width = 4000, height = 4000)
heatmap(doc.topics, margins = c(25, 25), cexRow = 2, cexCol = 2, Rowv = NA, Colv = NA)
dev.off()


library(plotly)

# Converti 'doc.topics' in data frame
doc.topics <- as.data.frame(doc.topics)

# Imposta i nomi delle colonne e delle righe in modo sicuro
firstwords <- firstwords[1:min(ncol(doc.topics), length(firstwords))]
colnames(doc.topics) <- firstwords
rownames(doc.topics) <- names(my_texts)

# Filtra solo le colonne numeriche
numeric_doc_topics <- doc.topics[, sapply(doc.topics, is.numeric)]

# Converti in matrice numerica
numeric_doc_topics_matrix <- as.matrix(numeric_doc_topics)

# Crea una heatmap interattiva con Plotly
heatmap_plot <- plot_ly(
  x = colnames(numeric_doc_topics_matrix),
  y = rownames(numeric_doc_topics_matrix),
  z = numeric_doc_topics_matrix,
  type = "heatmap",
  colors = colorRamp(c("white", "blue", "red"))
)

# Mostra la heatmap interattiva
heatmap_plot



htmlwidgets::saveWidget(heatmap_plot, "heatmap_interactive.html")


### Heatmap semplificata

# Creazione di una heatmap semplificata

doc.topics <- as.data.frame(doc.topics)
groups_tmp <- strsplit(rownames(doc.topics), "_")
groups_tmp <- sapply(groups_tmp, function(x) paste(x[1:3], collapse = "_"))
doc.topics$group <- groups_tmp

# Raggruppa per 'group' e calcola la media
doc.topics.simple <- doc.topics %>%
  group_by(group) %>%
  summarise(across(everything(), mean))

# Imposta i nomi delle righe
rownames(doc.topics.simple) <- doc.topics.simple$group

# Rimuovi la colonna 'group' prima di convertire in matrice
doc.topics.simple <- as.matrix(doc.topics.simple[, -1])

# Creazione e salvataggio della heatmap
png(filename = "heatmap_simple.png", width = 1000, height = 1000)
heatmap(doc.topics.simple, margins = c(25, 25), cexRow = 2, cexCol = 2)
dev.off()