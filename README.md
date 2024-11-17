## Italgas Topic Modelling Project

First reusult : https://digitalkoine.github.io/italgasjournaltopicmodelling/ 

This repository contains an R-based project for performing topic modeling on text data from Italgas. The analysis leverages Latent Dirichlet Allocation (LDA) using the mallet package to identify key topics in the provided corpus. The results are visualized with word clouds, bar charts, and heatmaps.

Project Overview

The project aims to extract meaningful topics from a collection of text files, preprocess the data, and visualize the topics discovered using LDA.

Key Features
Data Preprocessing: Cleans and tokenizes the text, removing punctuation, numbers, and common Italian stopwords.
Topic Modeling with LDA: Applies LDA using the mallet library to extract topics.
Visualization: Generates word clouds, bar charts, and heatmaps for visual interpretation of topics.
File Structure

topicmodelling_italgas.R: Main R script that performs the entire analysis pipeline, from data preprocessing to visualization.
stopwords-it.txt: File containing a list of Italian stopwords used for filtering.
SentiArt.csv: A sentiment lexicon file for additional sentiment analysis.
README.md: This file, providing an overview of the project.
LICENSE: MIT license for the project.
Prerequisites

Ensure you have the following R packages installed before running the script:

install.packages(c("tidyverse", "tidytext", "ggwordcloud", "plotly", "mallet"))
Setup

Clone this repository:
git clone https://github.com/yourusername/italgas-topic-modelling.git
cd italgas-topic-modelling
Place your text files in the corpus_italgas/ directory.
Open the topicmodelling_italgas.R script in your R environment.
Running the Analysis

Execute the topicmodelling_italgas.R script to preprocess the data, perform LDA, and generate visualizations. The script will:

Load and preprocess the text files from the specified corpus directory.
Tokenize the text, remove stopwords, and prepare the data for topic modeling.
Train an LDA model using the mallet package.
Extract top words for each topic and visualize them using word clouds and bar charts.
Generate an interactive heatmap of topic-document probabilities using plotly.
Output Files

Topics_wordcloud.png: A word cloud visualization of the main topics.
Topics_barchart.png: A bar chart showing the top words for each topic.
heatmap_interactive.html: An interactive heatmap using plotly to display topic-document associations.
heatmap_simple.png: A simplified static heatmap.
Example Usage

To run the analysis in R, use the following command:

source("topicmodelling_italgas.R")
The output visualizations will be saved in the project directory.

License

This project is licensed under the MIT License. See the LICENSE file for details.
