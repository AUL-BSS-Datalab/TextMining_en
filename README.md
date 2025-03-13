# Text Mining
This repository contains digital tools for text mining, aimed at students with little or no programming experience. The tools are developed as Shiny Apps, allowing users to interact through a familiar interface without dealing with the underlying code. These tools serve as an entry point for students interested in exploring text mining.

In this version of the tool, students and others can learn to use various functions characteristic of text mining and explore different visualizations related to it. The goal is to demystify text mining as an analytical approach and tool, emphasizing visualization and familiarity. The "corpora" folder contains a number of pre-prepared datasets, or corpora, each featuring various texts in different categories, including fairy tales by H.C. Andersen and the Brothers Grimm, as well as novels by Jane Austen. These corpora enable various analyses, including language translation, temporal development, and genre analysis. Users can easily switch between different corpora and texts, as well as between different analytical tools and visualizations. We recommend that students and others use the pre-prepared datasets to become familiar with the materials and functionalities before applying the tool to their own data. Users can upload single or multiple files, which then serve as a corpus for text mining. This allows users to quickly gain an overview of one or more texts within a corpus. This can serve as a starting point for further analysis or act as an exploratory tool to provide new perspectives on texts and corpora.

## Corpora
This folder contains various datasets related to text mining. Each corpus is sourced from the Royal Library or Project Gutenberg and is in the public domain. Learn more about Project Gutenberg here: https://www.gutenberg.org/

## Stopwords
This folder contains English and Danish stopword lists. Users can view the contents of both lists used in the text mining application.

## Accessing the Material
The text mining application can be run in two ways: via Posit Cloud, accessed through a browser, or locally on your own computer.

## Accessing the Material via a Browser
### 1. Go to Posit Cloud
To access the material via a browser, follow this link: https://posit.cloud/plans/free. You can create a free account or log in if you already have one.

### 2. Run the Program
1. Create a new RStudio project via the 'New project' button.
2. Create a new R script under 'Files' --> 'New file'.
3. Enter the following code into the script:
   ```R
   # Load the necessary packages to run the application. These packages extend the base functionality of R, allowing us to open and work with applications that have a user interface, as in the case of TextMining.
   library(shiny)
   library(thematic)
   library(readtext)
   library(writexl)
   library(DT)
   library(tidyverse)
   library(tidytext)
   library(quanteda)
   library(quanteda.textstats)
   library(ggraph)
   library(igraph)
   library(ggwordcloud)
   library(tidygraph)

   # Here we tell R to open the TextMining application hosted on GitHub under AUL-BSS-Datalab
   runGitHub("TextMining_en", "AUL-BSS-Datalab")
   ```
4. Run the code by pressing Shift + Ctrl + Enter.
5. You can now access the text mining application.

## Accessing the Material Locally
### 1. Install R
Download and install the latest version of R on your computer, choosing the version that matches your operating system. R is the 'language' we will use for programming.  
Download R here: https://posit.co/download/rstudio-desktop/

### 2. Install RStudio
Download and install the latest version of RStudio on your computer, choosing the version that matches your operating system. RStudio is the application where we write our code. Open RStudio when you want to start programming.  
Download RStudio here: https://posit.co/download/rstudio-desktop/

### 3. Run the Program
1. Create a new R script under 'Files'.
2. Enter the following code:
   ```R
   # Load the necessary packages to run the application. These packages extend the base functionality of R, allowing us to open and work with applications that have a user interface, as in the case of TextMining.
   library(shiny)
   library(thematic)
   library(readtext)
   library(writexl)
   library(DT)
   library(tidyverse)
   library(tidytext)
   library(quanteda)
   library(quanteda.textstats)
   library(ggraph)
   library(igraph)
   library(ggwordcloud)
   library(tidygraph)

   # Here we tell R to open the TextMining_en application hosted on GitHub under AUL-BSS-Datalab
   runGitHub("TextMining_en", "AUL-BSS-Datalab")
   ```
3. Run the code by pressing Shift + Ctrl + Enter.
4. You can now access the text mining application.
