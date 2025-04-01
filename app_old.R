
#-----------------------------------Anvendte biblioteker-----------------------------------

#Hent biblioteker
library(shiny) #https://cran.r-project.org/web/packages/shiny/index.html
library(thematic) #https://cran.rstudio.com/web/packages/thematic/index.html
library(readtext) #https://cran.r-project.org/web/packages/readtext/index.html
library(writexl) #https://cran.r-project.org/web/packages/writexl/index.html
library(tidyverse) #https://cran.r-project.org/web/packages/tidyverse/index.html
library(tidytext) #https://cran.r-project.org/web/packages/tidytext/index.html
library(quanteda) #https://cran.r-project.org/web/packages/quanteda/index.html
library(quanteda.textstats) #https://cran.r-project.org/web/packages/quanteda.textstats/index.html
library(ggraph) #https://cran.r-project.org/web/packages/ggraph/index.html
library(igraph) #https://cran.r-project.org/web/packages/igraph/index.html
library(ggwordcloud) #https://cran.r-project.org/web/packages/ggwordcloud/index.html
library(tidygraph)

#------------------------------------- Stop words -------------------------------------------

#Indlæs den danske stopordsliste fra Stop_Words mappen 
stop_words_da <- read.csv("Stopwords/stop_words_da.txt")
#Lav en ny liste indeholdene selvvalgte stopord
my_stop_words <- data.frame(word = c("miss", "mrs", "sir", "mr"))

#-----------------------------------Shiny App------------------------------------------------

#---------------------------------- Definer UI-----------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinyLayout.css")
  ),   
  
  titlePanel(title = "Text Mining"),
  thematic::thematic_shiny(),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 br(),
                 #Upload fil/filer 
                 fileInput("files",
                           label="Upload files",
                           multiple = TRUE),
                 textOutput("file_size_info"),
                 br(),
                 #Vælg encoding
                 radioButtons(inputId = "corpus_encoding",
                              label = "Choose an encoding for the corpus",
                              choices = c("UTF-8",
                                          "latin1"),
                              selected = "UTF-8"),
                 br(),
                 #Vælg sprog for stopordsliste
                 radioButtons(inputId = "language_stopwords",
                              label = "Choose a language for the stopwords",
                              choices = c("da",
                                          "en"),
                              selected = "en"),
                 br(),
                 #Fjern ord fra tekster
                 selectizeInput(inputId = "remove_word",
                                label = "Search to remove words from the corpus",
                                choices = c("choose" = "", my_stop_words),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "", create = TRUE)),
                 #Liste til selvvalgte stopord
                 verbatimTextOutput("list_removed_word"),
                 br(),
                 textOutput("help_info"),
                 br(),
                 br(),
                 tags$img(src = "logo-digital-white.svg", height="75%", width ="75%")),
    mainPanel(
      #Danner et menu-layout, hvor det er muligt at skifte mellem visualiseringerne
      tabsetPanel(type = "tabs",
                  tabPanel("Overview",
                           br(),
                           h4("Info"),
                           helpText("On this page, you will get an overview of the corpus"),
                           br(),
                                  textOutput("text_doc_sum"),
                                  br(),
                                  textOutput("text_token_sum"),    
                                  br(),
                                  textOutput("text_token_unique"),
                                  br(),
                           DT::DTOutput("text_token_sum_doc")),
                  tabPanel("Close reading",
                           br(),
                           h4("Info"),
                           helpText("Here it is possible to see an excerpt from the corpus, where stop words have either been removed or still appear"),
                           column(12,
                                  br(),
                                  radioButtons(inputId = "selected_stopword_view", 
                                               label = "View text with or without stopwords",
                                               choices = c("With", "Without"), 
                                               selected = "With")),
                           br(),
                           textOutput("viz_text")),
                  tabPanel("Bar chart",
                           br(),
                           h4("Info"),
                           helpText("The visualization shows the most frequently occurring words in the corpus as a whole and in the individual texts"),
                           column(3,
                                  br(),
                                  radioButtons(inputId = "selected_corpora_or_text",
                                               label = "View for the entire corpus or see the distribution in the texts",
                                               choices = c("Corpus",
                                                           "Texts"),
                                               selected = "Corpus")),
                           column(3,
                                  br(),
                                  sliderInput(inputId = "slice_size",
                                              label = "Choose the number of words in the visualization between 5 and 20",
                                              min = 5, max = 20, value = 5, step = 5)),
                           plotOutput("viz_plot")),
                  tabPanel("Wordcloud",
                           br(),
                           h4("Info"),
                           helpText("The visualization shows the most frequently occurring words in the corpus as a whole and in the individual texts"),
                           column(3,
                                  br(),
                                  radioButtons(inputId = "selected_corpora_or_text_cloud",
                                               label = "View for the entire corpus or see the distribution in the texts",
                                               choices = c("Corpus",
                                                           "Texts"),
                                               selected = "Corpus")),
                           column(3,
                                  br(),
                                  sliderInput(inputId = "word_freq_cloud", 
                                              label = "Choose the number of words in the visualization between 5 and 30",
                                              min = 5, max = 30, value = 20, step = 5)),
                           plotOutput("viz_wordcloud")),
                  tabPanel("Bigrams",
                           br(),
                           h4("Info"),
                           helpText("The visualization shows the word pairs that occur in the corpus. Adjust the minimum occurrence of word pairs - the word pair appears in the corpus at least x number of times"),
                           column(3,
                                  br(),
                                  #Definerer funktionen, hvor det er muligt at vælge minimum frekvens for ordene i visualiseringen
                                  sliderInput(inputId = "wordpair_freq_bigrams", 
                                              label = "Select the minimum frequency for word pairs in the visualization between 1 and 50",
                                              min = 1, max = 50, value = 10, step = 2)),
                           plotOutput("viz_bigrams")),
                  tabPanel("Context",
                           br(),
                           h4("Info"),
                           helpText("The visualization shows the context in which a searched word or phrase appears"),
                           br(),
                           column(3,
                                  br(),
                                  #Definerer funktionen, der gør det muligt at fremsøge et keyword
                                  textInput(inputId = "select_kwic",
                                            label = "Search here for a word or a phrase",
                                            value = "")),
                           column(3,
                                  br(),
                                  sliderInput(inputId = "window_context", 
                                              label = "Choose the size of the word's context",
                                              min = 1, max = 50, value = 1, step = 1)),
                           DT::DTOutput("viz_context"),
                           downloadButton(outputId ="download_KWIC", 
                                          label = "Download tabel")),
                  tabPanel("Clarification of concepts",
                           br(),
                           h4("Info"),
                           helpText("On this page, you will find definitions and explanations of the concepts and visualizations used in the application"),
                           br(),
                           h4("Upload files:"),
                           textOutput("term_info_text_1"),
                           br(),
                           h4("Choose encoding for the corpus:"),
                           textOutput("term_info_text_11"),
                           br(),
                           h4("Stopwords:"),
                           textOutput("term_info_text_2"),
                           br(),
                           h4("Choose a language for the stopwords:"),
                           textOutput("term_info_text_3"),
                           br(),
                           h4("Search to remove words from the corpus:"),
                           textOutput("term_info_text_4"),
                           br(),
                           h4("Overview:"),
                           textOutput("term_info_text_10"),
                           br(),
                           h4("Close reading:"),
                           textOutput("term_info_text_5"),
                           br(),
                           h4("Bar chart:"),
                           textOutput("term_info_text_6"),
                           br(),
                           h4("Wordcloud:"),
                           textOutput("term_info_text_7"),
                           br(),
                           h4("Bigrams:"),
                           textOutput("term_info_text_8"),
                           br(),
                           h4("Context:"),
                           textOutput("term_info_text_9"),
                           br(),
                           h4("If you want to know more:"),
                           textOutput("reference_info_text"),
                           br())
                  

      )
    )
  )
)

#------------------------- Definer server logic -------------------------------------------

server <- function(input, output) {
  #Gør det muligt at uploade filer på 30MB frem for default værdien på 5MB
  options(shiny.maxRequestSize=30*1024^2)
  
#------------------------ Fjern ord fra korpora --------------------------------------------
 #Lav dataframe reaktiv
  remove_word_df <- reactiveVal()
  #Definerer at de ord, der skal fjernes fra teksten stammer fra inputtet
  removed_word <- reactive({
    req(input$remove_word)
    data.frame(word = input$remove_word)
  })
  #Tilføj data til dataframe
  observeEvent(input$remove_word, {
    temp_df <- rbind(remove_word_df(), removed_word())
    remove_word_df(temp_df)
  })
  #Info om ventetid ved store filer
  output$file_size_info <- renderText({
    paste("Large files take longer to upload than small ones. Please be patient if the visualizations do not appear immediately.")
  })
  #Info tekst om begrebsafklaring
  output$help_info <- renderText({
    paste("Are you unsure about a term, a formulation, a calculation, or something else? Find answers under the Concept Clarification tab.")
  })
  
#-------------------------- Indlæs og klargøring filer --------------------------------------------------------  
  
  #Tidy version uden stopord (engelsk)
  tidy_corpus <- reactive({
    req(input$files)
    #Læs teksten fra filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words)
  })
  
  #Tidy version uden stopord (dansk)
  tidy_corpus_da <- reactive({
    req(input$files)
    #Læs teksten fra filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words_da)
  })
  
  #Tidy version med stopord
  tidy_corpus_with_stopwords <- reactive({
    req(input$files)
    #Læs teksten fra txt filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(word, text)
  })
  
  #Forbered tidy version af bigrams (engelsk)
  tidy_bigram <- reactive({
    req(input$files)
    #Læs teksten fra txt filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words$word) %>% 
      filter(!word2 %in% stop_words$word)
  }) 
  
  #Forbered tidy version af bigrams (dansk)
  tidy_bigram_da <- reactive({
    req(input$files)
    #Læs teksten fra txt filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>% 
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words_da$word) %>% 
      filter(!word2 %in% stop_words_da$word)
  }) 
  
  #Lav en corpus version af teksterne, der passer til quanteda pakken
  context_corpus <- reactive({
    req(input$files)
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    #Lav corpus
    create_corpus <- corpus(raw_corpus)
    #Rediger docnames til title, så de bliver meningsfulde og identificerbare
    context_docid <- paste(raw_corpus$title)
    docnames(create_corpus) <- context_docid
    #Tokenize corpus teksterne
    create_corpus <- tokens(create_corpus, remove_separators = TRUE)
  })
  
  #Lav en corpus version af teksterne, der passer til quanteda pakkens readability funktion
  context_corpus_LIX <- reactive({
    req(input$files)
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    #Lav corpus
    create_corpus <- corpus(raw_corpus)
  })

#---------------------------- Oversigt ---------------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$text_token_sum_doc <- DT::renderDT({
    selected_text_data_sum <- context_corpus()
    selected_text_data_sum_LIX <- context_corpus_LIX()
    
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_sum <- selected_text_data_sum %>% 
        filter(!word %in% remove_word) 
    }
    #Antal documenter i korporaet
    doc_sum <- ndoc(selected_text_data_sum)
    output$text_doc_sum <- renderText({paste("Number of texts in the corpus: ", doc_sum)})
    #Antal tokens i alt
    token_sum <- sum(ntoken(selected_text_data_sum, remove_punct = TRUE))
    output$text_token_sum <- renderText({paste("The total number of words in the corpus: ", token_sum)})
    #Antal unikke tokens
    token_unique <- sum(ntype(selected_text_data_sum, remove_punct = TRUE))
    output$text_token_unique <- renderText({paste("Number of unique words in the corpus: ", token_unique)})
    #Antal ord i de forskellige tekster
    tokens_in_texts <- ntoken(selected_text_data_sum, remove_punct = TRUE)
    
    #LIX beregning for de forskellige tekster
    LIX <- textstat_readability(selected_text_data_sum_LIX, "LIW", min_sentence_length = 2)
    
    #Lav en data frame over antal ord i hver tekst
    text_info_df <- data.frame(docnames(selected_text_data_sum), tokens_in_texts, LIX)
    #Fjern ekstra kolonne med information om tekstens placering i corpus
    text_info_df <-  subset(text_info_df, select = -document)
    
    #Ændrer navnet på kolonnerne
    names(text_info_df)[2] <- "Number of words"
    names(text_info_df)[3] <- "LIX"
    
    #Fjern ekstra kolonne med samme værdi som tekst kolonnen
    text_info_df <- subset(text_info_df, select = c("Number of words", "LIX"))
    text_info_df
  }, options = list(language = list(search = "Filter in the table")))
  
  
#---------------------------- Nærlæs_tekst ----------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_text <- renderText({
    #Skift mellem tekster med og uden stopord, alt efter hvilken knap, der er aktiveret
    if (input$selected_stopword_view == "With"){
      selected_text_data_read <- tidy_corpus_with_stopwords()

    } else if (input$selected_stopword_view == "Without" & input$language_stopwords == "en"){
      selected_text_data_read <- tidy_corpus()
      
    } else if (input$selected_stopword_view == "Without" & input$language_stopwords == "da"){
      selected_text_data_read <- tidy_corpus_da()
    }
    
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_read <- selected_text_data_read %>% 
        filter(!word %in% remove_word) 
    }
    
    #Visualisering af den fulde tekst
    head(selected_text_data_read$word, 1000)
    
  })

#------------------------ Søjlediagram --------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_plot <- renderPlot({
    if (input$language_stopwords == "en"){
      selected_text_data_plot <- tidy_corpus()

      }else if(input$language_stopwords == "da"){
        selected_text_data_plot <- tidy_corpus_da()
      }
    
    #Definerer at antallet at ord i visualiseringen skal matche inputtet herfor
    slice_size <- input$slice_size
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_plot <- selected_text_data_plot %>% 
        filter(!word %in% remove_word) 
    }
    
    #Visualiseringer af søjlediagram baseret på hele korpora eller enkelte tekster
    #vis enkelte tekster
    if (input$selected_corpora_or_text == "Texts"){
      selected_text_data_plot %>%
        group_by(title) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(title = as.factor(title),
               word = reorder_within(word, n, title)) %>% 
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col() +
        scale_fill_gradient(low = "#CAF0FE", high = "#002E70")+
        facet_wrap( ~ title, ncol = 4, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "The frequency of word occurrences in the text", 
             x = "Word", 
             y = "Frequency", 
             fill = "Frequency")
      
      
    #vis hele corpus 
   } else if (input$selected_corpora_or_text == "Corpus"){
      selected_text_data_plot %>%
        count(word, sort = TRUE) %>% 
        mutate(word = reorder(word, n)) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(low = "#CAF0FE", high = "#002E70") +
        geom_label(aes(x = word, y = n, label = n),    
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "The frequency of word occurrences in the corpus",
             x = "Word", 
             y = "Frequency", 
             fill = "Frequency")}
  })
  
#-------------------------------------- Wordcloud --------------------------------------------

output$viz_wordcloud <- renderPlot({
  #Får output til at matche input når der skiftes mellem teksterne
  if (input$language_stopwords == "en"){
    selected_text_data_cloud <- tidy_corpus()
    
  }else if(input$language_stopwords == "da"){
    selected_text_data_cloud <- tidy_corpus_da()
  }
  
  #Definerer at antal, der ønskes vist, kommer fra inputtet herfor
  word_freq_cloud <- input$word_freq_cloud
  #Definerer at ordet, der ønskes fjernet fra teksten, kommer fra inputtet herfor
  remove_word <- input$remove_word
  #Sorterer selvalgt stopord fra den valgte tekst
  if (!is.null(remove_word)){
    selected_text_data_cloud <- selected_text_data_cloud %>% 
      filter(!word %in% remove_word) 
  }
  #Visualiseringer af  wordcloud baseret på hele korpora eller enkelte tekster
  #vis enkelte tekster
  if (input$selected_corpora_or_text_cloud == "Texts"){
    selected_text_data_cloud %>%
      group_by(title) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ungroup() %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      facet_wrap(~title) +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient(low = "#CAF0FE", high = "#002E70")
    #vis hele corpus
  } else if (input$selected_corpora_or_text_cloud == "Corpus"){
    selected_text_data_cloud %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient(low = "#CAF0FE", high = "#002E70")
  }
  
 })
  
#------------------------- Bigrams --------------------------------------------------------
  #Visualisering ag Bigrams som netværksgraf
  output$viz_bigrams <- renderPlot({
    #Får output til at matche input når der skiftes mellem teksterne
    if (input$language_stopwords == "en"){
      selected_text_data_bigrams <- tidy_bigram()
      
    }else if(input$language_stopwords == "da"){
      selected_text_data_bigrams <- tidy_bigram_da()
    }
    
    #Definerer at antallet at ord i visualiseringen skal matche inputtet herfor
    wordpair_freq_bigrams <- input$wordpair_freq_bigrams
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_bigrams <- selected_text_data_bigrams %>% 
        filter(!word1 %in% remove_word) %>% 
        filter(!word2 %in% remove_word)
    }
    
    #Sikrer den samme visualisering
    set.seed(20)
    
    #Definer udseende på pilen, der markerer relationen mellem bigrams
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    #vis enkelte tekster
    viz_bigrams_graph <- selected_text_data_bigrams %>%
        count(word1, word2, sort = TRUE) %>%
        filter(n >= wordpair_freq_bigrams) %>% 
        graph_from_data_frame()
      
      ggraph(viz_bigrams_graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n),
                       show.legend = FALSE,
                       arrow = a,
                       end_cap = circle(.05, 'inches'),
                       color = "#002E70") +
        geom_node_point(size = 2, colour = "#002E70") +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
  
  })
  #------------------------ Kontekst --------------------------------------------------
  
  #Visualisering af kwic som tabel
  output$viz_context <- DT::renderDT({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_context <- context_corpus()
      
    #Definerer at antal ord, der ønskes vist, kommer fra inputtet herfor
    window_context <- input$window_context
    
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_context <- selected_text_data_context %>% 
        tokens_remove(remove_word) 
    }
    
    #Definerer at keyworded kommer fra inputtet herfor
    select_kwic <- input$select_kwic
    
    #KWIC visualisering
    KWIC <<- kwic(selected_text_data_context, pattern = phrase(select_kwic), window = window_context)

  }, options = list(language = list(zeroRecords = "Search for words or phrases to see the results", search = "Filter in the table")))
  
  #Download KWIC tabel til en xlsx (Excel) fil
  output$download_KWIC <- downloadHandler(
    filename = function() { "context_tabel.xlsx"},
    content = function(file) {write_xlsx(KWIC, path = file)
    })
  
#--------------------------- Begrebsafklaring ------------------------------------------------
  output$term_info_text_1 <- renderText({
    paste("Here you can upload one or more files that serve as a corpus in the visualizations. To achieve the best results, the files should be in txt or pdf format. The maximum file size is 30MB per file.")
  })
  
  output$term_info_text_2 <- renderText({
    paste("Stop words are the words that appear in a text or a corpus without being meaningful in this context. This means words like the Danish words 'og', 'i', 'at' and the English words 'the', 'a', 'an', etc.")
  })
  
  output$term_info_text_3 <- renderText({
    paste("The stop word lists contain, respectively, English and Danish stop words. The integrated stop word list for English texts comes from the R package tidytext and includes words from the three lexicons: onix, SMART, and snowball. The stop word list for Danish texts has been compiled by information specialists at the Royal Library. Both stop word lists can be found in the Stopwords folder.")
  })
  
  output$term_info_text_4 <- renderText({
    paste("Remove words from the text that are not already registered as stop words, but rather words that are not relevant to the specific corpus or analysis.")
  })
  
  output$term_info_text_5 <- renderText({
    paste("In this visualization, an excerpt of text corresponding to 1000 words for the specific corpus is displayed. Here, it is possible to view a text with or without stop words. This helps illustrate the impact that removing stop words can have on a corpus and its content.")
  })
  
  output$term_info_text_6 <- renderText({
    paste("In this visualization, the most frequently occurring words and their frequency counts are displayed as a bar chart. It is possible to view the most common words for the entire corpus or distributed across individual texts. You can adjust how many words are displayed in the visualization—whether you want to show the top five, ten, or twenty words.")
  })
  
  output$term_info_text_7 <- renderText({
    paste("In this visualization, the most frequently occurring words and their frequency counts are displayed as a word cloud. It is possible to see the most common words for the entire corpus or distributed across individual texts. You can adjust how many words are displayed in the visualization—whether you want to show the top five, ten, or twenty words. The larger a word appears in the visualization, the more times it occurs in the corpus or in the individual text.")
  })
  
  output$term_info_text_8 <- renderText({
    paste("In this visualization, word pairs that occur in the corpus are displayed. You can change the minimum occurrence of a word pair in the visualization—meaning a word pair must occur at least five, ten, or twenty times in the corpus before it is shown in the visualization. The arrows indicate the order in which the words appear; if the arrow points from 'a' to 'b', then 'a' appears before 'b' in the word pair.")
  })
  
  output$term_info_text_9 <- renderText({
    paste("In this visualization, the context in which a word or phrase appears is displayed. This method in text mining is called Key Word in Context (KWIC). It is possible to search for a single word or a phrase consisting of multiple words. Additionally, you can use * in your search. This will yield more results, as it allows you to search for all endings and not just the specified word—if you search for the word 'træ' followed by * (træ*), you will get results with træet, træer, træerne, etc. The 'docname' column describes the title of the text in the corpus where the word and context appear. The name is followed by a number that provides a more specific description of where the word or phrase is located. The 'from' and 'to' columns describe the position of the word or phrase in the sentence and are relevant when you are interested in their specific placement. The 'pre' and 'post' columns describe the context in which the word appears, i.e., the words that come before and after the searched word(s). The 'keyword' column contains information about the searched word and how it is written in the text. The 'pattern' column contains the actual input. You can see the frequency of the searched word or phrase in the corpus at the bottom left corner of the table—here it is described as entries.")
  })
  
  output$term_info_text_10 <- renderText({
    paste("Displays an overview of the specific corpus. The number of documents present in the corpus, the total word count in the corpus, as well as the word count in each individual text. The number of unique words is a calculation of how many different words appear in the corpus. In this way, each word counts only once and is not affected by whether it appears in the corpus one or more times. The LIX score is a calculation of a text's difficulty in terms of readability. The definition of LIX is from Björnsson (1968). The calculation of LIX is made using the following formula: ASL + ((100 * Nwsy >= 7)/Nw). Here, ASL = Average Sentence Length (i.e., the total number of words divided by the number of sentences), Nw = number of words, and Nwsy = number of word syllables. This is targeted at >= 7 (i.e., words with more than six letters). Additionally, another parameter is used in the calculation: min_sentence_length = 2. This parameter aims to define the minimum length for a sentence based on the number of words. Here, a sentence is defined as what is found before a period. By setting the limit at two instead of one, we avoid counting 'false' sentences. This means that sentences that start with '1.000' or 'H.C. Andersen' are not considered sentences.")
  })
  
  output$term_info_text_11 <- renderText({
    paste("Allow the selection between different text encodings. This is particularly relevant for texts containing Danish special characters: æ, ø, and å. Here, Latin1 may be advantageous. UTF-8 is the universal standard for text encoding, and it is often best suited for English-language texts")
  })
  
  output$reference_info_text <- renderText({
    paste("The text mining application is based on tidyverse and quanteda principles for data processing and visualization. It utilizes tidyverse and tidytext in the tabs Close Read Text, Bar Chart, Word Cloud, and Bigram. You can read more about this at the following link: https://www.tidytextmining.com/. The tabs containing Overview and Context are based on fundamental principles within quanteda. Learn more about this at the following link: http://quanteda.io/.")
  })  
}

#--------------------------- Kør appen ------------------------------------------------------
shinyApp(ui, server)

