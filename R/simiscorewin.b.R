
# This file is a generated template, your changes will not be overwritten

SimiScoreWinClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "SimiScoreWinClass",
    inherit = SimiScoreWinBase,
    private = list(
        .run = function() {

          #Defining Data Object
          my_data<- self$data
          
          #Options
          sotext<- self$options$sotext
          tatext<- self$options$tatext
          
          ### Defined Functions ###
          # Pre-processing data with defined function
          corpus_preprocessing = function(corpus){
            # Replace special symbols with space 
            toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
            # Normalization
            corpus <- tm_map(corpus, toSpace, "/")
            corpus <- tm_map(corpus,toSpace,"@")
            corpus <- tm_map(corpus,toSpace,"\\｜")
            corpus <- tm_map(corpus,toSpace,"#")
            corpus <- tm_map(corpus, toSpace, "®")
            # Casing (upper case & lower case), convert the text to lower case
            corpus <- tm_map(corpus, content_transformer(tolower))
            # Remove punctuation 
            corpus <- tm_map(corpus, removePunctuation)
            # Remove extra white space 
            corpus <- tm_map(corpus, stripWhitespace)
            # Remove Stop words
            corpus <- tm_map(corpus,removeWords,stopwords("english"))
            corpus <- tm_map(corpus,removeWords,c("the","and","The","And","A","An","a","an","e","d"))
            # Stemming (e.g. -ing vs original)
            corpus <- tm_map(corpus,stemDocument, language ="english")
            return(corpus)
          }
          
          # Calculate cosine similarity with TF-IDF
          cos_sim = function(matrix){
            numerator = matrix %*% t(matrix)
            A = sqrt(apply(matrix^2, 1, sum))
            denumerator = A %*% t(A)
            return(numerator / denumerator)
          }
          
          #Initialize Data variables
          #Creating Data Object
          if (is.null(self$options$tatext) ||
              is.null(self$options$sotext))
            return()
          
          simi_df<- data.frame(
            d_sotext<- self$data[[self$options$sotext]],
            d_tatext<- self$data[[self$options$tatext]]
          )
          
          
          # Change column name of the documents
          names(simi_df)[1] = "doc_id"
          names(simi_df)[2] = "text"
          
          # Transfer the data into corpus 
          doc_corpusSimi = VCorpus(DataframeSource(simi_df))
          
          # Apply data pre-processing to corpus
          corpus_cleanedSimi <- corpus_preprocessing(doc_corpusSimi)
          
          #Create a Document Term matrix, which containing the frequency of the words
          #each row represents a document/text message 
          #each column represents a distinct text/name
          #each cell is a count of the token for a document/text message
          doc_dtmSimi <- DocumentTermMatrix(corpus_cleanedSimi)
          dtm_mSimi <- as.matrix(doc_dtmSimi)
          
          # Apply TF-IDF Weighting 
          tfidf_Simi <- DocumentTermMatrix(doc_corpusSimi,control = list(weighting = weightTfIdf))
          tfidf_mSimi = as.matrix(tfidf_Simi)
          
          # Calculate Cosine Similarity 
          tfidf_cos_simSimi = cos_sim(dtm_mSimi)
          
          
          #Results
          results<- tfidf_cos_simSimi[1:ncol(tfidf_cos_simSimi)]
          self$results$text$setContent(results)
        })
)
