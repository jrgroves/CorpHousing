clean <- function(q, min, max){
  
  # 1. Tokenize into character 3-grams (e.g., "John" -> "Joh", "ohn")
  it <- itoken(q$name, 
               preprocessor = tolower, 
               progressbar = TRUE,
               tokenizer = function(x) {
                 tokenizers::tokenize_character_shingles(x, n = 3, 
                                                         strip_non_alphanum = TRUE)
               })
  
  # 2. Create the vocabulary
  stop_words = c("and", "or", "etal", "limited liability corporation", "llc",
                 "incorporated")
  
  vocab <- create_vocabulary(it, stopwords = stop_words)
  
  # 3. Vectorize text into a Document-Term Matrix (DTM)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer, progressbar = TRUE)
  
  # 4. Transform DTM into a sparse TF-IDF Matrix
  tfidf_model <- TfIdf$new()
  dtm_tfidf <- tfidf_model$fit_transform(dtm)
  
  #5. Compute Cosine Similarity Matrix Computing the cross-product of the matrix 
  #gives you a sparse similarity matrix containing all pair combinations.
  
  # Compute the similarity matrix
  similarity_matrix <- sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
  
  # Convert to a triplet matrix representation to easily extract row/col indices
  sparse_triplet <- as(similarity_matrix, "TsparseMatrix")
  
  # Filter pairs (ignore self-comparisons where row == col, and set a threshold)
  matches <- data.frame(
    row_idx = sparse_triplet@i + 1,
    col_idx = sparse_triplet@j + 1,
    score   = sparse_triplet@x) %>%
    filter(row_idx < col_idx,
           score >= min,
           score < max) # Keeps only unique pairs above 50% similarity
  
  #Handoff to RapidFuzz
  # 1. Take the filtered matches dataframe from text2vec
  handoff_pairs <- matches %>%
    mutate(
      string1 = q$name[row_idx],
      string2 = q$name[col_idx]
    )
  
  # 2. Vectorized calculation using RapidFuzz
  # mapply executes the C++ code over the filtered row pairs only
  handoff_pairs$rapidfuzz_score <- mapply(
    function(s1, s2) {
      # Returns an exact 0-100 score
      RapidFuzz::fuzz_WRatio(s1, s2)
    },
    handoff_pairs$string1,
    handoff_pairs$string2
  )
  
  return(handoff_pairs)
  
}