rm(list = ls())
gc()

load("./Build/Output/Own10.RData")

library(RapidFuzz)
library(tidyverse)
library(text2vec)
library(Matrix)
library(igraph)

sub_own <- OWN %>%
  filter(year == 2010) %>%
  arrange(co_name) %>%
  filter(tenure == "NONOWNER") %>%
  mutate(ID = seq(1:n()),
         name = as.character(co_name),
         name = case_when(name == "1 70 investments limited liability corporation" ~ 
                            "I 70 investments limited liability corporation",
                          TRUE ~ name)) %>%
  select(ID, name) %>%
  filter(ID < 10000)

# 1. Tokenize into character 3-grams (e.g., "John" -> "Joh", "ohn")
it <- itoken(sub_own$name, 
             preprocessor = tolower, 
             tokenizer = function(x) {
               tokenizers::tokenize_character_shingles(x, n = 3, 
                                                       strip_non_alphanum = FALSE)
})

# 2. Create the vocabulary
vocab <- create_vocabulary(it)

# 3. Vectorize text into a Document-Term Matrix (DTM)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# 4. Transform DTM into a sparse TF-IDF Matrix
tfidf_model <- TfIdf$new()
dtm_tfidf <- tfidf_model$fit_transform(dtm)

#4. Compute Cosine Similarity MatrixComputing the cross-product of the matrix 
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
         score >= 0.89,
         score < 0.9999999999) # Keeps only unique pairs above 50% similarity

#Handoff to RapidFuzz
# 1. Take the filtered matches dataframe from text2vec
handoff_pairs <- matches %>%
  mutate(
    string1 = sub_own$name[row_idx],
    string2 = sub_own$name[col_idx]
  )

# 2. Vectorized calculation using RapidFuzz
# mapply executes the C++ code over the filtered row pairs only
handoff_pairs$rapidfuzz_score <- mapply(
  function(s1, s2) {
    # Returns an exact 0-100 score
    RapidFuzz::fuzz_ratio(s1, s2)
  },
  handoff_pairs$string1,
  handoff_pairs$string2
)


#Now we are creating a unified master list of the corrected names

list <- final_validated_duplicates %>%
  select(string2) %>%
  distinct() 

master <- as.character(list)
temp2 <- as.character(temp[,2])

Results <- lapply(temp2, function(q) {
  best <- extract_best_match(q, master, score_cutoff = 90)
  data.frame(
    query = q,
    best_match = best$choice,
    score = round(best$score, 2))
})

results <- do.call(rbind, Results)



# 1. Create an igraph object from the verified duplicate pairs
g <- graph_from_data_frame(
  d = final_validated_duplicates[, c("string1", "string2")], 
  directed = FALSE
)

# 2. Find connected components (clusters of duplicates)
comp <- components(g)

# 3. Create a look-up dataframe of names and their assigned Cluster ID
cluster_lookup <- data.frame(
  name = names(comp$membership),
  cluster_id = as.vector(comp$membership),
  stringsAsFactors = FALSE
)

master_list <- temp %>%
  # Merge cluster IDs back to all original names
  left_join(cluster_lookup, by = "name") %>%
  # Assign unique IDs to names that had no duplicates (which are currently NA)
  mutate(
    cluster_id = ifelse(
      is.na(cluster_id), 
      max(cluster_id, na.rm = TRUE) + row_number(), 
      cluster_id
    )
  ) %>%
  # Normalize the Cluster ID into a clean Master ID string
  mutate(master_id = paste0("UID_", dense_rank(cluster_id))) %>%
  # Group by the Master ID to pick one "Standardized Name" per cluster
  group_by(master_id) %>%
  mutate(standardized_name = first(name)) %>% # Pick the first occurrence as the standard
  ungroup() %>%
  select(ID, original_name = name, master_id, standardized_name)

write.csv(master_list, file = "temp.csv")
