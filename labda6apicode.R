# Load required libraries
library(plumber)
library(tm)
library(SnowballC)
library(textTinyR)
library(dplyr)
library(tidyr)
library(proxy)
library(jsonlite)

# Preprocessing function
preprocess_text <- function(text) {
  if (is.na(text) || text == "") return("")
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))
  text <- wordStem(text, language = "en")
  return(text)
}

# Extract keywords - Simple method
extract_keywords_simple <- function(text) {
  words <- unlist(strsplit(preprocess_text(text), "\\s+"))
  keywords <- unique(words[words != ""])  # Remove empty strings
  return(keywords)
}

# Extract keywords - CFPD method
extract_keywords_cfpd <- function(text, top_n = 5) {
  words <- unlist(strsplit(preprocess_text(text), "\\s+"))
  if (length(words) == 0) return(character(0))
  
  word_freq <- table(words)
  word_positions <- lapply(unique(words), function(w) which(words == w))
  
  cfpd_scores <- sapply(names(word_freq), function(w) {
    tf <- as.numeric(word_freq[w]) / length(words)
    wps <- mean(1 / word_positions[[w]])
    spread <- ifelse(length(word_positions[[w]]) > 1, 1 - (sd(word_positions[[w]]) / length(words)), 1)
    score <- tf * wps * spread
    return(ifelse(is.na(score) | is.nan(score), 0, score))
  }, USE.NAMES = TRUE)
  
  names(sort(cfpd_scores, decreasing = TRUE))[1:min(top_n, length(cfpd_scores))]
}

# Compare keywords
compare_keywords <- function(model_keywords, student_keywords) {
  matched_keywords <- intersect(model_keywords, student_keywords)
  keyword_match_score <- length(matched_keywords) / max(1, length(model_keywords)) * 100  # Avoid division by zero
  return(list(matched_keywords = matched_keywords, keyword_match_score = keyword_match_score))
}

# Calculate cosine similarity
evaluate_answer_score <- function(model_answer, student_answer) {
  text_corpus <- Corpus(VectorSource(c(preprocess_text(model_answer), preprocess_text(student_answer))))
  tdm <- TermDocumentMatrix(text_corpus)
  tdm_matrix <- as.matrix(tdm)
  
  # Check if the matrix has at least 2 columns (2 documents)
  if (ncol(tdm_matrix) < 2) {
    return(0)  # If not enough terms, return similarity as 0
  }
  
  # Compute cosine similarity
  similarity_matrix <- proxy::dist(t(tdm_matrix), method = "cosine")
  
  # Ensure similarity_matrix has valid indices before accessing [1,2]
  if (length(similarity_matrix) == 1) {
    return(1 - as.numeric(similarity_matrix))  # If it's a single value, return its similarity
  }
  
  return(1 - as.numeric(similarity_matrix[1, 2]))  # Convert distance to similarity
}

# Generate score - Simple method
generate_score_simple <- function(cosine_similarity, keyword_match_score) {
  normalized_cosine_similarity <- 1 + 4 * cosine_similarity
  normalized_keyword_match_score <- 1 + 4 * (keyword_match_score / 100)
  final_score <- 0.6 * normalized_cosine_similarity + 0.4 * normalized_keyword_match_score
  return(round(final_score * 2) / 2)
}

# Generate score - CFPD method with Term Importance (TI)
generate_score_cfpd <- function(text) {
  # Preprocess the text
  words <- unlist(strsplit(preprocess_text(text), "\\s+"))
  
  # If no words, return minimum score
  if (length(words) == 0) return(1)
  
  # Calculate Term Frequency (TF)
  word_freq <- table(words)
  total_words <- length(words)
  tf <- word_freq / total_words
  
  # Calculate Entropy (H_w)
  word_prob <- word_freq / total_words
  entropy <- -sum(word_prob * log(word_prob + 1e-9))
  
  # Calculate Term Importance (TI)
  term_importance <- tf * (1 / (1 + entropy))
  
  # Calculate match score
  match_percentage <- sum(term_importance)
  
  # Convert to 1-5 scale
  raw_score <- (match_percentage * 4) + 1
  
  # Ensure score is between 1 and 5, round to nearest 0.5
  return(round(max(1, min(raw_score, 5)) * 2) / 2)
}

#* Evaluate student answer
#* @param model_answer Model's reference answer
#* @param student_answer Student's submitted answer
#* @post /evaluate_answer
function(model_answer, student_answer) {
  # Validate inputs
  if (missing(model_answer) || missing(student_answer)) {
    return(list(
      error = "Both model_answer and student_answer are required",
      status = 400
    ))
  }
  
  # Extract keywords using both methods
  model_keywords_simple <- extract_keywords_simple(model_answer)
  model_keywords_cfpd <- extract_keywords_cfpd(model_answer)
  
  student_keywords_simple <- extract_keywords_simple(student_answer)
  student_keywords_cfpd <- extract_keywords_cfpd(student_answer)
  
  # Compare keywords
  keyword_comparison_simple <- compare_keywords(model_keywords_simple, student_keywords_simple)
  keyword_comparison_cfpd <- compare_keywords(model_keywords_cfpd, student_keywords_cfpd)
  
  # Compute cosine similarity
  cosine_similarity <- evaluate_answer_score(model_answer, student_answer)
  
  # Generate both scores
  score_simple <- generate_score_simple(
    cosine_similarity, 
    keyword_comparison_simple$keyword_match_score
  )
  score_cfpd <- generate_score_cfpd(student_answer)
  
  # Prepare detailed results
  result <- list(
    keywords = list(
      model_keywords_simple = model_keywords_simple,
      model_keywords_cfpd = model_keywords_cfpd,
      student_keywords_simple = student_keywords_simple,
      student_keywords_cfpd = student_keywords_cfpd
    ),
    keyword_match = list(
      simple_match_score = keyword_comparison_simple$keyword_match_score,
      cfpd_match_score = keyword_comparison_cfpd$keyword_match_score
    ),
    cosine_similarity = cosine_similarity,
    scores = list(
      simple_score = score_simple,
      cfpd_score = score_cfpd
    ),
    details = list(
      model_answer = model_answer,
      student_answer = student_answer
    )
  )
  
  return(result)
}

#* Batch evaluate multiple answers
#* @param answers JSON array of answers
#* @post /evaluate_batch
function(answers) {
  # Validate input
  if (missing(answers)) {
    return(list(
      error = "JSON array of answers is required",
      status = 400
    ))
  }
  
  # Parse JSON input
  parsed_answers <- fromJSON(answers)
  
  # Validate input structure
  if (!all(c("model_answer", "student_answer") %in% names(parsed_answers))) {
    return(list(
      error = "Input must be a JSON array with 'model_answer' and 'student_answer' fields",
      status = 400
    ))
  }
  
  # Batch evaluation
  results <- lapply(1:nrow(parsed_answers), function(i) {
    model_answer <- parsed_answers$model_answer[i]
    student_answer <- parsed_answers$student_answer[i]
    
    # Extract keywords using both methods
    model_keywords_simple <- extract_keywords_simple(model_answer)
    model_keywords_cfpd <- extract_keywords_cfpd(model_answer)
    
    student_keywords_simple <- extract_keywords_simple(student_answer)
    student_keywords_cfpd <- extract_keywords_cfpd(student_answer)
    
    # Compare keywords
    keyword_comparison_simple <- compare_keywords(model_keywords_simple, student_keywords_simple)
    keyword_comparison_cfpd <- compare_keywords(model_keywords_cfpd, student_keywords_cfpd)
    
    # Compute cosine similarity
    cosine_similarity <- evaluate_answer_score(model_answer, student_answer)
    
    # Generate both scores
    score_simple <- generate_score_simple(
      cosine_similarity, 
      keyword_comparison_simple$keyword_match_score
    )
    score_cfpd <- generate_score_cfpd(student_answer)
    
    list(
      keywords = list(
        model_keywords_simple = model_keywords_simple,
        model_keywords_cfpd = model_keywords_cfpd,
        student_keywords_simple = student_keywords_simple,
        student_keywords_cfpd = student_keywords_cfpd
      ),
      keyword_match = list(
        simple_match_score = keyword_comparison_simple$keyword_match_score,
        cfpd_match_score = keyword_comparison_cfpd$keyword_match_score
      ),
      cosine_similarity = cosine_similarity,
      scores = list(
        simple_score = score_simple,
        cfpd_score = score_cfpd
      ),
      details = list(
        model_answer = model_answer,
        student_answer = student_answer
      )
    )
  })
  
  return(results)
}

#* Get average scores by student
#* @get /average_scores_by_student
function() {
  submissions_file <- "submissions.json"

 if (!file.exists(submissions_file)) {
 return(list(message = "No submissions available yet.", status = 404))
  }

 submissions <- fromJSON(readLines(submissions_file), simplifyVector = FALSE)

  if (length(submissions) == 0) {
 return(list(message = "No submissions available yet.", status = 404))
  }

 # Calculate average simple score for each student
  average_scores <- tapply(sapply(submissions, function(x) x$evaluation_scores$simple_score),
                          sapply(submissions, function(x) x$student_name),
                           mean)

 return(as.list(average_scores))
}

#* Get all student submissions
#* @get /get_submissions
function() {
  submissions_file <- "submissions.json"

  if (!file.exists(submissions_file)) {
 return(list(message = "No submissions available yet.", status = 404))
  }

  submissions <- fromJSON(readLines(submissions_file), simplifyVector = FALSE)

 return(submissions)
}

#* Save student submission and evaluate
#* @param student_name The name of the student
#* @param question The question text
#* @param student_answer The student's submitted answer
#* @post /save_submission
function(student_name, question, student_answer) {
  # Validate inputs
  if (missing(student_name) || missing(question) || missing(student_answer)) {
    return(list(
      error = "Both student_name, question, and student_answer are required",
      status = 400
    ))
  }

  # Get the model answer for the given question (assuming latest for simplicity)
  # In a more robust app, you'd link submissions to specific questions by ID
  questions_file <- "questions.json"
  model_answer <- ""
  if (file.exists(questions_file)) {
    questions <- fromJSON(readLines(questions_file), simplifyVector = FALSE)
    if (length(questions) > 0) {
      # Find the model answer for the matching question text
      # This assumes question text is unique - use IDs for better practice
      matching_question <- Filter(function(q) q$question == question, questions)
      if (length(matching_question) > 0) {
        model_answer <- matching_question[[1]]$model_answer
      }
    }
  }

  # Evaluate the student's answer using the /evaluate_answer logic
  evaluation_results <- evaluate_answer(model_answer, student_answer) # Reuse existing logic

  # Prepare the submission data
  submission_data <- list(
    student_name = student_name,
    question = question,
    student_answer = student_answer,
    evaluation_scores = evaluation_results$scores # Include the scores
  )

  submissions_file <- "submissions.json"

  # Read existing submissions or initialize an empty list
  if (file.exists(submissions_file)) {
    existing_submissions <- fromJSON(readLines(submissions_file), simplifyVector = FALSE)
    updated_submissions <- c(existing_submissions, list(submission_data))
  } else {
    updated_submissions <- list(submission_data)
  }

  # Write the updated submissions back to the file
  write(toJSON(updated_submissions, auto_unbox = TRUE, pretty = TRUE), submissions_file)

  return(list(message = "Submission saved and evaluated successfully.", status = 200, evaluation = evaluation_results))
}

#* Health check endpoint
#* @get /health
function() {
  return(list(
    status = "healthy",
    version = "1.0.0",
    description = "Answer Evaluation API with Two Scoring Methods"
  ))
}

#* Add a new student
#* @param student_name The name of the student to add
#* @post /add_student
function(student_name) {
  if (missing(student_name) || student_name == "") {
 return(list(error = "Student name is required.", status = 400))
  }

  students_file <- "students.json"

  if (file.exists(students_file)) {
    existing_students <- fromJSON(readLines(students_file), simplifyVector = FALSE)
    updated_students <- c(existing_students, list(student_name))
  } else {
    updated_students <- list(student_name)
  }
  write(toJSON(updated_students, auto_unbox = TRUE, pretty = TRUE), students_file)
  return(list(message = "Student added successfully.", status = 200))
}

#* Save question and model answer
#* @param question The question text
#* @param model_answer The model answer text
#* @post /save_question
function(question, model_answer) {
  if (missing(question) || missing(model_answer)) {
    return(list(
      error = "Both 'question' and 'model_answer' are required.",
      status = 400
    ))
  }

  new_question <- list(question = question, model_answer = model_answer)
  questions_file <- "questions.json"

  if (file.exists(questions_file)) {
    existing_questions <- fromJSON(readLines(questions_file), simplifyVector = FALSE)
    updated_questions <- c(existing_questions, list(new_question))
  } else {
    updated_questions <- list(new_question)
  }

  write(toJSON(updated_questions, auto_unbox = TRUE, pretty = TRUE), questions_file)

  return(list(message = "Question and model answer saved successfully.", status = 200))
}

#* Get the latest question and model answer
#* @get /get_question
function() {
  questions_file <- "questions.json"

  if (!file.exists(questions_file)) {
    return(list(message = "No questions available yet.", status = 404))
  }

  questions <- fromJSON(readLines(questions_file), simplifyVector = FALSE)

  if (length(questions) == 0) {
    return(list(message = "No questions available yet.", status = 404))
  }
  return(questions[[length(questions)]]) # Return the latest question and model answer
}