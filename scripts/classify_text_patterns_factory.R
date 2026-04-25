
###############################################
# Generalized Text Classification Function Factory
###############################################

make_classifier <- function(true_patterns, false_patterns) {

  # Ensure patterns are lowercased
  true_patterns  <- tolower(true_patterns)
  false_patterns <- tolower(false_patterns)

  function(x, return_detail = FALSE, extra_true = NULL, extra_false = NULL) {

    # Lowercase text for case-insensitive matching
    x_lower <- tolower(x)

    # Allow extra patterns at runtime
    all_true_patterns  <- c(true_patterns,  tolower(extra_true))
    all_false_patterns <- c(false_patterns, tolower(extra_false))

    # Collapse patterns into single alternations
    true_re  <- paste(all_true_patterns, collapse = "|")
    false_re <- paste(all_false_patterns, collapse = "|")

    # Boolean TRUE/FALSE per text
    has_true  <- grepl(true_re,  x_lower, perl = TRUE)
    has_false <- grepl(false_re, x_lower, perl = TRUE)

    # Final classification
    status <- ifelse(
      has_true & !has_false, TRUE,
      ifelse(!has_true & has_false, FALSE, NA)
    )

    # Basic logical output
    if (!return_detail) {
      return(status)
    }

    # For detail: find first matching pattern
    first_match <- function(txt, pats) {
      if (is.null(pats) || length(pats) == 0) return(NA_character_)
      hits <- pats[vapply(pats, function(p) grepl(p, txt, perl = TRUE), logical(1))]
      if (length(hits) == 0) return(NA_character_)
      hits[[1L]]
    }

    true_pattern  <- vapply(x_lower, first_match, character(1), pats = all_true_patterns)
    false_pattern <- vapply(x_lower, first_match, character(1), pats = all_false_patterns)

    data.frame(
      text                  = x,
      classification        = status,
      matched_true_pattern  = true_pattern,
      matched_false_pattern = false_pattern,
      stringsAsFactors      = FALSE
    )
  }
}

###############################################
# Specialized Interview Classifier
###############################################

interview_true_patterns <- c(
  "completed interview",
  "spoke with patient",
  "answered all questions",
  "interview completed with rn",
  "\\bspoke with (mother|father|caregiver)\\b",
  "completed investigation",
  "investigation completed",
  "provided education",
  "educated on disease"
)

interview_false_patterns <- c(
  "\\b(vm|lvm)\\b",
  "left voicemail",
  "no answer",
  "lvm x ?[0-9]+",
  "out of jurisdiction",
  "reviewed (mr|epic|medical record|records)",
  "record review only",
  "attempted to reach",
  "unable to reach",
  "spoke with hospital staff",
  "confirmed with (nurse|doctor|physician)"
)

# Create the specialized classifier
interview_classifier <- make_classifier(
  true_patterns  = interview_true_patterns,
  false_patterns = interview_false_patterns
)
