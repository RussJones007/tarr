# ------------------------------------------------------------------------------------------------------------------->
# Script: text_classifier.r
# Description:  A general text classfier function factory and an instance of a function 
# to classify notes/encounters.
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: December 2, 2025
# 
# ------------------------------------------------------------------------------------------------------------------->


#' Create Text Classifier
#'
#' `make_classifier()` is a function factory for creating a text classifier function. To generate a classifier, two
#' character vectors of regular expressions are supplied: one containing patterns that indicate a classification of
#' `TRUE`, and another containing patterns that indicate `FALSE`. An example of a classifier created using
#' `make_classifier()` is the `case_interview_classifier()` function. The pattern vectors used for that classifier are
#' documented in the Details section below.
#'
#' The TRUE and FALSE pattern vectors captured inside the returned classifier may be retrieved or set programmatically
#' using the helper function [`classifier_patterns()`].
#'
#' @param true_patterns,false_patterns Character vectors of regular expressions.
#' @param ignore_case Logical, default `TRUE`. Set to `FALSE` to create a case-sensitive classifier.
#' @param bias determines which pattern to use when both patterns are positive.  Defaults to the
#' true_pattern
#'
#' @return A function with signature:
#' \preformatted{
#'   function(x,
#'            return_detail = FALSE,
#'            extra_true = NULL,
#'            extra_false = NULL)
#' }
#'
#' Arguments of the returned function:
#'   * `x`: character vector to be searched.
#'   * `return_detail`: logical. If `FALSE` (default), returns a logical vector.
#' If `TRUE`, returns a data frame containing the input text, the logical classification, and the first TRUE or FALSE
#' pattern that matched.
#'   * `extra_true`, `extra_false`: optional character vectors of additional regex patterns to augment the default lists.
#'   * `bias`  determine what result should be used when the true_pattern and the false_pattern both match.  THe default
#'   is to use the true _pattern result.
#'   * `default_class` is what the classification should be is nother pattern types match.  The defualt is NA, but can 
#'   set to TRUE or FALSE.
#'
#' @seealso
#'   * [classifier_patterns()] to retrieve the TRUE and FALSE pattern lists used by a classifier created with 
#'   `make_classifier()`.
#'
#' @export
make_classifier <- function(true_patterns, false_patterns, ignore_case = TRUE) {
  # --- Internal helper kept private to this factory
  sanitize_patterns <- function(pats) {
    pats <- unlist(pats, use.names = TRUE)          # keep names here
    pats <- pats[!is.na(pats)]
    pats <- trimws(pats)
    pats <- pats[nzchar(pats)]
    pats <- pats[!duplicated(pats)]                 # dedupe while preserving names
    pats
  }
  
  # Sanitize inputs once
  true <- normalize_pattern_names(sanitize_patterns(true_patterns))
  false <- normalize_pattern_names(sanitize_patterns(false_patterns))
  
  # Build non-capturing alternations for fast boolean detect
  make_alt <- function(pats) {
    if (length(pats) == 0) return(NULL)
    paste0("(?:", pats, ")", collapse = "|")
  }
  
  # Resolve display labels for pattern reporting in the detail path.
  pattern_labels <- function(pats) {
    nm <- names(pats)
    if (is.null(nm) || any(!nzchar(nm))) {
      paste0("p", seq_along(pats))
    } else {
      nm
    }
  }
  
  compile_pattern_bundle <- function(pats) {
    list(
      patterns = pats,
      re = make_alt(pats),
      group_names = pattern_labels(pats)
    )
  }
  
  true_bundle  <- compile_pattern_bundle(true)
  false_bundle <- compile_pattern_bundle(false)
  true <- true_bundle$patterns
  false <- false_bundle$patterns
  true_re <- true_bundle$re
  false_re <- false_bundle$re
  true_group_names <- true_bundle$group_names
  false_group_names <- false_bundle$group_names
  
  # Return the first matching pattern label in vector order.
  first_matching_pattern <- function(values, patterns, labels) {
    out <- rep_len(NA_character_, length(values))
    if (!length(patterns) || !length(values)) return(out)
    
    remaining <- rep_len(TRUE, length(values))
    
    for (i in seq_along(patterns)) {
      if (!any(remaining)) break
      
      hit <- stringi::stri_detect_regex(
        values[remaining],
        patterns[[i]],
        opts_regex = list(case_insensitive = ignore_case)
      )
      
      if (any(hit)) {
        idx <- which(remaining)[hit]
        out[idx] <- labels[[i]]
        remaining[idx] <- FALSE
      }
    }
    
    out
  }
  
  # Return the classifier function that closes over precompiled patterns
  function(x, return_detail = FALSE, 
           extra_true       = NULL, 
           extra_false      = NULL, 
           bias             = c("true_pattern", "false_pattern"),
           default_class    = c("NA", "TRUE", "FALSE")) {
    stopifnot(is.character(x))
    
    bias          <- match.arg(arg = bias, choices = c("true_pattern", "false_pattern"))
    default_class <- match.arg(arg = default_class)
    default_class <- switch(default_class,
                            "NA"    = NA,
                            "TRUE"  = TRUE,
                            "FALSE" = FALSE)
    
    
    # Allow runtime extras; sanitize them
    extra_true  <- normalize_pattern_names(sanitize_patterns(extra_true))
    extra_false <- normalize_pattern_names(sanitize_patterns(extra_false))
    
    # If extras are present, rebuild alternations (only then)
    local_true_bundle <- if (length(extra_true)) {
      compile_pattern_bundle(normalize_pattern_names(sanitize_patterns(c(true, extra_true))))
    } else {
      true_bundle
    }
    local_false_bundle <- if (length(extra_false)) {
      compile_pattern_bundle(normalize_pattern_names(sanitize_patterns(c(false, extra_false))))
    } else {
      false_bundle
    }
    local_true <- local_true_bundle$patterns
    local_false <- local_false_bundle$patterns
    
    # Local alternations (boolean)
    local_true_re  <- local_true_bundle$re
    local_false_re <- local_false_bundle$re
    
    # Boolean detection (single pass per side)
    has_true <- if (!is.null(local_true_re)) {
      stringi::stri_detect_regex(x, local_true_re,  opts_regex = list(case_insensitive = ignore_case))
    } else rep_len(FALSE, length(x))
    
    has_false <- if (!is.null(local_false_re)) {
      stringi::stri_detect_regex(x, local_false_re, opts_regex = list(case_insensitive = ignore_case))
    } else rep_len(FALSE, length(x))
    
    # Final classification
    status <- rep_len(default_class, length(x))
    status[has_true & !has_false] <- TRUE
    status[!has_true & has_false] <- FALSE
    status[has_true & has_false]  <- if(bias == "true_pattern") TRUE else FALSE
    
    # Fast path
    if (!return_detail) return(status)
    
    # Group names to use in output (precomputed unless extras present)
    local_true_names  <- local_true_bundle$group_names
    local_false_names <- local_false_bundle$group_names
    
    matched_true_pattern  <- rep_len(NA_character_, length(x))
    matched_false_pattern <- rep_len(NA_character_, length(x))
    
    if (any(has_true)) {
      matched_true_pattern[has_true] <- first_matching_pattern(
        x[has_true],
        local_true,
        local_true_names
      )
    }
    
    if (any(has_false)) {
      matched_false_pattern[has_false] <- first_matching_pattern(
        x[has_false],
        local_false,
        local_false_names
      )
    }
    
    data.frame(
      text                  = x,
      classification        = status,
      matched_true_pattern  = matched_true_pattern,
      matched_false_pattern = matched_false_pattern,
      stringsAsFactors      = FALSE
    )
  }
}

#' Normalize Pattern Names
#'
#' Clean pattern names into stable labels used in detailed classifier output. Empty names are replaced with sequential
#' labels such as `p1`, `p2`, and invalid names are normalized to lowercase underscore-separated identifiers. If
#' normalization changes any names, a warning is emitted showing the original and updated values. Duplicate names after
#' normalization are rejected with an error.
#'
#' @param pats A named character vector of regex patterns.
#'
#' @return A character vector with normalized names.
#'
#' @keywords internal
normalize_pattern_names <- function(pats) {
  if (!length(pats)) return(pats)
  
  nm <- names(pats)
  if (is.null(nm)) {
    names(pats) <- paste0("p", seq_along(pats))
    return(pats)
  }
  
  original_nm <- nm
  blank <- is.na(nm) | !nzchar(trimws(nm))
  
  cleaned_nm <- tolower(trimws(nm))
  cleaned_nm[blank] <- paste0("p", which(blank))
  cleaned_nm <- gsub("[^a-z0-9]+", "_", cleaned_nm)
  cleaned_nm <- gsub("_+", "_", cleaned_nm)
  cleaned_nm <- gsub("^_+|_+$", "", cleaned_nm)
  cleaned_nm[!nzchar(cleaned_nm)] <- paste0("p", which(!nzchar(cleaned_nm)))
  needs_prefix <- grepl("^[^a-z]", cleaned_nm)
  cleaned_nm[needs_prefix] <- paste0("p_", cleaned_nm[needs_prefix])
  
  changed <- original_nm != cleaned_nm
  changed[is.na(changed)] <- TRUE
  
  if (any(changed)) {
    changes <- paste0("\"", original_nm[changed], "\" -> \"", cleaned_nm[changed], "\"")
    warning(
      "Pattern names were normalized: ",
      paste(changes, collapse = ", "),
      call. = FALSE
    )
  }
  
  dupes <- unique(cleaned_nm[duplicated(cleaned_nm)])
  if (length(dupes)) {
    stop(
      "Pattern names must be unique after normalization. Duplicates: ",
      paste(sprintf("\"%s\"", dupes), collapse = ", "),
      ". Rename the duplicated entries explicitly.",
      call. = FALSE
    )
  }
  
  names(pats) <- cleaned_nm
  pats
}


#' Retrieve or modify TRUE/FALSE pattern lists for a classifier
#'
#' `classifier_patterns()` retrieves the pattern vectors captured inside a classifier created with `make_classifier()`.
#' These are the base TRUE and FALSE regular-expression vectors that the classifier uses for text matching.
#'
#' The function returns the base TRUE and FALSE pattern vectors exactly as stored in the classifier's closure, with
#' names preserved.
#'
#' The pattern lists may also be *modified* at runtime using the replacement function:
#'
#' ```r
#' classifier_patterns(clf) <- list(
#'   true_patterns  = c("yes", "affirm"),
#'   false_patterns = c("no",  "negate")
#' )
#' ```
#'
#' You may instead use `true` and `false` as list names, and may update only one side at a time:
#'
#' ```r
#' classifier_patterns(clf) <- list(true = c("yep", "sure"))
#' ```
#'
#' Internally, the replacement function sanitizes the provided patterns and rebuilds the precompiled regex alternations
#' and group names so the classifier immediately uses the updated pattern sets.
#'
#' @md
#'
#' @param f A classifier function returned by `make_classifier()`.
#'
#' @return
#' `list(true = <character vector>, false = <character vector>)`
#'
#' When used in replacement form, invisibly returns the modified classifier.
#'
#' @seealso
#' * [`make_classifier()`] — create classifier functions
#' * Replacement method: [`classifier_patterns<-()`]
#'
#' @examples
#' clf <- make_classifier(
#'   true_patterns  = c(foo = "yes", bar = "affirm"),
#'   false_patterns = c(baz = "no",  qux = "negate")
#' )
#'
#' # Retrieve base patterns
#' classifier_patterns(clf)
#'
#' # Set both TRUE and FALSE patterns
#' classifier_patterns(clf) <- list(
#'   true_patterns  = c("yep", "sure"),
#'   false_patterns = c("nah", "nope")
#' )
#'
#' # Set only TRUE patterns
#' classifier_patterns(clf) <- list(true = c("yes", "affirm"))
#'
#' @export
classifier_patterns <- function(f) {
  stopifnot(is.function(f))
  
  env <- environment(f)
  
  # Pull the base patterns from the closure
  if (!exists("true", envir = env, inherits = TRUE) ||
      !exists("false", envir = env, inherits = TRUE)) {
    stop("Not a classifier created by make_classifier().")
  }
  
  list(
    true = get("true", envir = env, inherits = TRUE),
    false = get("false", envir = env, inherits = TRUE)
  )
}

#' @rdname classifier_patterns
#'
`classifier_patterns<-` <- function(f, value) {
  stopifnot(is.function(f))
  if (!is.list(value)) stop("`value` must be a list of pattern vectors.")
  
  env <- environment(f)
  if (is.null(env)) stop("Supplied function has no environment; not a make_classifier() closure.")
  
  # Ensure the closure has the expected captured objects/helpers
  needed <- c("true", "false", "sanitize_patterns", "compile_pattern_bundle", "normalize_pattern_names")
  missing <- needed[!vapply(needed, exists, logical(1), envir = env, inherits = TRUE)]
  if (length(missing)) {
    stop("Classifier closure is missing: ", paste(missing, collapse = ", "),
         ". Ensure `f` was created by make_classifier().")
  }
  
  sanitizer               <- get("sanitize_patterns",       envir = env, inherits = TRUE)
  compile_pattern_bundle  <- get("compile_pattern_bundle",  envir = env, inherits = TRUE)
  normalize_pattern_names <- get("normalize_pattern_names", envir = env, inherits = TRUE)
  
  current_true  <- get("true",  envir = env, inherits = TRUE)
  current_false <- get("false", envir = env, inherits = TRUE)
  
  # Accept either true/false or true_patterns/false_patterns
  val_true  <- value[["true_patterns"]]
  if (is.null(val_true))  val_true  <- value[["true"]]
  val_false <- value[["false_patterns"]]
  if (is.null(val_false)) val_false <- value[["false"]]
  
  # Compute new base vectors: replace provided sides, leave others unchanged
  new_true  <- if (is.null(val_true))  current_true  else normalize_pattern_names(sanitizer(val_true))
  new_false <- if (is.null(val_false)) current_false else normalize_pattern_names(sanitizer(val_false))
  
  # Assign updated bases
  assign("true",  new_true,  envir = env)
  assign("false", new_false, envir = env)
  
  true_bundle  <- compile_pattern_bundle(new_true)
  false_bundle <- compile_pattern_bundle(new_false)
  
  assign("true_re",            true_bundle$re,          envir = env)
  assign("false_re",           false_bundle$re,         envir = env)
  assign("true_group_names",   true_bundle$group_names, envir = env)
  assign("false_group_names",  false_bundle$group_names,envir = env)
  assign("true_bundle",        true_bundle,             envir = env)
  assign("false_bundle",       false_bundle,            envir = env)
  
  invisible(f)
}

# Interview text patterns -----------------------------------------------------------------------------------------
interview_true <- list(
  # e.g., "interview completed", "completed the interview", "investigation conducted"
  completions = r"(\b(?:(?:interview|investigation)\s+(?:completed|done|conducted|accomplished)|(?:completed|done|conducted|accomplished)\s+(?:the\s+)?(?:interview|investigation))\b)",
  
  # e.g., "conducted the interview", "completed investigation", "did interview"
  conducted   = r"(\b(?:conducted|completed|did)\s+(?:the\s+)?(?:investigation|interview)\b)",
  
  # e.g., "spoke with/to patient/mother/guardian..."
  # fixed typos earlier; kept your expanded relatives/caregiver; optional "the"
  spoke       = r"(\b(spoke|interviewed)\s+(?:with|to\s+)?(?:the\s+)?(?:case|pt\.?|patient|mother|mom|father|dad|parent(?:s)?|grand(?:mother|father|parent)(?:s)?|sibling(?:s)?|aunt|uncle|proxy|guardian|caregiver)\b)",
  
  # e.g., "per patient", "per mother", "per guardian"
  per_person  = r"(\bper\s+(?:the\s+)?(?:pt\.?|patient|mother|mom|father|dad|parent(?:s)?|grand(?:mother|father|parent)(?:s)?|guardian|proxy|caregiver)\b)",
  
  # e.g., "mother of the case", "guardian of case"
  relative    = r"(\b(?:mother|mom|father|dad|aunt|uncle|grand(?:mother|father|parent)|guardian)\s+of\s+(?:the\s+)?case\b)",
  
  # e.g., "patient reports/states/said ..."
  reports     = r"(\b(?:pt\.?|patient|case|mother|mom|father|dad|guardian)\s+(?:reports?|states?|says?|said)\b)",
  
  # e.g., "symptoms lasted 3 days"
  misc        = r"(\bsymptoms\s+lasted\b)",
  
  # e.g., "patient called back", "mother returned call", "dad called"
  call_back   = r"(\b(?:pt\.?|patient|mother|mom|father|dad|grand(?:mother|father|parent))\s+(?:called(?:\s+back)?|returned\s+call)\b)"
)

interview_false <- list(
  # admin/system sources
  nedss      = r"(\b(?:NEDSS|NBS|External\s+source)\b)",
  
  # records requested
  request    = "(\\brecords (request(ed)?|sent)\\b) | (\\brequest(ed)? records\\b)",
  
  # medical staff references
  medical    = r"(\b(talked|spoke)\s+(?:with\s)?(infection preventionist|ip|nurse|dr|doctor))",
  
  # medical assistant references
  med_aid    = r"(\b(?:MA|medical\s+assistant)\b)",
  
  # faxing, labs, ROI, routing
  fax_lab    = r"(\b(?:fax(?:ed|ing)?|labs?|roi|sent\s+to)\b)",
  
  # record/chart reviews
  records    = r"(\b(?:record\s+review(?:\s+only)?|reviewed\s+records|abstract(?:ed|ing|ion)?|chart\b))",
  
  # mail
  letter     = r"(\bform\s+letter\b)",
  
  # status/administrative markers
  status     = r"(\b(?:NAC|not\s+a\s+case|case\s+criteria|data\s+entry|dup(?:e|licate)|OOJ|OOC|LTF)\b)",
  
  # spoke with staff/clinicians (not the case/proxy)
  staff      = r"(\bspoke\s+(?:with|to)\s+(?:the\s+)?(?:staff|dr\b|doctor|nurse|director|principal|ems|hospital\s+staff|clinic\s+staff)\b)",
  
  # voicemail / LVM entries
  lvm        = r"(\b(?:LVM(?:\s*x\s*\d+)?|left\s+voicemail|left\s+voice\s*mail)\b(?:\s+(?:with|for)\s+(?:nurse|doctor|director|office))?)",
  
  # Make it context-specific so we don't match unrelated "refused".
  refusal     = r"(\b(?:refus(?:ed|al)|declined)\s+(?:the\s+)?(?:interview|questions)\b)",
  
  # already raw in your input (kept)
  callback   = r"(\brequested (CB|call ?back)\b)",
  ltf        = r"(\b(?:as/s)?ltf ?(?:letter)?\b)",
  imm        = r"(Outside Immunization)",
  case_status= r"(\b(probable|confirmed|not a case|NAC|OOC|LTF)\b)"
)

# case_interview_classifier ----
#' Determine Case was Interviewed
#' 
#' When using case_interview_classifier() pass text as a character vector, each entry is checked for indication that the
#' case or proxy was interviewed. The function is very useful when processing encounters and notes from EpiTrax where
#' investigators type a variety of responses. Those responses can be passed to this function and it returns TRUE (case interviewd)
#'  or FALSE (case interview did not happen).
#' 
#' @details
#' **Patterns used by `case_interview_classifier()` to determine interview status.**
#'
#' The classifier evaluates notes against two groups of rules:
#' **TRUE** (evidence that case/patient/proxy was interviewed or provided information) and
#' **FALSE** (administrative or non-interview activity such as LVM, chart review, staff-only communication).
#'
#' ### Summary (human-readable rule intents)
#'
#' | TRUE (Interview signals)                                                                 | FALSE (Non-interview / admin signals)                                                         |
#' | :---------------------------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------- |
#' | Interview or investigation marked **completed/done/conducted/accomplished**              | System/admin sources (e.g., **NEDSS**, **NBS**, **External source**)                          |
#' | **Conducted/completed/did** the interview or investigation                               | **Records request/sent**                                                                       |
#' | **Spoke**/**interviewed** with/to **patient/case/proxy/relative/caregiver**              | Spoke/talked with **IP/nurse/doctor** (staff-only exchanges)                                   |
#' | Notes phrased as **“per patient/mother/father/guardian”**                                 | **MA / medical assistant** references                                                          |
#' | **Mother/father/aunt/uncle/grandparent/guardian of the case**                             | **Faxed/faxing**, **labs**, **ROI**, **sent to**                                               |
#' | Patient/case/parent/guardian **reports/states/says/said**                                 | **Record/chart review**, abstracting, reviewed records                                         |
#' | **Refused/declined interview/questions** (context-specific interview outcome)             | **Form letter**                                                                                |
#' | **“Symptoms lasted”** phrasing                                                            | Status markers: **NAC**, **not a case**, **case criteria**, **data entry**, **dupe**, **OOJ/OOC/LTF** |
#' | Patient/parent **called / called back / returned call**                                   | **Spoke with staff/dr/doctor/nurse/director/principal/EMS/hospital staff/clinic staff**        |
#' |                                                                                           | **LVM / left voicemail** (optionally “x N”, “with/for nurse/doctor/director/office”)           |
#' |                                                                                           | **Requested CB / call back**                                                                   |
#' |                                                                                           | **as/s LTF letter**                                                                            |
#' |                                                                                           | **Outside Immunization**                                                                       |
#' |                                                                                           | Case status keywords: **probable/confirmed/not a case/NAC/OOC/LTF**                            |
#'
#'
#' **Notes**
#' - Matching is performed ignoring case as a default.
#'
#' 
#' @param x is the character vector
#' @param return_detail FALSE is the default and causes a logical vector to be returned.  When set to TRUE,
#'  a four column data frame is returned with the text, status (TRUE/FALSE), matched_true_pattern, matched_false_pattern.
#' @param extra_true,extra_false a character vector with additional true or false patterns 
#' @rdname make_classifier
#' @returns For case_interview_classifier() by default a logical vector, TRUE when matched to a true_pattern, and FALSE
#'   when a false_pattern is matched.
#' @export
case_interview_classifier <- make_classifier(
  true_patterns  = interview_true,
  false_patterns = interview_false
)
