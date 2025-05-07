#' Search a dataframe for variable names matching a pattern.
#'
#' @param pattern character string in regular expression format.
#' @param df dataframe object, if left as NULL it assumes the dataframe is labeled "df".
#' @param exclude character string in regular expression format identifying variable names to exclude. Defaults to NULL.
#' @param type character vector of variable types to include. Defaults to all types. Valid types are: "numeric", "factor", "character", "double", "logical", "integer".
#' @param ignore.case logical indicating whether to ignore case when matching the pattern. Defaults to TRUE.
#' @return character vector of variable names found within selected dataframe.
#' @seealso [grepl()]
#' @export
#' @examples
#' varlist("hsns", exclude="5|oj")
#' # identifies all variable names that include 'hsns' but excludes those that have a 5 or "oj".
#' # Pipe key "|" is the "or" operator; other operators (i.e,. "&") can also be used.
#'
#' varlist(type="factor",exclude=c("gb|^c"))
#' # all factor variable except variable gb and variables starting with c.
#'
#' varlist(pattern="neo[1-9]", df = data)
#' # all variables in dataframe labeled "data" that start with neo followed by any number (e.g., 'neo1','neo2', etc.).
#'
#' sapply(df[,varlist(type="numeric",pattern="credit")], summary)
#' # can use in conjunction with sapply.
varlist <- function (pattern = NULL, df = NULL, exclude = NULL,
                     type = c("numeric", "factor", "character", "double", "logical", "integer"),
                     ignore.case = TRUE) {

  # --- Input Validation ---
  if (is.null(pattern)) {
    stop("Argument 'pattern' must be provided (regular expression string).")
  }
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("Argument 'pattern' must be a single character string (regex).")
  }
  if (!is.null(exclude) && (!is.character(exclude) || length(exclude) != 1) ) {
    stop("Argument 'exclude' must be NULL or a single character string (regex).")
  }
  if (!is.logical(ignore.case) || length(ignore.case) != 1) {
    stop("Argument 'ignore.case' must be TRUE or FALSE.")
  }
  # Allow type to be NULL or missing, meaning default to all types
  if (!missing(type) && !is.null(type) && !is.character(type)) {
    stop("Argument 'type' must be a character vector of valid types or NULL.")
  }


  # --- Dataframe Handling ---
  # If df argument is NULL, try to get 'df' from the calling environment
  if (is.null(df)) {
    if (exists("df", envir = parent.frame())) {
      df <- get("df", envir = parent.frame())
      # Optional: Inform the user that the default 'df' is being used
      # message("Using dataframe 'df' from the calling environment.")
    } else {
      stop("Argument 'df' was not provided and no object named 'df' was found in the calling environment.")
    }
  }

  # Check if the result is actually a dataframe
  if (!is.data.frame(df)) {
    stop("The object assigned to 'df' is not a data frame.")
  }

  # --- Variable Type Filtering ---
  # Get all variable names first
  all_vars <- names(df)
  if (length(all_vars) == 0) {
    return(character(0)) # Handle empty dataframe
  }

  # Filter by type
  vars_filtered_by_type <- character(0)
  valid_types_provided <- FALSE

  # Define the base R type checking functions
  type_checks <- list(
    numeric = is.numeric,   # Includes integer and double
    double = is.double,
    integer = is.integer,
    factor = is.factor,
    character = is.character,
    logical = is.logical
  )

  # Determine which types to actually check based on user input
  # Use intersect to only consider valid types specified in the function definition
  types_to_check <- intersect(tolower(type), names(type_checks))

  if (length(types_to_check) > 0) {
    valid_types_provided <- TRUE
    # Apply the checks for the selected types
    # Note: Using `is.numeric` will catch both integer and double if 'numeric' is in types_to_check
    selected_vars_list <- list()
    for (t in types_to_check) {
      # Apply the corresponding check function to each column
      matches <- sapply(df, type_checks[[t]])
      selected_vars_list[[t]] <- all_vars[matches]
    }
    # Combine results from different type checks and remove duplicates
    vars_filtered_by_type <- unique(unlist(selected_vars_list, use.names = FALSE))

  } else {
    # If 'type' argument was missing, NULL, or empty after validation
    # Default behavior: consider all variables regardless of type
    vars_filtered_by_type <- all_vars
    # Or if type was specified but none were valid after intersect():
    if (!missing(type) && !is.null(type)) {
      warning("None of the specified 'type' values were valid. Considering all variable types. Valid types are: ",
              paste(names(type_checks), collapse=", "))
    }
  }

  # If after type filtering (if any types were specified) we have no variables, return early
  if (valid_types_provided && length(vars_filtered_by_type) == 0) {
    # message("No variables found matching the specified types.") # Optional message
    return(character(0))
  }


  # --- Pattern Matching and Exclusion ---
  # Filter by include pattern
  if (!is.null(pattern)) {
    vars_matched <- vars_filtered_by_type[grepl(pattern, vars_filtered_by_type, ignore.case = ignore.case)]
  } else {
    # Should not happen due to initial check, but for robustness:
    vars_matched <- vars_filtered_by_type
  }


  # Filter by exclude pattern
  if (!is.null(exclude)) {
    # Ensure exclude pattern doesn't remove *all* matches if exclude is too broad
    vars_to_exclude <- grepl(exclude, vars_matched, ignore.case = ignore.case)
    final_list <- vars_matched[!vars_to_exclude]
  } else {
    final_list <- vars_matched
  }

  return(final_list)
}
