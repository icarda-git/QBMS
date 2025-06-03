#' EXISTING USE CASES:
#'
#' 1. Do I have any engine restriction regarding this function call?
#'    trigger stop() call with a message for not supported call or parameter 
#'    e.g., only BMS supports filtering list_trials by year
#'
#' 2. Do I have to alter call_url before send it to this engine?
#'    e.g., in BreedBase if trial_db_id = program_db_id, then list_studies url
#'          need to replace trialDbId by programDbId
#'
#' 3. Do I have to alter the results object/dataframe before return it back?
#'    e.g., in BreedBase take the first row as a column header in the get_study_data
#'
#' INJECT ACTION POINTS:
#' 
#' a. get_brapi_url (brapi.R)
#'    check if the given func_name required any pre-processing action for current
#'    engine, if yes, then call engine_pre_process(call_url, engine) just before 
#'    send back default brapi_map value, instead return back the updated call_url
#'
#' b. brapi_get_call (http.R)
#'    check if the given func_name required any post-processing action for current
#'    engine, if yes, then call engine_post_process(result_data, engine) just
#'    before send back results list, and replace it by the updated version


engine_pre_process <- function(call_url, engine, func_name) {
  if (engine == "breedbase" & func_name == "list_studies") {

    # handle the case of BreedBase trials (studies) listed in the root program folder (trial)
    if (qbms_globals$state$trial_db_id == qbms_globals$state$program_db_id) {
      call_url <- sub("\\?trialDbId\\=", '?programDbId=', call_url)
    }
  }
  
  call_url
}

engine_post_process <- function(result_data, engine, func_name) {
  if (engine == "breedbase" & func_name == "get_study_data") {
    result_data$data <- result_data$data[-1, ]
  }

  if (engine == "breedbase" & func_name == "list_studies") {

    # handle the case of BreedBase trials (studies) listed in the root program folder (trial)
    if (qbms_globals$state$trial_db_id == qbms_globals$state$program_db_id) {
      result_data$data <- result_data$data[is.na(result_data$data$trialName), ]
      rownames(result_data$data) <- NULL
    }
  }
  
  result_data
}

