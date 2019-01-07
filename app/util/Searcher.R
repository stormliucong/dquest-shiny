getTrialsBySearch = function(con,
                             condition = c('breast cancer'),
                             age = 60,
                             gender = 'Female',
                             country = 'United States',
                             state = NULL,
                             status = c('Recruiting'),
                             phase = NULL,
                             ctrl = FALSE) {
  ### init trials ###
  final_trials = NULL
  condition_trials = NULL
  gender_trials = NULL
  age_trials = NULL
  country_trials = NULL
  state_trials = NULL
  ctrl_trials = NULL
  status_trials = NULL
  phase_trials = NULL
  
  ### get condition defined trials ###
  # condition = c('breast cancer', 'liver cancer')
  if (!is.null(condition)) {
    condition = tolower(condition)
    condition = paste0("'", condition, "'")
    condition = paste0(condition, collapse = ",")
    condition = paste0("(", condition, ")")
    condition_trials = dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT nct_id FROM conditions WHERE downcase_name IN ",
        condition,
        ""
      )
    )
    cat(paste0('get trials defined by conditions : ' , length(unique(condition_trials$nct_id))))
  }
  
  ### end ###
  
  ### get gender defined trials ###
  # gender = "male"
  if (!is.null(gender)) {
    if (tolower(gender) == 'all') {
      gender = paste0("('", gender, "','')")
    } else{
      if (tolower(gender) == 'male') {
        gender = paste0("('", gender, "','All','')")
      } else{
        gender = paste0("('", gender, "','All','')")
      }
    }
    gender_trials = dbGetQuery(con,
                               paste0(
                                 "SELECT DISTINCT nct_id FROM eligibilities WHERE gender IN ",
                                 gender,
                                 ""
                               ))
    cat(paste0('get trials defined by gender : ' , length(unique(gender_trials[,1]))))
    
  }
  ### end ###
  
  ### get age defined trials ###
  # age = 1
  if (!is.null(age)) {
    all_age = dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT nct_id, minimum_age, maximum_age FROM eligibilities"
      )
    )
    all_age = data.table(all_age)
    # split table.
    all_age$minimum_age_as_number = as.character(lapply(strsplit(
      as.character(all_age$minimum_age), split = " "
    ), "[", 1))
    all_age$minimum_age_as_unit = as.character(lapply(strsplit(
      as.character(all_age$minimum_age), split = " "
    ), "[", 2))
    all_age$maximum_age_as_number = as.character(lapply(strsplit(
      as.character(all_age$maximum_age), split = " "
    ), "[", 1))
    all_age$maximum_age_as_unit = as.character(lapply(strsplit(
      as.character(all_age$maximum_age), split = " "
    ), "[", 2))
    
    age_nct = all_age[((
      minimum_age_as_number <= age &
        minimum_age_as_unit == 'Years'
    ) |
      (
        is.na(minimum_age_as_unit) |
          minimum_age_as_unit != 'Years'
      )
    ) &
      ((
        maximum_age_as_number >= age &
          maximum_age_as_unit == 'Years'
      ) | is.na(maximum_age_as_unit)
      ), nct_id]
    age_trials = data.table(nct_id = age_nct)
    cat(paste0('get trials defined by age : ' , length(unique(age_trials$nct_id))))
  }
  ### end ###
  
  
  ### get contry defined trials ###
  # country = c('United States', 'liver cancer')
  if (!is.null(country)) {
    country = paste0("'", country, "'")
    country = paste0(country, collapse = ",")
    country = paste0("(", country, ")")
    country_trials = dbGetQuery(con,
                                paste0(
                                  "SELECT DISTINCT nct_id FROM facilities WHERE country IN ",
                                  country,
                                  ""
                                ))
    cat(paste0('get trials defined by country : ' , length(unique(country_trials[,1]))))
  }
  
  ### end ###
  
  ### get state defined trials ###
  # state = c('Illinois', 'New York')
  if (!is.null(state)) {
    state = paste0("'", state, "'")
    state = paste0(state, collapse = ",")
    state = paste0("(", state, ")")
    sql_query = paste0(
      "SELECT DISTINCT nct_id FROM facilities WHERE state IN ",
      state, " AND country IN ",country, 
      ""
    )
    state_trials = dbGetQuery(con,
                              sql_query)
    
    cat(paste0('get trials defined by state : ' , length(unique(state_trials[,1]))))
    
  }
  ### end ###
  
  ### get health defined trials ###
  if (ctrl == TRUE) {
    ctrl = "('Accepts Healthy Volunteers','null')"
    sql_query = paste0(
      "SELECT DISTINCT nct_id FROM eligibilities WHERE healthy_volunteers IN ",
      ctrl,
      ""
    )
    cat(sql_query,"\n")
    ctrl_trials = dbGetQuery(
      con,
      sql_query
    )
    cat(paste0('get trials defined by ctrl : ' , length(unique(ctrl_trials$nct_id))))
  } else{
    ctrl_trials = NULL
  }
  
  ### end ###
  
  ### get status defined trials ###
  if (!is.null(status)) {
    status = paste0("'", status, "'")
    status = paste0(status, collapse = ",")
    status = paste0("(", status, ")")
    sql_query =  paste0(
      "SELECT DISTINCT nct_id FROM studies WHERE last_known_status IN ",
      status,
      ""
    )
    status_trials = dbGetQuery(
      con,
      sql_query
    )
    cat(paste0('get trials defined by status : ' , length(unique(status_trials$nct_id))))
    
  }
  ### end ###
  
  ### get phase defined trials ###
  if (!is.null(phase)) {
    phase = paste0("'", phase, "'")
    phase = paste0(phase, collapse = ",")
    phase = paste0("(", phase, ")")
    phase_trials = dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT nct_id FROM studies WHERE phase IN ",
        phase,
        ""
      )
    )
    cat(paste0('get trials defined by phase : ' , length(unique(phase_trials$nct_id))))
    
  }
  ### end ###
  
  ### get final trials ###
  var_list = list(
    condition_trials$nct_id,
    gender_trials$nct_id,
    age_trials$nct_id,
    country_trials$nct_id,
    state_trials$nct_id,
    ctrl_trials$nct_id,
    phase_trials$nct_id,
    status_trials$nct_id
  )
  
  union_trials = Reduce(
    union,
    var_list
  )
  cat(paste0("length of union trials", length(union_trials),"\n"))
  
  intersect_trials = union_trials
  
  for(single_trials in var_list){
    if(!is.null(single_trials)){
      intersect_trials = intersect(single_trials,intersect_trials)
      cat(paste0("length of intersect trials", length(intersect_trials),"\n"))
      
    }
  }
  
  cat(paste0("length of final trials", length(intersect_trials),"\n"))
  return(intersect_trials)
}

getTrialsInfoById = function(con,nct_id_list){
  nct_id_list
  if (!is.null(nct_id_list)) {
    nct_id = nct_id_list
    nct_id = paste0("'", nct_id, "'")
    nct_id = paste0(nct_id, collapse = ",")
    nct_id = paste0("(", nct_id, ")")
    sql_query =  paste0(
      "SELECT DISTINCT nct_id,brief_title,study_type,last_known_status,phase FROM studies
      WHERE nct_id IN ",
      nct_id,
      ""
    )
    cat(sql_query,"\n")
    trials_info = dbGetQuery(
      con,
      sql_query
    )
    trials_info
    return(trials_info)
  }
}

