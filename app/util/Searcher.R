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
    # condition = paste0("'", condition, "'")
    # condition = paste0(condition, collapse = ",")
    # condition = paste0("(", condition, ")")
    condition_trials = tbl(con,"conditions") %>%
      filter(downcase_name %in% condition) %>%
      select(nct_id) %>% distinct() %>%
      collect()
    # cat(paste0('get trials defined by conditions : ' , length(unique(condition_trials$nct_id)),"\n"))
  }
  
  ### end ###
  
  ### get gender defined trials ###
  # gender = "male"
  if (!is.null(gender)) {
    if(tolower(gender) == "male"){
      gender_trials = tbl(con,"eligibilities") %>% 
        filter(gender != 'Female') %>%
        select(nct_id) %>% distinct() %>%
        collect()
      # cat(paste0('get trials defined by gender : ' , length(unique(gender_trials$nct_id)),"\n"))
      
    }
    
    if(tolower(gender) == "female"){
      gender_trials = tbl(con,"eligibilities") %>% 
        filter(gender != 'Male') %>%
        select(nct_id) %>% distinct() %>%
        collect()
      # cat(paste0('get trials defined by gender : ' , length(unique(gender_trials$nct_id)),"\n"))
    }
  }
  ### end ###
  
  ### get age defined trials ###
  # age = 1
  if (!is.null(age) & age > 0) {
    age_tmp = tbl(con,"eligibilities") %>%
      select(nct_id,minimum_age,maximum_age) %>% as.data.frame() %>%
      separate(minimum_age,c("min","min_unit")," ") %>% 
      separate(maximum_age,c("max","max_unit")," ") %>% 
      as_tibble()
    age_tbl = age_tmp %>% filter(min =="N/A" & max == "N/A")
    age_tbl = age_tmp %>% filter(min == "N/A" & (as.integer(max) > age & max_unit == 'Years')) %>% bind_rows(age_tbl)
    age_tbl = age_tmp %>% filter(max == "N/A" & (as.integer(min) < age & min_unit == 'Years')) %>% bind_rows(age_tbl)
    age_trials = age_tbl %>% select(nct_id) %>% distinct()
    # cat(paste0('get trials defined by age : ' , length(unique(age_trials$nct_id)),"\n"))
  }
  ### end ###
  
  
  ### get contry defined trials ###
  # country = c('United States', 'liver cancer')
  country_ = country
  if (!is.null(country_)) {
    country_trials = tbl(con,"facilities") %>% filter(country %in% country_) %>% select(nct_id) %>% distinct() %>% collect()
    # cat(paste0('get trials defined by country : ' , length(unique(country_trials$nct_id)),"\n"))
  }
  
  ### end ###
  
  ### get state defined trials ###
  # state = c('Illinois', 'New York')
  state_ = state
  if (!is.null(state)) {
    state_trials = tbl(con,"facilities") %>% filter(state %in% state_) %>% select(nct_id) %>% distinct() %>% collect()
    # cat(paste0('get trials defined by state : ' , length(unique(state_trials[,1]))))
  }
  ### end ###
  
  ### get health defined trials ###
  if (ctrl == TRUE) {
    ctrl_trials = tbl(con,"eligibilities") %>% filter(healthy_volunteers != 'No') %>%
      select(nct_id) %>% distinct() %>% collect()
    # cat(paste0('get trials defined by ctrl : ' , length(unique(ctrl_trials$nct_id)),"\n"))
  }
  
  ### end ###
  
  ### get status defined trials ###
  if (!is.null(status)) {
    status_trials = tbl(con,"studies") %>%
      filter(is.na(last_known_status) | last_known_status %in% status) %>% 
      select(nct_id) %>% distinct() %>% collect()
    # cat(paste0('get trials defined by status : ' , length(unique(status_trials$nct_id)),"\n"))
    
  }
  ### end ###
  
  ### get phase defined trials ###
  phase_ = phase
  if (!is.null(phase)) {
    phase_trials = tbl(con,"studies") %>%
      filter(is.na(phase) | phase %in% phase_ | phase == 'N/A') %>% 
      select(nct_id) %>% distinct() %>% collect()
    # cat(paste0('get trials defined by phase : ' , length(unique(phase_trials$nct_id)),"\n"))
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
  # cat(paste0("length of union trials: ", length(union_trials),"\n"))
  
  intersect_trials = union_trials
  
  for(single_trials in var_list){
    if(!is.null(single_trials)){
      intersect_trials = intersect(single_trials,intersect_trials)
      # cat(paste0("length of intersect trials: ", length(intersect_trials),"\n"))
      
    }
  }
  
  # cat(paste0("length of final trials: ", length(intersect_trials),"\n"))
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
    trials_info = dbGetQuery(
      con,
      sql_query
    )
    trials_info
    return(trials_info)
  }
}

