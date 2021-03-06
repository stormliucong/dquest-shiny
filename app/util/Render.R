renderTrialInfo = function(trialSet, trialDt, session, number = 999999) {
  output = NULL
  if(dim(trialDt)[1] > 0){
    data = data.table(trialDt)
    data = data[nct_id %in% trialSet] %>% head(n = number)
    data[, nct_id := paste0(
      '<a target="_blank" href="https://clinicaltrials.gov/ct2/show/',
      nct_id,
      '">',
      nct_id,
      '</a>'
    )]
    
    # shuffle the rows.
    data = data[, .SD[sample(.N)]]
    # dt = DT::datatable(data = data,options = list(
    #   scrollY = 200,
    #   scroller = TRUE,
    #   escape = FALSE
    # ))
    # ouput = DT::renderDT({
    #   data
    # })
    
    # a bug: DT::datatable does not work to embed html.
    data$phase = as.factor(data$phase)
    data$status = as.factor(data$last_known_status)
    data$study_type = as.factor(data$study_type)
    data = data[, c('nct_id', 'brief_title', 'phase', 'last_known_status','study_type')]
    output = DT::renderDataTable(
      data,
      escape = FALSE,
      server = TRUE,
      filter = 'top',
      rownames = F,
      options = list(scrollX = TRUE)
    )
  }
  return(output)
}

renderQuestion = function(concept, wMatrix, session) {
  domain = wMatrix %>% filter(common_omop_id == concept) %>% pull(domain) %>% unique()
  concept_name = wMatrix %>% filter(common_omop_id == concept) %>% pull(common_omop_name) %>% unique()
  # major_value_unit = wMatrix %>% filter(common_omop_id == concept) %>% group_by(value_unit) %>% summarise(count = n()) %>% arrange(-count) %>% head(n = 1) %>% pull(value_unit)
  renderQuestionDomain(concept_name = concept_name,domain = domain)
  insertButtons(session)
  return(NULL)
}

renderQuestionDomain = function(concept_name, domain) {
  
  switch(
    domain,
    "condition" = renderQuestionCondition(concept_name),
    "observation" = renderQuestionObservation(concept_name),
    "measurement" = renderQuestionMeasurement(concept_name),
    "procedure" = renderQuestionProcedure(concept_name),
    "drug" = renderQuestionDrug(concept_name)
  )
  return(NULL)
}

renderQuestionCondition = function(concept_name) {
  label1 = paste0("Have you had ", concept_name, "?")
  choice1 = c("Yes", "No", "I don't know")
  insertRatioButtons(label1, choice1)
  label2 = paste0("When did you have this condition ?")
  insertTimeRangeInput(label2)
  return(NULL)
}

renderQuestionProcedure = function(concept_name) {
  label1 = paste0("Have you had ", concept_name, "?")
  choice1 = c("Yes", "No", "I don't know")
  insertRatioButtons(label1, choice1)
  label2 = paste0("When did you have this procedure ?")
  insertTimeRangeInput(label2)
  return(NULL)
}

renderQuestionMeasurement = function(concept_name, major_value_unit = "mvu") {
  if (!is.na(major_value_unit)) {
    label1 = paste0("What is your latest ",
                    concept_name,
                    " (",
                    major_value_unit,
                    ") ?")
  } else{
    label1 = paste0("What is your latest", concept_name, " ?")
  }
  insertNumericInput(label1)
  return(NULL)
}

renderQuestionDrug = function(concept_name) {
  label1 = paste0("Have you taken ", concept_name, "?")
  choice1 = c("Yes", "No", "I don't know")
  insertRatioButtons(label1, choice1)
  label2 = paste0("When did you take this drug ?")
  insertTimeRangeInput(label2)
  return(NULL)
}

renderQuestionObservation = function(concept_name) {
  label1 = paste0("Have you had ", concept_name, "?")
  choice1 = c("Yes", "No", "I don't know")
  insertRatioButtons(label1, choice1)
  label2 = paste0("When did you have this observation ?")
  insertTimeRangeInput(label2)
  return(NULL)
}
