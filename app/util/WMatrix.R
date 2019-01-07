getWMatrix = function(trialSet,asked_concept_id){
  wMatrix = CRITERIA_LIB %>% filter(nct_id %in% trialSet) %>%
    filter(!common_omop_id %in% asked_concept_id)
  return(wMatrix)
}
