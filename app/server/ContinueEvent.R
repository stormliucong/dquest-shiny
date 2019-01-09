observeEvent(input$continue, {
  req(react$trialSet_tmp)
  react$trialSet = react$trialSet_tmp
  react$asked_concept_id = c(react$asked_concept_id, react$common_concept_id)
  wMatrix = getWMatrix(react$trialSet,react$asked_concept_id)
  if (dim(wMatrix)[1] == 0 |
      length(react$trialSet) == 0) {
    showNotification("All trials have been filtered out or all trial criteria has been asked")
  } else{
    # confirm update and remove common concept
    # react$wMatrix = removeConceptId(react$wMatrix_tmp,react$common_concept_id)
    # optimizing.
    react$common_concept_id = findConcept(wMatrix = wMatrix,
                                          asked_concept_id = react$asked_concept_id)
    #refresh question form.
    refreshQA(session)
    # render question.
    # cat('common concept id\n')
    # cat(react$common_concept_id)
    renderQuestion(react$common_concept_id, wMatrix, session)
    
    # update count.
    if (length(input$skip) > 0) {
      if (input$skip == FALSE) {
        counter$countervalue <- counter$countervalue + 1
      }
    }
  }
})