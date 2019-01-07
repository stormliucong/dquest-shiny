observeEvent(input$search, {
  withBusyIndicatorServer("search",
                          if (is.null(input$condition)) {
                            # condition is required.
                            showNotification(paste("Condition is required for search"), duration = 0)
                          } else{
                            # get nct_id.
                            cat("searching ...\n")
                            react$trialSet_tmp = getTrialsBySearch(
                              con = react$MY_CON,
                              condition = input$condition,
                              age = input$age,
                              gender = input$gender,
                              country = input$country,
                              state = input$state,
                              ctrl = input$ctrl,
                              status = input$status,
                              phase = input$phase
                            )
                            # update search result.
                            react$wMatrix_tmp = react$wMatrix[nct_id %in% react$trialSet_tmp]
                            cat("getting trials info ...\n")
                            TRIAL_INFO = getTrialsInfoById(con = react$MY_CON,nct_id_list = react$trialSet_tmp)
                            cat("dim of TRIAL_INFO", dim(TRIAL_INFO),"\n")
                            # render trial table 
                            cat("rendering pages ...\n")
                            output$trial_info = renderTrialInfo(react$trialSet_tmp, TRIAL_INFO, session)
                            # go to the trial tab when clicking the button
                            updateTabsetPanel(session, inputId = "navbar", selected = "trials")
                          })
  
  
})