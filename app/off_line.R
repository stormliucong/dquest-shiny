### load config ###
source(file = './config.R')
### end ###
CACHE$CONDITION_KEY_WORD_TOP100[1:10]

conditionId = getTrialsBySearch(con = CON,condition = c("Alzheimer's Disease"), age = -1,gender = NULL,country = NULL,state = NULL,status = NULL,phase = NULL)
knowledgeBase_small %>% as_tibble() %>% filter(nct_id %in% conditionId) %>% group_by(common_omop_name,domain) %>% summarize(n_nct = length(unique(nct_id))) %>% arrange(-n_nct)
