getApi = function(dbname, host, port, user, password) {
  drv = dbDriver('PostgreSQL')
  con = dbConnect(
    drv = drv,
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  )
  return(con)
}

getCache = function(con) {
  aact_condition = dbGetQuery(con,
                              "SELECT name,COUNT(*) AS count FROM conditions GROUP BY name ORDER BY count DESC")
  condition_keyword = aact_condition$name[1:100]
  
  facilitity_search = dbGetQuery(con, "SELECT DISTINCT country, state FROM facilities")
  status_search = dbGetQuery(con,"SELECT DISTINCT last_known_status FROM studies")
  phase_search = dbGetQuery(con,"SELECT DISTINCT phase FROM studies")
  
  
  return(
    list(
      CONDITION_KEY_WORD_TOP100 = condition_keyword,
      COUNTRY_STATE_TABLE = data.table(facilitity_search),
      STATUS_TABLE = data.table(status_search),
      PHASE_TABLE = data.table(phase_search)
    )
  )
}

