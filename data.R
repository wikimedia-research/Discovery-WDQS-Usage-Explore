# Run on stat2

start_date <- as.Date("2016-07-01")
# end_date <- Sys.Date()-1
end_date <- as.Date("2016-08-29")

# webrequest by country
webrequest_by_country <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
              SELECT CONCAT(year,'-',month,'-',day) AS dt, geocoded_data['country'] AS country, 
              COUNT(DISTINCT client_ip) AS n_client_ip,
		  	  COUNT(DISTINCT CONCAT(client_ip,user_agent)) AS n_user,
              COUNT(uri_query) AS n_query,
              PERCENTILE_APPROX(time_firstbyte, 0.5) AS median_time_firstbyte,
              PERCENTILE(response_size, 0.5) AS median_response_size,
              SUM(CASE WHEN agent_type = 'user' THEN 1 END) AS n_user_query
              FROM webrequest",
              clause_data$date_clause,
			  # "AND hour=8",
              "AND webrequest_source = 'misc'
              AND uri_host = 'query.wikidata.org'
              AND uri_path = '/bigdata/namespace/wdq/sparql'
              AND http_status IN('200','304')
              AND INSTR(uri_query, '?query=') > 0
              GROUP BY CONCAT(year,'-',month,'-',day), geocoded_data['country'];") 
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(webrequest_by_country, "webrequest_by_country.rds", "gz")
system("scp stat2:/home/chelsyx/webrequest_by_country.rds data/")


# user by country
user_by_country <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT dt, country, 
				 COUNT(DISTINCT user_id) AS n_user,
				 PERCENTILE(n_query, 0.5) AS median_n_query_per_user,
                 SUM(CASE WHEN agent_type = 'spider' THEN 1 END) AS n_spider
				 FROM
				 (SELECT CONCAT(year,'-',month,'-',day) AS dt, 
				 geocoded_data['country'] AS country, 
				 CONCAT(client_ip,user_agent) AS user_id,
                 COUNT(uri_query) AS n_query,
                 MAX(agent_type) AS agent_type
                 FROM webrequest",
                 clause_data$date_clause,
				 # "AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day), geocoded_data['country'], CONCAT(client_ip,user_agent)) AS agg_by_user
				 GROUP BY dt, country
				 ;")
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(user_by_country, "user_by_country.rds", "gz")
system("scp stat2:/home/chelsyx/user_by_country.rds data/")


# user by os
user_by_os <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT dt, os, agent_type,
				 COUNT(DISTINCT user_id) AS n_user,
				 SUM(n_query) AS n_query,
				 PERCENTILE(n_query, 0.5) AS median_n_query_per_user
				 FROM				 				 
				 (SELECT CONCAT(year,'-',month,'-',day) AS dt, 
				 user_agent_map['os_family'] AS os, 
				 CONCAT(client_ip,user_agent) AS user_id,
                 COUNT(uri_query) AS n_query,
				 MAX(agent_type) AS agent_type
                 FROM webrequest",
                 clause_data$date_clause,
				 #"AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day), user_agent_map['os_family'], CONCAT(client_ip,user_agent)) AS agg_by_user				 				 
				 GROUP BY dt, os, agent_type
				 ;")
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(user_by_os, "user_by_os.rds", "gz")
system("scp stat2:/home/chelsyx/user_by_os.rds data/")


# user by browser
user_by_browser <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT dt, browser, agent_type,
				 COUNT(DISTINCT user_id) AS n_user,
				 SUM(n_query) AS n_query,
				 PERCENTILE(n_query, 0.5) AS median_n_query_per_user
				 FROM				 				 
				 (SELECT CONCAT(year,'-',month,'-',day) AS dt, 
				 user_agent_map['browser_family'] AS browser, 
				 CONCAT(client_ip,user_agent) AS user_id,
                 COUNT(uri_query) AS n_query,
				 MAX(agent_type) AS agent_type
                 FROM webrequest",
                 clause_data$date_clause,
				 #"AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day), user_agent_map['browser_family'], CONCAT(client_ip,user_agent)) AS agg_by_user				 				 
				 GROUP BY dt, browser, agent_type
				 ;")
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(user_by_browser, "user_by_browser.rds", "gz")
system("scp stat2:/home/chelsyx/user_by_browser.rds data/")
 

# user by device
user_by_device <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT dt, device, agent_type,
				 COUNT(DISTINCT user_id) AS n_user,
				 SUM(n_query) AS n_query,
				 PERCENTILE(n_query, 0.5) AS median_n_query_per_user
				 FROM				 				 
				 (SELECT CONCAT(year,'-',month,'-',day) AS dt, 
				 user_agent_map['device_family'] AS device, 
				 CONCAT(client_ip,user_agent) AS user_id,
                 COUNT(uri_query) AS n_query,
				 MAX(agent_type) AS agent_type
                 FROM webrequest",
                 clause_data$date_clause,
				 # "AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day), user_agent_map['device_family'], CONCAT(client_ip,user_agent)) AS agg_by_user				 				 
				 GROUP BY dt, device, agent_type
				 ;")
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(user_by_device, "user_by_device.rds", "gz")
system("scp stat2:/home/chelsyx/user_by_device.rds data/")


# user by agent_type
user_by_agent_type <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT dt, agent_type,
				 COUNT(DISTINCT user_id) AS n_user,
				 SUM(n_query) AS n_query,
				 PERCENTILE(n_query, 0.5) AS median_n_query_per_user
				 FROM				 				 				 
				 (SELECT CONCAT(year,'-',month,'-',day) AS dt, 
				 CONCAT(client_ip,user_agent) AS user_id,
                 COUNT(uri_query) AS n_query,
				 MAX(agent_type) AS agent_type
                 FROM webrequest",
                 clause_data$date_clause,
				 #"AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day),CONCAT(client_ip,user_agent)) AS agg_by_user				 				 				 
				 GROUP BY dt, agent_type
				 ;")
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(user_by_agent_type, "user_by_agent_type.rds", "gz")
system("scp stat2:/home/chelsyx/user_by_agent_type.rds data/")


# webrequest by referer_class
webrequest_by_referer_class <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
              SELECT CONCAT(year,'-',month,'-',day) AS dt, referer_class, 
              COUNT(DISTINCT client_ip) AS n_client_ip,
		  	  COUNT(DISTINCT CONCAT(client_ip,user_agent)) AS n_user,
              COUNT(uri_query) AS n_query,
              PERCENTILE_APPROX(time_firstbyte, 0.5) AS median_time_firstbyte,
              PERCENTILE(response_size, 0.5) AS median_response_size,
              SUM(CASE WHEN agent_type = 'user' THEN 1 END) AS n_user_query
              FROM webrequest",
              clause_data$date_clause,
			  # "AND hour=8",
              "AND webrequest_source = 'misc'
              AND uri_host = 'query.wikidata.org'
              AND uri_path = '/bigdata/namespace/wdq/sparql'
              AND http_status IN('200','304')
              AND INSTR(uri_query, '?query=') > 0
              GROUP BY CONCAT(year,'-',month,'-',day), referer_class;") 
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(webrequest_by_referer_class, "webrequest_by_referer_class.rds", "gz")
system("scp stat2:/home/chelsyx/webrequest_by_referer_class.rds data/")


# Median number of queries per user by date
md_query_per_user <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT dt, 
                 PERCENTILE(n_query, 0.5) AS median_n_query_per_user
                 FROM
                 (SELECT CONCAT(year,'-',month,'-',day) AS dt, 
                 CONCAT(client_ip,user_agent) AS user_id,
                 COUNT(uri_query) AS n_query
                 FROM webrequest",
                 clause_data$date_clause,
                 # "AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day), CONCAT(client_ip,user_agent)) AS agg_by_user
                 GROUP BY dt
                 ;")
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(md_query_per_user, "md_query_per_user.rds", "gz")
system("scp stat2:/home/chelsyx/md_query_per_user.rds data/")


# median time first byte, response size
md_1byte_size <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  # clause_data <- wmf::date_clause("2016-07-01")
  query <- paste("USE wmf;
                 SELECT CONCAT(year,'-',month,'-',day) AS dt, 
                 PERCENTILE_APPROX(time_firstbyte, 0.5) AS median_time_firstbyte,
                 PERCENTILE(response_size, 0.5) AS median_response_size
                 FROM webrequest",
                 clause_data$date_clause,
                 # "AND hour=8",
                 "AND webrequest_source = 'misc'
                 AND uri_host = 'query.wikidata.org'
                 AND uri_path = '/bigdata/namespace/wdq/sparql'
                 AND http_status IN('200','304')
                 AND INSTR(uri_query, '?query=') > 0
                 GROUP BY CONCAT(year,'-',month,'-',day);") 
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(md_1byte_size, "md_1byte_size.rds", "gz")
system("scp stat2:/home/chelsyx/md_1byte_size.rds data/")