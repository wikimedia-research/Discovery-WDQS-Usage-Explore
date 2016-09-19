webrequest_by_country <- readr::read_rds("data/webrequest_by_country.rds")
user_by_country <- readr::read_rds("data/user_by_country.rds")
user_by_os <- readr::read_rds("data/user_by_os.rds")
user_by_browser <- readr::read_rds("data/user_by_browser.rds")
user_by_device <- readr::read_rds("data/user_by_device.rds")
user_by_agent_type <- readr::read_rds("data/user_by_agent_type.rds")
webrequest_by_referer_class <- readr::read_rds("data/webrequest_by_referer_class.rds")
md_query_per_user <- readr::read_rds("data/md_query_per_user.rds")
md_1byte_size <- readr::read_rds("data/md_1byte_size.rds")

library(dplyr)
library(ggplot2)
library(magrittr)
import::from(ggthemes, theme_tufte)

theme_set(theme_tufte(base_family = "Gill Sans", base_size = 18))

webrequest_by_country$n_user_query <- as.numeric(webrequest_by_country$n_user_query)
webrequest_by_country$dt %<>% lubridate::ymd()
user_by_country$n_spider <- as.numeric(user_by_country$n_spider)
user_by_country$dt %<>% lubridate::ymd()

temp <- webrequest_by_country %>%
  mutate(n_user_query = ifelse(is.na(n_user_query), 0, n_user_query)) %>%
  group_by(dt) %>%
  summarise(all_query=sum(n_query),user_query=sum(n_user_query)) %>%
  mutate(spider_query=all_query-user_query) %>%
  tidyr::gather("type", "n", 2:4)
  {ggplot(temp, aes(x=dt, y=n, colour=type)) + 
  geom_line() +
  geom_segment(data = filter(temp, type=="all_query", dt == temp$dt[which.max(n)]),
               aes(y = 0, yend=n, x = dt, xend = dt),
               color = "black", linetype = "dashed") +
  geom_text(data = filter(temp, type=="all_query", dt == temp$dt[which.max(n)]),
              aes(label = paste0(dt,", ",n," total queries")),
              hjust = "right", vjust = "top", color = "black", nudge_x = -0.1, nudge_y = -0.05, size=7) +
  scale_x_date(name = "Date") +
  scale_y_continuous(labels=polloi::compress, name = "Number of Queries") +
  scale_color_discrete(name="User Type",
                      breaks=c("all_query", "spider_query", "user_query"),
                      labels=c("All", "Known Automata", "User")) +
  ggtitle("Number of WDQS Queries", subtitle="July 1st - August 29th")} %>%
  ggsave("all_query_ts.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

{user_by_country %>%
  mutate(n_spider = ifelse(is.na(n_spider), 0, n_spider)) %>%
  group_by(dt) %>%
  summarise(all_user=sum(n_user),spider=sum(n_spider)) %>%
  mutate(user=all_user-spider) %>%
  tidyr::gather("agent_type", "n", 2:4) %>%
  ggplot(aes(x=dt, y=n, colour=agent_type)) + 
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  scale_color_discrete(name="User Type",
                       breaks=c("all_user", "spider", "user"),
                       labels=c("All", "Known Automata", "User")) +
  ggtitle("Number of WDQS Users", subtitle="July 1st - August 29th")} %>%
  ggsave("all_user_ts.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

# median no. query by user ts
md_query_per_user$dt %<>% lubridate::ymd()
{ggplot(md_query_per_user, aes(x=dt, y=median_n_query_per_user)) + 
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Median Number of Queries per User (IP+UA)") +
  ggtitle("Median Number of Queries per User", subtitle="July 1st - August 29th")} %>%
  ggsave("md_query_per_user_ts.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

# median time first byte ts
md_1byte_size$dt %<>% lubridate::ymd()
{ggplot(md_1byte_size, aes(x=dt, y=median_time_firstbyte)) + 
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Median Time to First Byte (Second)") +
  ggtitle("Median Time to First Byte", subtitle="July 1st - August 29th")} %>%
  ggsave("median_time_firstbyte_ts.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

# median response size ts
{ggplot(md_1byte_size, aes(x=dt, y=median_response_size)) + 
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Median Response Size") +
  ggtitle("Median Response Size", subtitle="July 1st - August 29th")} %>%
  ggsave("median_response_size_ts.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

# top country ts
top_query_country <- webrequest_by_country %>%
  group_by(country)  %>%
  summarise(n_query=sum(n_query)) %>%
  arrange(desc(n_query)) %>%
  top_n(10, n_query) %>%
  {.$country}
temp <- webrequest_by_country %>%
  filter(country %in% top_query_country) %>%
  group_by(dt,country) %>%
  summarise(all_query=sum(n_query))
query_country_ts_p1 <- ggplot(temp, aes(x=dt, y=all_query, colour=country)) + 
  geom_line() +
  geom_segment(data = filter(temp, country==temp$country[which.max(temp$all_query)], dt == temp$dt[which.max(temp$all_query)]),
               aes(y = 0, yend=all_query, x = dt, xend = dt),
               color = "black", linetype = "dashed") +
  geom_text(data = filter(temp, country==temp$country[which.max(temp$all_query)], dt == temp$dt[which.max(temp$all_query)]),
            aes(label = paste0(dt,", ",all_query," queries in ", country)),
            hjust = "right", vjust = "top", color = "black", nudge_x = -0.1, nudge_y = -0.05) +
  scale_x_date(name = "Date") +
  scale_y_continuous(breaks=c(1,2e5,4e5,6e5),labels=polloi::compress, name = "Number of Queries") +
  ggtitle("Top 10 Countries by Number of WDQS Queries", subtitle="July 1st - August 29th")
query_country_ts_p2 <- ggplot(temp, aes(x=dt, y=all_query, colour=country)) + 
  #geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 9)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_x_date(name = "Date") +
  scale_y_log10(labels=polloi::compress, name = "Number of Queries") +
  ggtitle("Top 10 Countries by Number of WDQS Queries", subtitle="July 1st - August 29th, Smoothed")  
query_country_ts_p <- plot_grid(plotlist = list(query_country_ts_p1, query_country_ts_p2), ncol = 2)
ggsave("query_country_ts.png", query_country_ts_p, path = "figures", width = 15, height = 5, units = "in", dpi = 300)


top_user_country <- user_by_country %>%
  group_by(country)  %>%
  summarise(n_user=sum(n_user)) %>%
  arrange(desc(n_user)) %>%
  top_n(10, n_user) %>%
  {.$country}
temp <- user_by_country %>%
  filter(country %in% top_user_country) %>%
  group_by(dt,country) %>%
  summarise(all_user=sum(n_user))
user_country_ts_p1 <- ggplot(temp, aes(x=dt, y=all_user, colour=country)) + 
  geom_line() +
  geom_segment(data = filter(temp, country==temp$country[which.max(temp$all_user)], dt == temp$dt[which.max(temp$all_user)]),
               aes(y = 0, yend=all_user, x = dt, xend = dt),
               color = "black", linetype = "dashed") +
  geom_text(data = filter(temp, country==temp$country[which.max(temp$all_user)], dt == temp$dt[which.max(temp$all_user)]),
            aes(label = paste0(dt,", ",all_user," users in ", country)),
            hjust = "right", vjust = "top", color = "black", nudge_x = -0.1, nudge_y = -0.05) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  ggtitle("Top 10 Countries by Number of WDQS Users", subtitle="July 1st - August 29th")
user_country_ts_p2 <- ggplot(temp, aes(x=dt, y=all_user, colour=country)) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 50)) +
  # geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  ggtitle("Top 10 Countries by Number of WDQS Users", subtitle="July 1st - August 29th, Smoothed")
user_country_ts_p <- plot_grid(plotlist = list(user_country_ts_p1, user_country_ts_p2), ncol = 2)
ggsave("user_country_ts.png", user_country_ts_p, path = "figures", width = 14, height = 5, units = "in", dpi = 300)


# Exclude US spider 0816-0819
temp <- webrequest_by_country %>%
  mutate(n_user_query = ifelse(is.na(n_user_query), 0, n_user_query)) %>%
  mutate(n_query = ifelse(webrequest_by_country$country=="United States" & webrequest_by_country$dt %in% seq(as.Date("2016-08-16"), as.Date("2016-08-19"), "day"),
                          n_user_query, n_query)) %>%
  group_by(dt) %>%
  summarise(all_query=sum(n_query),user_query=sum(n_user_query)) %>%
  mutate(spider_query=all_query-user_query) %>%
  tidyr::gather("type", "n", 2:4)
{ggplot(temp, aes(x=dt, y=n, colour=type)) + 
  geom_line() +
  geom_segment(data = filter(temp, type=="all_query", dt == temp$dt[which.max(n)]),
               aes(y = 0, yend=n, x = dt, xend = dt),
               color = "black", linetype = "dashed") +
  geom_text(data = filter(temp, type=="all_query", dt == temp$dt[which.max(n)]),
            aes(label = paste0(dt,", ",n," total queries")),
            hjust = "right", vjust = "top", color = "black", nudge_x = -0.1, nudge_y = -0.05, size=7) +
  scale_x_date(name = "Date") +
  scale_y_continuous(breaks=c(1, seq(2e5,6e5,2e5)), labels=polloi::compress, name = "Number of Queries") +
  scale_color_discrete(name="User Type",
                       breaks=c("all_query", "spider_query", "user_query"),
                       labels=c("All", "Known Automata", "User")) +
  ggtitle("Number of WDQS Queries", subtitle="July 1st - August 29th, Excluding US Known Automata from Aug 16-19")} %>%
  ggsave("all_query_ecl_us_spider0816_ts.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

# BFAST on query
library(bfast)
source("seasonal.R")
query_ts <- webrequest_by_country %>%
  mutate(n_user_query = ifelse(is.na(n_user_query), 0, n_user_query)) %>%
  mutate(n_query = ifelse(webrequest_by_country$country=="United States" & webrequest_by_country$dt %in% seq(as.Date("2016-08-16"), as.Date("2016-08-19"), "day"),
                          n_user_query, n_query)) %>%
  group_by(dt) %>%
  summarise(all_query=sum(n_query),user_query=sum(n_user_query)) %>%
  {.$all_query} %>%
  ts(frequency=7)
bpfit <- bfast(query_ts, h=.25, season="harmonic", max.iter=100)
bpfit
png("figures/adjust_query_decompose.png",width = 10, height = 10, units = "in", res = 300)
# plot(bpfit, type="components", main="BFAST Decomposition: Adjusted Number of Queries")
out <- bpfit$output[[2]]
ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
tsp(ft) <- tsp(bpfit$Yt)
ft <- list(time.series = ft)
seasonal(ft, out, main = "BFAST Decomposition: Adjusted Number of Queries")
dev.off()

# BFAST on user
user_ts <- user_by_country %>%
  mutate(n_spider = ifelse(is.na(n_spider), 0, n_spider)) %>%
  group_by(dt) %>%
  summarise(all_user=sum(n_user),spider=sum(n_spider)) %>%
  {.$all_user} %>%
  ts(frequency=7)
bpfit <- bfast(user_ts, h=.25, season="harmonic", max.iter=100)
bpfit
png("figures/user_decompose.png",width = 10, height = 10, units = "in", res = 300)
# plot(bpfit, type="components", main="BFAST Decomposition: The Number of Users")
out <- bpfit$output[[1]]
ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
tsp(ft) <- tsp(bpfit$Yt)
ft <- list(time.series = ft)
seasonal(ft, out, main = "BFAST Decomposition: The Number of Users")
dev.off()
