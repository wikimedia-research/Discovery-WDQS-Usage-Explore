webrequest_by_country <- readr::read_rds("data/webrequest_by_country.rds")
user_by_country <- readr::read_rds("data/user_by_country.rds")
user_by_os <- readr::read_rds("data/user_by_os.rds")
user_by_browser <- readr::read_rds("data/user_by_browser.rds")
user_by_device <- readr::read_rds("data/user_by_device.rds")
user_by_agent_type <- readr::read_rds("data/user_by_agent_type.rds")
webrequest_by_referer_class <- readr::read_rds("data/webrequest_by_referer_class.rds")

library(dplyr)
library(ggplot2)
library(cowplot)
import::from(ggthemes, theme_tufte)

theme_set(theme_tufte(base_family = "Gill Sans", base_size = 18))

# Aggregate by country
webrequest_by_country$n_user_query <- as.numeric(webrequest_by_country$n_user_query)
user_by_country$n_spider <- as.numeric(user_by_country$n_spider)

{webrequest_by_country %>%
  mutate(n_user_query = ifelse(is.na(n_user_query), 0, n_user_query)) %>%
  group_by(country)  %>%
  summarise(n_query=sum(n_query), user_query=sum(n_user_query)) %>%
  mutate(spider_query=n_query-user_query) %>%
  arrange(desc(n_query)) %>%
  top_n(20, n_query) %>%
  select(-n_query) %>%
  tidyr::gather("agent_type", "n", 2:3) %>%
  group_by(country) %>%
  mutate(all_n= sum(n)) %>%
  ggplot(aes(x = factor(country, levels=country[1:20]), y = n, fill = agent_type)) + 
  geom_bar(stat = "identity")+
  scale_x_discrete(name = "Country") +
  scale_y_continuous(labels = polloi::compress, name = "Number of Queries") +
  scale_fill_discrete(name="User Type",
                      breaks=c("spider_query", "user_query"),
                      labels=c("Known\nAutomata", "User")) +
  geom_text(aes(x=country, y=all_n, ymax=all_n, label=polloi::compress(all_n), 
                hjust=ifelse(all_n>3e6, 0.5,0))) +
  coord_flip()+
  ggtitle("Top 20 Countries by Number of WDQS Queries")} %>%
  ggsave("n_query_by_country.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

{user_by_country %>%
  mutate(n_spider = ifelse(is.na(n_spider), 0, n_spider)) %>%
  group_by(country)  %>%
  summarise(n_user=sum(n_user), spider=sum(n_spider)) %>%
  mutate(user=n_user-spider) %>%
  arrange(desc(n_user)) %>%
  top_n(20, n_user) %>%
  select(-n_user) %>%
  tidyr::gather("agent_type", "n", 2:3) %>%
  group_by(country) %>%
  mutate(all_n= sum(n)) %>%
  ggplot(aes(x = factor(country, levels=country[1:20]), y = n, fill = agent_type)) + 
  geom_bar(stat = "identity")+
  scale_x_discrete(name = "Country") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  scale_fill_discrete(name="User Type",
                      breaks=c("spider", "user"),
                      labels=c("Known\nAutomata", "User")) +
  geom_text(aes(x=country, y=all_n, ymax=all_n, label=polloi::compress(all_n), 
                hjust=ifelse(all_n>6000, 0.5,0))) +
  coord_flip()+
  ggtitle("Top 20 Countries by Number of WDQS Users")} %>%
  ggsave("n_user_by_country.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)


# Aggregate by os
top15_os_by_user <- user_by_os %>%
  filter(agent_type=='user') %>%
  group_by(os) %>%
  summarise(n_user=sum(n_user)) %>%
  arrange(desc(n_user)) %>%
  top_n(15, n_user)
{user_by_os %>%
  filter(agent_type=='user') %>%
  group_by(os) %>%
  summarise(n_user=sum(n_user)) %>%
  filter(os %in% top15_os_by_user$os) %>%
  ggplot(aes(x = factor(os, levels=top15_os_by_user$os), y = n_user)) + 
  geom_bar(stat = "identity", fill="#FF9999")+
  scale_x_discrete(name = "Operating Systems") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  geom_text(aes(x=os, y=n_user, ymax=n_user, label=polloi::compress(n_user), 
                hjust=ifelse(n_user>6000,0.5,0)), size=6) +
  theme(axis.text=element_text(size=18))+
  coord_flip() +
  ggtitle("Top 15 Operating Systems by Number of Not Known Automata WDQS Users")} %>%
  ggsave("n_user_by_os.png", ., path = "figures", width = 12, height = 10, units = "in", dpi = 300)

top15_os_by_query <- user_by_os %>%
  filter(agent_type=='user') %>%
  group_by(os) %>%
  summarise(n_query=sum(n_query)) %>%
  arrange(desc(n_query)) %>%
  top_n(15, n_query)
{user_by_os %>%
  filter(agent_type=='user') %>%
  group_by(os) %>%
  summarise(n_query=sum(n_query)) %>%
  filter(os %in% top15_os_by_query$os) %>%
  ggplot(aes(x = factor(os, levels=top15_os_by_query$os), y = n_query)) + 
  geom_bar(stat = "identity", fill="#FF9999")+
  scale_x_discrete(name = "Operating Systems") +
  scale_y_log10(breaks = c(10, 1e3, 1e5, 1.3e6), labels = polloi::compress, name = "Number of Queries") +
  # scale_y_continuous(breaks = c(1, 5e5, 1e6), labels = polloi::compress, name = "Number of Queries") +
  geom_text(aes(x=os, y=n_query, ymax=n_query, label=polloi::compress(n_query), 
                hjust=ifelse(n_query>1.2e6,0.5,0)), size=6) +
  theme(axis.text=element_text(size=18))+
  coord_flip() +
  ggtitle("Top 15 Operating Systems by Number of Not Known Automata WDQS Queries")} %>%
  ggsave("n_query_by_os.png", ., path = "figures", width = 13, height = 10, units = "in", dpi = 300)


# Aggregate by browser
top15_browser_by_user <- user_by_browser %>%
  filter(agent_type=='user') %>%
  group_by(browser) %>%
  summarise(n_user=sum(n_user)) %>%
  arrange(desc(n_user)) %>%
  top_n(15, n_user)
{user_by_browser %>%
  filter(agent_type=='user') %>%
  group_by(browser) %>%
  summarise(n_user=sum(n_user)) %>%
  filter(browser %in% top15_browser_by_user$browser) %>%
  ggplot(aes(x = factor(browser, levels=top15_browser_by_user$browser), y = n_user)) + 
  geom_bar(stat = "identity", fill="#FF9999")+
  scale_x_discrete(name = "Browser") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  geom_text(aes(x=browser, y=n_user, ymax=n_user, label=polloi::compress(n_user), 
                hjust=ifelse(n_user>1e4,0.5,0)), size=6) +
  theme(axis.text=element_text(size=18))+
  coord_flip() + 
  ggtitle("Top 15 Browser by Number of Not Known Automata WDQS Users")} %>%
  ggsave("n_user_by_browser.png", ., path = "figures", width = 12, height = 10, units = "in", dpi = 300)

top15_browser_by_query <- user_by_browser %>%
  filter(agent_type=='user') %>%
  group_by(browser) %>%
  summarise(n_query=sum(n_query)) %>%
  arrange(desc(n_query)) %>%
  top_n(15, n_query)
{user_by_browser %>%
  filter(agent_type=='user') %>%
  group_by(browser) %>%
  summarise(n_query=sum(n_query)) %>%
  filter(browser %in% top15_browser_by_query$browser) %>%
  ggplot(aes(x = factor(browser, levels=top15_browser_by_query$browser), y = n_query)) + 
  geom_bar(stat = "identity", fill="#FF9999")+
  scale_x_discrete(name = "Browser") +
  scale_y_log10(breaks=c(10,1000,1e5,1.5e6), labels=polloi::compress, name = "Number of Queries") +
  # scale_y_continuous(breaks=c(1, seq(5e5,1.5e6,5e5)), labels=polloi::compress, name = "Number of Queries") +
  geom_text(aes(x=browser, y=n_query, ymax=n_query, label=polloi::compress(n_query), 
                hjust=ifelse(n_query>1.3e6,0.5,0)), size=6) +
  theme(axis.text=element_text(size=18))+
  coord_flip() +
  ggtitle("Top 15 Browser by Number of Not Known Automata WDQS Queries")} %>%
  ggsave("n_query_by_browser.png", ., path = "figures", width = 12, height = 10, units = "in", dpi = 300)


# Aggregate by device
top15_device_by_user <- user_by_device %>%
  group_by(device) %>%
  summarise(n_user=sum(n_user)) %>%
  arrange(desc(n_user)) %>%
  top_n(15, n_user)
{user_by_device %>%
  group_by(device, agent_type) %>%
  summarise(n_user=sum(n_user)) %>%
  filter(device %in% top15_device_by_user$device) %>%
  ggplot(aes(x = factor(device, levels=top15_device_by_user$device), y = n_user, fill = agent_type)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(name = "Device") +
  scale_y_continuous(name = "Number of Users (IP+UA)") +
  ggtitle("Top 15 Device by Number of WDQS Users")} %>%
  ggsave("n_user_by_device.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)

top15_device_by_query <- user_by_device %>%
  group_by(device) %>%
  summarise(n_query=sum(n_query)) %>%
  arrange(desc(n_query)) %>%
  top_n(15, n_query)
{user_by_device %>%
  group_by(device, agent_type) %>%
  summarise(n_query=sum(n_query)) %>%
  filter(device %in% top15_device_by_query$device) %>%
  ggplot(aes(x = factor(device, levels=top15_device_by_query$device), y = n_query, fill = agent_type)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(name = "Device") +
  scale_y_continuous(name = "Number of Queries") +
  ggtitle("Top 15 Device by Number of WDQS Queries")} %>%
  ggsave("n_query_by_device.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)


# Aggregate by agent_type
agent_type_p1 <- user_by_agent_type %>%
  mutate(agent_type=recode(agent_type, 'spider'='known\nautomata')) %>%
  group_by(agent_type) %>%
  summarise(n_user=sum(n_user)) %>%
  ggplot(aes(x = agent_type, y = n_user)) + 
  geom_bar(stat = "identity", fill="#FF9999")+
  scale_x_discrete(name = "User Type") +
  scale_y_continuous(labels = polloi::compress, name = "Number of Users (IP+UA)") +
  geom_text(aes(x=agent_type, y=n_user, ymax=n_user, label=polloi::compress(n_user), 
                hjust=ifelse(n_user>2e4,0.5,0)), size=6) +
  theme(axis.text=element_text(size=20)) +
  coord_flip() 
agent_type_p2 <- user_by_agent_type %>%
  mutate(agent_type=recode(agent_type, 'spider'='known\nautomata')) %>%
  group_by(agent_type) %>%
  summarise(n_query=sum(n_query)) %>%
  ggplot(aes(x = agent_type, y = n_query)) + 
  geom_bar(stat = "identity", fill="#FF9999")+
  scale_x_discrete(name = "User Type") +
  scale_y_continuous(breaks = c(1, seq(1e6, 4e6, 1e6)), labels = polloi::compress, name = "Number of Queries")+
  geom_text(aes(x=agent_type, y=n_query, ymax=n_query, label=polloi::compress(n_query), hjust=ifelse(n_query>4e6,0.5,0)), size=6) +
  theme(axis.text=element_text(size=20)) +
  coord_flip() 
agent_type_p <- plot_grid(plotlist = list(agent_type_p1, agent_type_p2), nrow = 2, 
                          labels = "Number of WDQS Queries and Number of WDQS Users by User Type", hjust = -0.03, label_size = 20)
ggsave("by_agent_type.png", agent_type_p, path = "figures", width = 10, height = 10, units = "in", dpi = 300)


# Aggregate by referer_class
{webrequest_by_referer_class %>%
  group_by(referer_class) %>%
  summarise(n_query=sum(n_query), user_query=sum(n_user_query)) %>%
  mutate(spider_query=n_query-user_query) %>%
  arrange(desc(n_query)) %>%
  select(-n_query) %>%
  tidyr::gather("agent_type", "n", 2:3) %>%
  group_by(referer_class) %>%
  mutate(all_n=sum(n)) %>%
  ggplot(aes(x = factor(referer_class, levels=referer_class), y = n, fill = agent_type)) + 
  geom_bar(stat = "identity")+
  scale_x_discrete(name = "Referer Class") +
  scale_y_continuous(labels=polloi::compress, name = "Number of Queries") +
  scale_fill_discrete(name="User Type",
                      breaks=c("spider_query", "user_query"),
                      labels=c("Known\nAutomata", "User")) +
  geom_text(aes(x=referer_class, y=all_n, ymax=all_n, label=polloi::compress(all_n), hjust=ifelse(all_n>6e6,0.5,0)), size=6) +
  theme(axis.text=element_text(size=20)) +
  coord_flip()+
  ggtitle("Number of WDQS Queries by Referer Class")} %>%
  ggsave("n_query_by_referer_class.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)
