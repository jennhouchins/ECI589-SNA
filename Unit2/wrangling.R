# ASSIGNMENT DESCRIPTION #####################################
# File:         wrangling.R
# Project:      Unit 2 Independent Analysis
# Author:       Jennifer Houchins
#
# Description:  This file just served as a place to test all my code chunks
#               before putting them in my Rmarkdown xaringan presentation

# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, tidytext, igraph, rtweet,
               tidygraph, ggraph, vader, webshot, here)

get_token()

csed_tweets <- search_tweets(q = "#CSforAll OR #CSEd OR #CSEducation OR #KaporCRCS OR #CSForGood",
                             n=5000,
                             lang = "en", 
                             include_rts = FALSE)


csed_tweets_1 <- rtweet::flatten(csed_tweets)

write_csv(csed_tweets_1, here("Presentation", "data","csed-tweets.csv"))


sample_n(csed_tweets, 10) %>%
  select(screen_name, text)


# webshot(url = NULL, file = "webshot.png", vwidth = 992,
#         vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
#         delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
#         useragent = NULL)


# Extract edges and nodes

ties <-  csed_tweets_1 %>%
  relocate(sender = screen_name, # rename screen_name to sender
           target = mentions_screen_name) %>% # rename to receiver
  select(sender,
         target,
         created_at,
         text) %>%
  unnest_tokens(input = target,
                output = receiver,
                to_lower = FALSE) %>%
  relocate(sender, receiver) %>%
  drop_na(receiver)


actors_1 <- ties %>%
  pivot_longer(cols = sender:receiver, 
               names_to = "nodes",
               values_to = "screen_name") 

actors <- actors_1 %>% 
  select(screen_name) %>%
  distinct() %>%
  drop_na()


#create network object

network <- tbl_graph(edges = ties, 
                     nodes = actors)

network

# explore - centrality

network_1 <- network %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

network_1

node_measures <- network_1 %>% 
  activate(nodes) %>%
  data.frame()

summary(node_measures)

# the engaged folks

transmitters <- node_measures %>% 
  top_n(1, out_degree)

transceivers <- node_measures %>% 
  top_n(1, in_degree)

transcenders <- node_measures %>% 
  top_n(1, degree)


# closeness and betweenness

network_2 <- network_1 %>% 
  activate(nodes) %>% 
  mutate(closeness = centrality_closeness(),
         betweenness = centrality_betweenness())

# sociogram

ggraph(network_1, layout = "fr") + 
  geom_node_point(aes(size = in_degree, 
                      alpha = out_degree, 
                      colour = degree)) +
  geom_node_text(aes(label = screen_name, 
                     size = degree/2,
                     alpha = degree), 
                 repel=TRUE) +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                 end_cap = circle(3, 'mm'),
                 alpha = .3) + 
  theme_graph()

# Community detection

network_3 <- network_2 %>%
  activate(nodes) %>%
  mutate(group = group_infomap())

network_3

network_3 %>%
  ggraph(layout = "kk") + 
  geom_node_point(aes(size = in_degree, 
                      colour = group)) +
  geom_node_text(aes(label = screen_name, 
                     size = degree/2,
                     alpha = degree), 
                 repel=TRUE) +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                 end_cap = circle(3, 'mm'),
                 alpha = .3) + 
  theme_graph()


groups <- network_3 %>% 
  activate(nodes) %>%
  data.frame()

groups$group %>% 
  sort()


# Sentiments

summary_vader <- vader_df(ties$text)

summary_vader

tweet_sentiment <-inner_join(summary_vader, 
                             ties,
                             by = "text")

tweet_sentiment

user_sentiment <- tweet_sentiment %>%
  group_by(sender) %>%
  summarise(sentiment = mean(compound))

user_sentiment

user_positive_sentiment <- tweet_sentiment %>%
  group_by(sender) %>%
  summarise(sentiment = mean(compound)) %>% 
  filter(sentiment > 0) 

user_positive_sentiment
