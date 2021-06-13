library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(KoNLP)
useNIADic()
library(ggplot2)
library(tidyr)
library(textclean)
library(widyr)
library(tidygraph)
library(ggraph)
library(showtext)
library(tm)

raw_tekken7 = read_csv("tekken7_2.csv") %>% mutate(id = row_number())
tekken7 <- raw_tekken7 %>% mutate(review = str_replace_all(review, "[^°¡-ÆR]", " "), review = str_squish(review)) %>%
  distinct(review, .keep_all = T) %>% filter(str_count(review, boundary("word")) >= 3)
tekken7

tekken <- tekken7 %>% unnest_tokens(input = review, output = word, token = extractNoun, drop = F) %>%
  filter(str_count(word) > 1) %>% group_by(id) %>% distinct(word, .keep_all = T) %>% ungroup() %>% select(id, word)
tekken %>% count(word, sort = T) %>% print(n = 500)
count_word <- tekken %>% add_count(word) %>% filter(n <= 100) %>% select(-n)

stopword <- c("µéÀÌ","ÇØ¼­","ÇÏ±â","ÇÏÁö","¸¸Å­","ÀÌ°É","ÇØº¸","ÇÏ´Ù",
              "ÇØµµ","´ç½Å","µîµî","±îÁö","ÀÌ°Å","¸¶¶ó","¼ø°£","¾ðÁ¦",
              "¾Æ¹«°Í","¿©±â","ÇÏ¶ó","±×°Å","±×°Í","±×³¯","±×·±°Å",
              "±×µé","¸Ç³¯","¹º°¡","º°°Å","ÇÏ°Ô","ÇÏ¸é","ÇÏ³×","ÇØ¿ä",
              "´Ïµé","ÇÏ±â","ÇÏÁö","ÇÑ°Å","ÇØÁÖ","¾îµð","ÇÏ½Å","ÇÏ°Å",
              "ÇØ¶ó","°¡²û","»ò´Ù","È¤½Ã","´ëºÎºÐ","±×µý°Å","Á¤µµ",
              "´Ï¿¡","µ¿¾È","·ùÀÇ","·Î¿î","·Èµí")
count_word <- count_word %>% filter(!word %in% stopword) %>%
  mutate(word = recode(word, "·»–jÀ»" = "·£»Ì",
                       "·»»Ì¸¸" = "·£»Ì",
                       "·»»ÌÀ»" = "·£»Ì",
                       "·£»Ì¿¡´Ù°¡" = "·£»Ì",
                       "·£»Ì¿¡" = "·£»Ì",
                       "·£»ÌÇÏ³Ä" = "·£»Ì",
                       "·£»ÌÇÏ°í" = "·£»Ì",
                       "·£»ÌÁ»" = "·£»Ì",
                       "·£»ÌÇÏ´Â" = "·£»Ì"))
count_word_doc <- count_word %>% count(id, word, sort = T)
dtm_tekken <- count_word_doc %>% cast_dtm(document = id, term = word, value = n)
dtm_tekken
as.matrix(dtm_tekken[1:7, 1:7])
library(topicmodels)
library(ldatuning)
models <- FindTopicsNumber(dtm = dtm_tekken, topics = 2:20, return_models = T,
                           control = list(seed = 1234))
models %>% select(topics, Griffiths2004)
FindTopicsNumber_plot(models)
lda_model <- LDA(dtm_tekken, k = 8, method = "Gibbs", control = list(seed = 1234))
lda_model
term_topic <- tidy(lda_model, matrix = "beta")
term_topic %>% filter(topic == 5) %>% arrange(-beta)
terms(lda_model, 10) %>% data.frame()
term_topic
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic %>% count(topic)
doc_class <- doc_topic %>% group_by(document) %>% slice_max(gamma, n = 1)
doc_class$document <- as.integer(doc_class$document)
doc_class %>% arrange(document)
raw_tekken7 <- read_csv("tekken7_2.csv") %>% mutate(id = row_number())
tekken7_topic <- raw_tekken7 %>% left_join(doc_class, by = c("id" = "document"))
tekken7_topic <- tekken7_topic %>% na.omit()
top_terms <- term_topic %>% group_by(topic) %>% 
  slice_max(beta, n = 6, with_ties = F) %>% summarise(term = paste(term, collapse = ", "))
top_terms
count_topic <- tekken7_topic %>% count(topic)
count_topic_word <- count_topic %>% left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("TOPIC", topic))
count_topic_word

ggplot(count_topic_word, aes(x = reorder(topic_name, n), y= n, fill = topic_name)) +
  geom_col(show.legend = F) + coord_flip() + geom_text(aes(label = n),hjust = -0.2) +
  geom_text(aes(label = term), hjust = 1.04, col = "white", fontface = "bold") +
  scale_y_continuous(limits = c(0, 150)) + labs(x = NULL)
tekken_topic <- tekken7_topic %>% mutate(review = str_squish(replace_html(review))) %>%
  arrange(-gamma)
tekken_topic %>% select(gamma, review)
tekken_topic %>% filter(topic == 1 & str_detect(review, "¿À¶ô½Ç")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 2 & str_detect(review, "ÃßÃµ")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 3 & str_detect(review, "´ººñ")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 4 & str_detect(review, "¿Â¶óÀÎ")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 5 & str_detect(review, "Ä£±¸")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 6 & str_detect(review, "¹ë·±½º")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 7 & str_detect(review, "¸Á°×")) %>%
  head(10) %>% pull(review)
tekken_topic %>% filter(topic == 8 & str_detect(review, "ÀÚ½Å")) %>%
  head(10) %>% pull(review)
name_topic <- tibble(topic = 1:8,
                     name = c("1. ¿À¶ô½Ç ½ÃÀý°úÀÇ ºñ±³",
                              "2. ÃßÃµ/ºñÃßÃµÀÇ ÀÌÀ¯",
                              "3. ´ººñ·Î¼­ÀÇ Èûµê",
                              "4. ¿Â¶óÀÎ ´ëÀü¿¡ ´ëÇÑ ºñÆÇ",
                              "5. Ä£±¸ÀÇ À¯¹«¿¡ µû¸¥ ÃßÃµ",
                              "6. °ÔÀÓÀÇ ¹ë·±½º¿¡ ´ëÇÑ ºñÆÇ",
                              "7. ¸Á°×ÀÌÁö¸¸ »ý°¢³ª´Â °ÔÀÓ",
                              "8. ½ÃÀÛ, À¯ÀÔ¿¡ ´ëÇÑ ÀÇ°ß"))
top_term_topic <- term_topic %>% group_by(topic) %>% slice_max(beta, n = 10)
top_term_topic_name <- top_term_topic %>% left_join(name_topic, by = "topic")
top_term_topic_name
ggplot(top_term_topic_name, aes(x= reorder_within(term, beta, name), y=beta, 
                                fill =factor(topic))) + geom_col(show.legend = F) +
  facet_wrap(~name, scales = "free", ncol = 2) + coord_flip() + scale_x_reordered() +
  labs(title = "Tekken7¿¡ ´ëÇÑ À¯ÀúµéÀÇ Æò°¡ ÅäÇÈ", subtitle = "ÅäÇÈº° ÁÖ¿ä ´Ü¾î top10",
       x = NULL, y = NULL) + theme(title = element_text(size = 14))
#------------------LDA¸ðµ¨¸µ-----------------------

glimpse(raw_tekken7)
tekken7 <- raw_tekken7 %>% filter(str_count(review, " ") >= 1) %>% 
  mutate(review_raw = str_squish(replace_html(review)), 
         review = str_replace_all(review, "[^°¡-ÆR]", " "),
         review = str_squish(review))
word_noun <- tekken7 %>% unnest_tokens(input = review, output = word,
                                       token = extractNoun, drop = F)
frequency <- word_noun %>% count(word, sort = T) %>% filter(str_count(word)>1)
frequency %>% head(30) %>% print(n = 30)
stopword_noun <- c("µéÀÌ","ÇÏ¸é","ÇÏ°Ô","ÇØ¼­","¸¶Á¦","Á¤µµ")
top20_noun <- frequency %>% filter(!word %in% stopword_noun) %>% head(20)
top20_noun %>% print(n = Inf)

library(scales)
ggplot(top20_noun, aes(x = reorder(word, n), y = n)) + geom_col() +
  coord_flip() + geom_text(aes(label = comma(n, accuracy = 1)), hjust = -0.3) +
  scale_y_continuous(limits = c(0,400)) + labs(title = "Tekken7¿¡ ´ëÇÑ Æò°¡ ÁÖ¿ä ´Ü¾î",
                                               subtitle = "¾ð±Þ ºóµµ Top20", x =NULL) +
  theme_minimal() + theme(plot.title = element_text(size = 14, face = "bold"),
                          plot.subtitle = element_text(size = 12))
'''
word_sympathy <- word_noun %>% rename(like = sympathyCount, dislike = antipathyCount) %>%
  mutate(diff = like - dislike, sympathy = ifelse(diff >= 1, "like",
                                                  ifelse(diff<= -1, "dislike", "neutral")))
'''
#------------------wordCloud--------------------------
library(ggwordcloud)
new = raw_tekken7 %>% str_replace_all("[^°¡-ÆR]", " ") %>% str_squish() %>% as_tibble()
word_space <- new %>% unnest_tokens(input = value, output = word, token ="words") %>% 
  filter(str_count(word) > 1) %>% count(word, sort = T)
font_add_google(name = "Stylish", family = "st")
showtext_auto()
ggplot(word_space, aes(label = word, size = n)) + geom_text_wordcloud(seed=1234, family = "st") +
 scale_radius(limits = c(8,NA), range = c(3,30)) +  scale_color_gradient2(low = "#B4C3FF", high = "#5ACCFF") + theme_minimal()
#------------------sentiment_doc----------------------
dic <- read_csv("knu_sentiment_lexicon.csv")
word_tekken <- tekken7 %>% unnest_tokens(input = review, output = word, token = "words", drop = F) %>% filter(str_count(word) > 1)
word_tekken <- word_tekken %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_tekken %>% select(word, polarity)
word_tekken <- word_tekken %>% mutate(sentiment = ifelse(polarity >= 1, "pos", ifelse(polarity <= -1, "neg", "neu")))
word_tekken %>% count(sentiment)
top10_sentiment <- word_tekken %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n = 10)
top10_sentiment %>% print(n = Inf)
score_tekken <- word_tekken %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
score_tekken %>% select(score, review) %>% arrange(score)
score_tekken %>% count(score) %>% print(n = Inf)
score_tekken <- score_tekken %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score<=-1, "neg", "neu")))
freq_score <- score_tekken %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
freq_score
ggplot(freq_score, aes(x = sentiment, y= n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos","neu","neg"))
ggplot(top10_sentiment, aes(x = reorder(word, n), y =n, fill = sentiment)) + geom_col() + coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + facet_wrap(~sentiment, scales = "free") + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))  + labs(x=NULL)

#------------------cor_graph---------------------------
tekken_ <- raw_tekken7 %>% select(review) %>% mutate(review = str_replace_all(review, "[^°¡-ÆR]", " "),
                                                     review = str_squish(review), id = row_number())
tekken_pos <- tekken_ %>% unnest_tokens(input = review, output = word, token = "words", drop = F)
tekken_pos <- tekken_pos %>% filter(str_count(word) > 1) %>% arrange(id)
word_cors <- tekken_pos %>% add_count(word) %>% filter(n>=20) %>% pairwise_cor(item = word,
                                                                               feature = id, sort = T)
set.seed(1234)
graph_cors <-  word_cors %>% filter(correlation >= 0.10) %>% as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

set.seed(1234)
ggraph(graph_cors, layout = "fr") + geom_edge_link(color = "gray50", aes(edge_alpha = correlation,
                                                                         edge_width = correlation), show.legend = F) +
  scale_edge_width(range = c(1,4)) + geom_node_point(aes(size = centrality, color = group), show.legend = F)+
  scale_size(range = c(5,10)) + geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()
#------------------bigram_graph------------------------
line_tekken <- tekken_pos %>% group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
bigram_tekken <- line_tekken %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
bigram_separated <- bigram_tekken %>% separate(bigram, c("word1", "word2"), sep = " ")
pair_bigram <- bigram_separated %>% filter(word1 != word2) %>%count(word1, word2, sort = T) %>% na.omit()

set.seed(1234)
graph_bigram <- pair_bigram %>% filter(n >= 3) %>% as_tbl_graph(directed = F) %>% 
  mutate(contrality = centrality_degree(), group = as.factor(group_infomap()))

set.seed(1234)
ggraph(graph_bigram, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + 
  geom_node_point(aes(size = contrality, color = group), show.legend = F) + scale_size(range = c(4,8)) +
  geom_node_text(aes(label = name), repel = T, size = 5) + theme_graph()

