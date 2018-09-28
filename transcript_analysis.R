library(stringr)
library(reader)
library(dplyr)
library(tidytext)

rm(list = ls())

df <- read.csv("transcript_parsed.txt", quote='"')

# Get the Ford and Kavanaugh lines
df_sub <- subset(df,Speaker %in% c("KAVANAUGH","FORD"))

# Lowercase
df_sub$Statement <- tolower(df_sub$Statement)
# Remove parentheticals
df_sub$Statement <- gsub("\\s*\\([^\\)]+\\)","",df_sub$Statement)

# words to remove- names and honorifics that do not contribute to this analysis
blacklist <- c("dr","kavanaugh","brett","ford","christine","blasey","judge","ms","mark","leland")

# Now lets process
tidy_text <- df_sub %>% 
  unnest_tokens(word, Statement) %>%
  filter(!word %in% stop_words$word,
         !word %in% blacklist,
         str_detect(word, "[a-z]"))


# Compute the word ratios
word_ratios <- tidy_text %>%
  count(word, Speaker) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(Speaker, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(FORD / KAVANAUGH)) %>%
  arrange(desc(logratio))

word_ratios_top <- word_ratios %>% 
  group_by(logratio < 0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  arrange(abs(logratio))
colnames(word_ratios_top)[5] <- "who"
word_ratios_top$who <- as.factor(word_ratios_top$who)
levels(word_ratios_top$who) <- c("Dr. Ford", "Judge\nKavanaugh")

px <- ggplot(word_ratios_top, aes(reorder(word, abs(logratio)), abs(logratio), fill = who)) +
  facet_wrap(~who, scales = "free_y")+
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Likelihood") +
  xlab("")+
  theme_bw()+
  #scale_discrete_manual(values = c("purple","green"))+
  scale_fill_manual(name = "", labels = c("Ford", "Kavanugh"),
                    values = c("mediumpurple3","mediumseagreen"))+
  theme(axis.text.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 20),
        plot.title = element_text(size = 24, hjust = 0.5),
        panel.spacing = unit(2, "lines"))+
  ggtitle("Common Words Most Associated with Speakers\nat the Sept. 27, 2018 Senate Judiciary Hearing")
px

ggsave("Hearing_Word_Usage.png", plot = px, width = 11, height = 8.5 )
