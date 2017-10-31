


train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("data/test.csv", stringsAsFactors = FALSE)

train$User_ID <- as.factor(train$User_ID)
test$User_ID <- as.factor(test$User_ID)

train$Product_ID <- as.factor(train$Product_ID)
test$Product_ID <- as.factor(test$Product_ID)

train$Gender <- as.factor(train$Gender)
test$Gender <- as.factor(test$Gender)

train$City_category <- as.factor(train$City_category)
test$City_category <- as.factor(test$City_category)

train$Marital_Status <- as.factor(train$Marital_Status)
test$Marital_Status <- as.factor(test$Marital_Status)



train$source <- "train"
test$source <- "test"
test$Purchase <- 0

df <- rbind(train, test)

summary(df)


summarize(countt = n_distinct(User_ID)) %>%
  arrange(desc(countt)) %>%

df %>% group_by(User_ID) %>% 
  head(10) %>%
  ggplot(aes(User_ID)) + geom_bar()

p2 <- df %>% arrange(desc(sumt)) %>% 
  head(10) %>%
  ggplot(aes(x = User_ID)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = palette_light()) +
  theme_gray() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", fill = "")

p2

df %>% count(User_ID) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(User_ID, n)) + geom_bar(stat = "identity")
  

