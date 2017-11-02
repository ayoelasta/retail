


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
  summarize(tot = n(), gend = first(Gender)) %>%
  arrange(desc(tot)) %>%
  head(20) %>%
  ggplot(aes(User_ID, tot, fill = gend)) + geom_bar(stat = "identity") +
  labs(x = "USER", y = "Total number of purchases", fill = "Gender") + 
  ggtitle("Number of purchases by each user showing their gender") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# purchase count by gender
df %>% ggplot(aes(Gender)) + geom_bar()

# purchase amount by gender
p1 <- df %>% filter(source == "train") %>%
  group_by(Gender) %>%
  summarize(totalPurchases = sum(as.numeric(Purchase), na.rm = TRUE),
            count = n()) %>%
  ggplot() + geom_bar(aes(Gender, totalPurchases), stat = "identity")

p2 <- df %>% filter(source == "train") %>%
  group_by(Gender) %>%
  summarize(meanPurchases = mean(as.numeric(Purchase), na.rm = TRUE),
            count = n()) %>%
  ggplot() + geom_bar(aes(Gender, meanPurchases), stat = "identity")


grid.arrange(p1, p2, widths = c(0.5, 0.8))

n_distinct(df$User_ID)

# Purchases by occupation
df %>% filter(source == "train") %>%
  group_by(User_ID) %>%
  summarize(Occ = first(Occupation), sumOcc = sum(Purchase), count = n()) %>%
  group_by(Occ) %>%
  summarize(summ = sum(sumOcc)) %>%
  arrange(desc(summ))


# Purchases made per occupation per gender
df %>% filter(source == "train") %>%
  group_by(User_ID) %>%
  summarize(Occ = first(Occupation), sumOcc = sum(Purchase), count = n(), Gen = first(Gender)) %>%
  ggplot(aes(Occ,sumOcc, fill = Gen)) + geom_bar(stat = "identity") +
  labs(x = "Occupation", y = "Amount Spent on Purchases", title = "Amount spent per Occupation")

# Ratio of males to females in Occupation 9
df %>% filter(Occupation == 9) %>%
  group_by(Gender) %>%
  summarize(n = n())

# Gender ration for all occupations
df %>% group_by(Occupation, Gender) %>%
  summarize(n = n()) %>%
  mutate(freq = round(n / sum(n), 2)) %>%
  ggplot(aes(Occupation, freq, fill = Gender)) + geom_bar(stat = "identity")


df %>% filter(source == "train") %>%
  group_by(Occupation, Gender) %>%
  summarize(sumOcc = sum(Purchase), count = n())


# Age
df %>% filter(source == "train") %>%
  ggplot(aes(Age, Purchase)) + geom_bar(stat = "identity")

df %>% filter(source == "train") %>%
  
