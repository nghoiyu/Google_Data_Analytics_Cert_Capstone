#################################################################################
# Google Data Analytics Certificate Capstone Project
# Hoi Yu Ng
#################################################################################

#clear the environment
rm(list=ls())

#Load necessary packages
library(RSelenium)
library(rvest)
library(tidyverse)
library(data.table)
library(XML)
library(quanteda)
library(quanteda.textstats)
library(readtext)
library(ggplot2)

#Load RSelenium server and open the browser 
#Note: Java must be installed before the browser can be opened
server <- rsDriver(port=4444L, browser = "firefox")
remDr <- server$client

#Create an empty vector to save the question titles
titles <- c()

# create an empty list for data frames
dfs <- list()

#A list of corresponding number of WP MPs in the searching page
MP_No <- c(15, 18, 31, 35, 40, 52, 65, 74, 87)

#A for loop to collect all the titles of the WP MPs' oral parliamentary questions asked in the 14th Parliament until 11 Jan 2022
for (i in MP_No) {
  
  #Open the searching page of the Singapore's Parliament Official Website
  #Click the URL twice, otherwise it may not work
  
  #first click
  remDr$navigate(paste0("https://sprs.parl.gov.sg/search/home"))
  Sys.sleep(2)
  
  #second click
  remDr$navigate(paste0("https://sprs.parl.gov.sg/search/home"))
  Sys.sleep(2)
  
  # Navigate the searching page of the Singapore's Parliament Official Website
  
  #Select the 14th term of parliament using CSS selector
  remDr$findElements("css selector", "option:nth-child(15)")[[1]]$clickElement()
  Sys.sleep(1) #Stop for one second
  
  #Click the "Show Advanced Search" button 
  remDr$findElements("css selector", "#advSearchLabel")[[1]]$clickElement()
  Sys.sleep(1) #Stop for one second
  
  #Click the "by MP" button
  remDr$findElements("css selector", "#byMP")[[1]]$clickElement()
  Sys.sleep(1) #Stop for one second
  
  #Select "Oral Answers to Questions"
  remDr$findElements("css selector", "#advanceSearch > div > div > div:nth-child(3) > div > select > option:nth-child(13)")[[1]]$clickElement()
  Sys.sleep(1) #Stop for one second
  
  #Select "Written Answers to Questions for Oral Answers Not Answered By the End of Question Time" (oral questions not answered)
  remDr$findElements("css selector", "#advanceSearch > div > div > div:nth-child(3) > div > select > option:nth-child(19)")[[1]]$clickElement()
  Sys.sleep(1) #Stop for one second

  #Select the targeted MP
  remDr$findElements("css selector", paste0("#portDIV > select > option:nth-child(", i, ")"))[[1]]$clickElement()
  Sys.sleep(1) #Stop for one second
  
  #Click the search button
  remDr$findElements("css selector", ".btn-black+ .btn-black")[[1]]$clickElement()
  Sys.sleep(3) #Stop for three seconds
  
  #Save all tabs of the browser into an object
  windows <- remDr$getWindowHandles()
  Sys.sleep(3) #Stop for three seconds
  
  #Select the second tab (the search results tap)
  remDr$switchToWindow(windows[[2]])
  Sys.sleep(3) #Stop for three seconds
  
  #Read the html page of the search results
  html <- read_html(remDr$getPageSource()[[1]])
  Sys.sleep(3) #Stop for three seconds
  
  #Scrape the first page
  html <- read_html(remDr$getPageSource()[[1]])
  title1 <- html %>% html_nodes('td a') %>% html_text(trim = T)
  Sys.sleep(3) #Stop for three seconds
  
  #Click the next page button
  remDr$findElements("css selector", "#searchResults > div.col-lg-12.col-md-12.paginationSection > section > ul > li:nth-child(1) > a > em")[[1]]$clickElement()
  Sys.sleep(3) #Stop for three seconds
  
  #Create a new empty vector
  title3 <- c()
  
  Sys.sleep(3) #Stop for three seconds

  #Scrape the remaining pages and stop automatically
  repeat {
    next_link_as_list <- remDr$findElements("css selector", "#searchResults > div.col-lg-12.col-md-12.paginationSection > section > ul > li:nth-child(3) > a > em")
    if (length(next_link_as_list) == 0) {
      break} #Check if a next page exists. If not, break the part of the loop.
    html <- read_html(remDr$getPageSource()[[1]])
    title2 <- html %>% html_nodes('td a') %>% html_text(trim = T)
    title3 <- c(title3, title2) #append the collected titles
    Sys.sleep(3) #Stop for three seconds
    next_link_as_list[[1]]$clickElement()
    Sys.sleep(3) #Stop for three seconds
  }
  
  titles <- c(title1, title3) #append the collected titles
  Sys.sleep(3) #Stop for three seconds
  
  #Create a datafrome for the collected title with the MP's number
  df <- data.frame(question_titles = titles, MP = i)
  Sys.sleep(3) #Stop for three seconds
  
  # append dataframes to a list
  dfs[[i]] <- df
  Sys.sleep(3) #Stop for three seconds
  
  #close the current window
  remDr$closeWindow()
  
  #Return to the first tab (the searching page)
  remDr$switchToWindow(windows[[1]])
}

# Concatenate all dataframes into a single data set
final_dataset <- as.data.frame(rbindlist(dfs))

# Examine and view the final data set
str(final_dataset)
View(final_dataset)

#Create a copy of the final data set
final_dataset2 <- final_dataset

#Replace MPs' numbers with MPs' names based on the source code of the searching page

final_dataset2 <- final_dataset2 %>%                               
  mutate(MP = replace(MP,  MP == 15, "Louis Chua"), 
         MP = replace(MP,  MP == 18, "Dennis Tan"),
         MP = replace(MP,  MP == 31, "Gerald Giam"),
         MP = replace(MP,  MP == 35, "He Ting Ru"),
         MP = replace(MP,  MP == 40, "Jamus Lim"),
         MP = replace(MP,  MP == 52, "Leon Perera"),
         MP = replace(MP,  MP == 65, "Muhamad Faisal"),
         MP = replace(MP,  MP == 74, "Pritam Singh"),
         MP = replace(MP,  MP == 87, "Sylvia Lim"))

final_dataset2 #View the updated data set       

#Export the final data set as CSV file (will be uploaded to Github)
write.csv(final_dataset2,"C:/Users/ng hoi yu/Desktop/Google Capstone/final_dataset2.csv", row.names = FALSE)

#Read the data set back to R from Github
final_dataset2 <- 
  read.csv(file = 'https://raw.githubusercontent.com/nghoiyu/Google_Data_Analytics_Cert_Capstone/main/final_dataset2.csv')

#The data set is ready, we can go to the analysis stage

# Answering the first question
# Count the total number of oral questions asked by WP MPs

count(final_dataset2, "MP")

#Create a dataframe on the number of oral parliamentary questions that a WP MP asked
no_question <- as.data.frame(table(final_dataset2$MP))
no_question <- no_question %>% rename(MP = Var1) #rename Var1 to MP
no_question <- no_question %>% rename(no_of_q = Freq) #rename Freq to number of questions
no_question <- arrange(no_question, desc(no_of_q)) #arrange the data by number of questions asked in descending order
no_question

#Plot a bar chart of the number of oral parliamentary questions that a WP MP asked
no_question_bar <- ggplot(no_question, aes(x = reorder(MP, no_of_q), y = no_of_q)) + 
  geom_bar(stat="identity") + coord_flip() + #stat="identity" is needed as we are plotting count data
  ggtitle("Number of oral question titles of each WP MP") +
  xlab("MP") + ylab("number of questions")
no_question_bar

# Answering the second question
# The first step is to convert the data set into a quanteda corpus.

mp_corp <- corpus(final_dataset2, text_field = "question_titles") 
#text field is the column which contains the text to be analysed

#The only document-level variable in our corpus is the name of the WP MPs.
head(docvars(mp_corp))

# The second step is to construct a token object by tokenising the text field of the corpus
toks_mp_corp <- tokens(mp_corp, remove_punct = TRUE) # punctuations are also removed

##The third step is to remove tokens that are not helpful in our analysis.
#Remove the stopwords from the corpus
toks_mp_corp <- tokens_select(toks_mp_corp, pattern = stopwords("en"), selection = "remove")

#Remove the word "Singapore" and other similar terms from the corpus as they are not very helpful
toks_mp_corp <- tokens_select(toks_mp_corp, 
                              pattern = c("Singapore", "singapore's", "singaporeans", "s"), selection = "remove")

#The fourth step is to construct a document-feature matrix from the corpus
dfm_mp_corp <- 
  dfm(toks_mp_corp, remove_numbers = TRUE, remove_symbols = TRUE) #also remove numbers and symbols from the text

#The fifth step is to conduct a simple frequency analysis 

#We now calculate the most frequent words used in the question titles
freq_mp_corp <- textstat_frequency(dfm_mp_corp, n = 20)
head(freq_mp_corp, 20)

#Visualise the frequency of the top 15 words in the document feature matrix
top_15_words <- dfm_mp_corp %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "Word", y = "Word Frequency")
top_15_words

#In this step we apply the Policy-Agenda dictionary to count the policy topics appeared in the question titles
load("policy_agendas_english.RData") #load the dictionary, remember to save the dictionary in the working directory
policyagendas.lexicon <- dictionary(dictLexic2Topics)
View(policyagendas.lexicon) #view the dictionary

#Use the tokens_lookup function of quanteda to match and combine the corpus with the dictionary
toks_mp_corp_dict <- tokens_lookup(toks_mp_corp, dictionary = policyagendas.lexicon)

# create a document-feature matrix and group it by MP
dfm_dict <- dfm(toks_mp_corp_dict) %>% 
  dfm_group(groups = MP)

#View the document feature matrix
print(dfm_dict)

#Convert the document feature matrix to a dataframe using convert()
df_dfm_dict <- convert(dfm_dict, to = "data.frame")

#View all columns' names
colnames(df_dfm_dict)

#Rename doc_id to MP
df_dfm_dict <- df_dfm_dict %>% rename(MP = doc_id)

#Rename government_ops to government_operation
df_dfm_dict <- df_dfm_dict %>% rename(government_operation = government_ops)

#Remove the irrelevant policy topics. They are either irrelevant to Singapore or have zero count.
df_dfm_dict <- df_dfm_dict %>% select(-sstc, -prov_local, -aboriginal, -agriculture, 
                      -forestry, -fisheries, -"land-water-management",
                      -intergovernmental, -constitutional_natl_unity)

#Create a new dataframe to count the frequency of policy topics appeared in WP MPs' parliamentary questions
df_dfm_dict_freq <- data.frame(macroeconomics = sum(df_dfm_dict$macroeconomics), 
                               civil_rights = sum(df_dfm_dict$civil_rights),
                              healthcare = sum(df_dfm_dict$healthcare),
                              labour = sum(df_dfm_dict$labour),
                              immigration = sum(df_dfm_dict$immigration),
                              education = sum(df_dfm_dict$education),
                              environment = sum(df_dfm_dict$environment),
                              energy = sum(df_dfm_dict$energy),
                              transportation = sum(df_dfm_dict$transportation),
                              crime = sum(df_dfm_dict$crime),
                              social_welfare = sum(df_dfm_dict$social_welfare),
                              housing = sum(df_dfm_dict$housing),
                              finance = sum(df_dfm_dict$finance),
                              defence = sum(df_dfm_dict$defence),
                              foreign_trade = sum(df_dfm_dict$foreign_trade),
                              intl_affairs = sum(df_dfm_dict$intl_affairs),
                              government_operations = sum(df_dfm_dict$government_operation),
                              culture = sum(df_dfm_dict$culture), 
                              religion = sum(df_dfm_dict$religion))

#Transform the new dataframe from wide to long format so that a bar plot can be plotted
data_long <- gather(df_dfm_dict_freq, policy_topic, frequency, macroeconomics:religion, factor_key=TRUE)

#Plot a bar chart to show the frequency of policy topics among the parliamentary question titles
ggplot(data = data_long, aes(x = reorder(policy_topic, frequency), y = frequency)) + 
  geom_bar(stat="identity") + ggtitle("Topic distribution of WP MPs' parliamentary questions titles") +
  xlab("policy topic") + coord_flip()

#Plot a stacked bar chart to show the relative importance of each policy topic among the parliamentary questions asked by each WP's MP 

#A new dataframe is created by selecting nine more important policy topics
df_dfm_dict2 <- df_dfm_dict %>%
  select(MP, macroeconomics, finance, transportation, housing, immigration, labour, healthcare, education, civil_rights) %>%
  gather(macroeconomics:civil_rights, key = "Topic", value = "Share") %>% 
  group_by(MP) %>% 
  mutate(Share = Share/sum(Share)) %>% 
  mutate(Topic = as_factor(Topic))

#Plot a stacked bar chart
ggplot(df_dfm_dict2, aes(MP, Share, colour = Topic, fill = Topic)) +
  geom_bar(stat="identity") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Pastel1") +
  xlab("") + ylab("Share of topics [%]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Topics in the parliamentary questions titles on the Policy-Agendas dictionary") 

#################################################################################
# The End
#################################################################################