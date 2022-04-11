# for scrapping webpages and working with xml
library(rvest)   
# for the tidy magic
library(dplyr)   
library(tidyr)
# for the string manipulations
library(stringr)
library(forcats)

library(ggplot2)
library(RColorBrewer)
app_id <- Sys.getenv("WOLFRAM_ID")
# Wiki table to data.frame
WikiTable <- function(url = 'https://en.wikipedia.org/wiki/Le_Monde%27s_100_Books_of_the_Century', 
                      table_number = 1) {
# read the page provided in the first argument
wiki_df <- read_html(url) %>% 
  # select only tables
  html_nodes(css = "table") %>% 
  # keep only the first table
  .[[table_number]] %>%
  # transform to the data.frame
  html_table() %>%
  # parse for the final year of publication
  mutate(YearFinal = as.integer(case_when(             
    nchar(Year) > 7 ~ str_sub(Year, -4, -1),         # 1899-1901 => 1901
    nchar(Year) > 4 ~ paste0(str_sub(Year, 1, 2),    # 1901-03 => 1903 
                             str_sub(Year, -2, -1)),
    TRUE ~ Year) ))                                  # 1901 => 1901

}

WolframSearch <- function(name, question, app_id) {
  # in case of error or warning return NA
  tryCatch( {
    # construct link
    wa_url <- paste0("http://api.wolframalpha.com/v2/query?input=", 
                 URLencode(paste(name, question)), "&appid=", app_id)
    # read responce as xml
    wa_cont <- xml2::read_xml(wa_url)
    # find plaintext nodes, select only second (where data is stored) 
    wa_data <- xml2::xml_find_all(wa_cont, xpath = './/plaintext')[[2]] %>% 
    # extract text  
    xml2::xml_text() %>% 
    # split by row  
    str_split(pattern = "\n", simplify = TRUE) %>% 
    # transpose to verital
    t() %>% 
    # form a tibble  
    as_tibble() %>%
    # call the only column 'author_data'  
    setNames("author_data") %>% 
    # separate by ' | ' (on columns) using regex magic  
    separate(author_data, 
           c("variable", "value"), 
           "\\s\\|\\s\\b") %>%
    pivot_wider(names_from = variable) },
    
    # return NA in case of error or warning
    warning = function(w) {return(NA)},     
    error = function(e) {return(NA)})
  }

#Vectorize WolframSearch function to use it in pipes.
WolframSearch_v <- Vectorize(WolframSearch)

# for each row in wiki table add relevant Wolfram info 
AddData <- function(df, question, app_id) {
  result <- df %>% 
  mutate(data = WolframSearch_v(Author, question, app_id))
}

result <- WikiTable() %>% 
  AddData("year of birth and sex", app_id) %>% 
  unnest(data) %>% 
  filter(!is.na(`year of birth`)) %>%
  mutate(age = YearFinal - as.integer(`year of birth`) - 0.5) %>% 
  select("Title", "Author", "Language", "Gender"="gender", "Age"="age") 


result$Language <- fct_infreq(result$Language)
  


ggplot(result, aes(x = Age)) +                           
  geom_histogram(aes(y = ..count..), color="darkblue", fill="lightblue", binwidth = 5) +
  geom_density(aes(y= ..density.. * nrow(result) * 5), alpha = 0.1, color="darkblue", fill="lightblue") +
  theme_classic() +
  labs(x = "Age", y = "Number of books",
       fill = NULL) 

ggplot(result, aes(x = Language, fill = Gender)) +                           
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust=1)) +
  labs(x = NULL, y = "Number of books",
       fill = NULL) +
  scale_fill_brewer(palette = "Set1")



 