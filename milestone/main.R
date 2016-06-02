## File: data_formating.R
## Description: Manipulating the input data to the make analysis ready format.


library(tm) # Textmining package
library(SnowballC) # For stemming words 
library(dplyr) ## For sorting the word freqency
library(ggplot2) ## For ploting
library(RWeka) ## For adding ngram tokenization

if(!(dir.exists('DS_Capstone'))){
    source('setup.R')
}

set.seed(270891)

en_files <- list.files('./data/raw/', full.names = T)

for (i in en_files){
    con <- file(i,'r')
    filename <- unlist(strsplit(i, '[/]'))[4]
    name <- unlist(strsplit(filename, '[.]'))[2]
    temp <- readLines(con)
    len <- length(temp)
    temp1 <- temp[as.logical(rbinom(len, 1, .10))] ## Sampling only 10% of the records
    assign(name, temp1)
    rm(temp, temp1) ## To save system memory
    close(con)
}

## Writing the sample data for future reference
write(news, './data/sample/news.txt')
write(blogs, './data/sample/blogs.txt')
write(twitter, './data/sample/twitter.txt')

## TDM Function

make_tdm <- function(x, ng=1, stopword = TRUE ){
    
    corp <- Corpus(VectorSource(x))
    
    corp <- tm_map(corp, tolower) # To transform all the char to lowercase
    corp <- tm_map(corp, removePunctuation) ## To remove punctuation
    corp <- tm_map(corp, function(x) gsub("[^[:alnum:]]", " ", x)) # To remove special chars
    corp <- tm_map(corp, removeNumbers) ## Numbers provides less value to predective text analysis
    corp <- tm_map(corp, removeWords, profanity) ## To remove swearwords

    if(stopword) {
        corp <- tm_map(corp, removeWords, stopwords()) ##default language is English
    }
    
    corp <- tm_map(corp, stemDocument) ## Stemming the document
    corp <- tm_map(corp, stripWhitespace) ## To stripwhite space
    corp <- tm_map(corp, PlainTextDocument) ## Converting to plaintext format
    
    ngt <- function(x) NGramTokenizer(x,control = Weka_control(min=ng, max=ng))
    tdm <- TermDocumentMatrix(corp, control = list(tokenize= ngt))
    
    return(tdm)
}

#We have the clean Corpse data file here. Now it can be converted into TermDocument TermDocumentMatrix to find the most popular words in the dataset.
news_tdm_unigram <- make_tdm(news)
blogs_tdm_unigram <- make_tdm(blogs)
twitter_tdm_unigram <- make_tdm(twitter)

## Simple analysis

findFreqTerms(tdm_news, 300)
findAssocs(tdm_news, "said", 0.1)
tdm_news_popular <- removeSparseTerms(tdm_news, .05)

## Frequency table

freq_table <- function(tdm){
    temp <- row_sums(tdm)
    df <- as.data.frame(temp)
    df$Words <- row.names(df)
    names(df) <- c('Frequency','Words')
    rownames(df) <- NULL
    df <- df %>% transform(Words = reorder(Words, Frequency)) %>%
        arrange(desc(Frequency))
    rm(temp)
    return(df)
}

## Frequency table for unigram model

news_uni_freq <- freq_table(news_tdm_unigram)
blogs_uni_freq <- freq_table(blogs_tdm_unigram)
twitter_uni_freq <- freq_table(twitter_tdm_unigram)

## Ploting Unigram model

nu_plot <- ggplot(news_uni_freq[1:15,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 15 News Unigram') +  xlab(NULL)

bu_plot <- ggplot(blogs_uni_freq[1:15,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 15 Blogs Unigram')  

tu_plot <- ggplot(twitter_uni_freq[1:15,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 15 Twitter Unigram') +  xlab(NULL) 

grid.arrange(bu_plot, nu_plot, tu_plot, ncol=3)

## Frequency table for di gram and tri gram 

news_2_freq <- freq_table(news_tdm_2gram)
blogs_2_freq <- freq_table(blogs_tdm_2gram)
twitter_2_freq <- freq_table(twitter_tdm_2gram)

news_3_freq <- freq_table(news_tdm_3gram)
blogs_3_freq <- freq_table(blogs_tdm_3gram)
twitter_3_freq <- freq_table(twitter_tdm_3gram)


## Ploting di and tri gram
n2_plot <- ggplot(news_2_freq[1:10,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 10 News Di-gram') +  xlab(NULL)

b2_plot <- ggplot(blogs_2_freq[1:10,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 10 Blogs Di-gram')  

t2_plot <- ggplot(twitter_2_freq[1:10,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 10 Twitter Di-gram') +  xlab(NULL) 

n3_plot <- ggplot(news_3_freq[1:10,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 10 News Tri-gram') +  xlab(NULL)

b3_plot <- ggplot(blogs_3_freq[1:10,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 10 Blogs Tri-gram')  

t3_plot <- ggplot(twitter_3_freq[1:10,], aes(Words, Frequency)) + 
    geom_bar(stat = 'identity') + coord_flip() +
    ggtitle('Top 10 Twitter Tri-gram') +  xlab(NULL) 

grid.arrange(b2_plot, n2_plot, t2_plot, b3_plot, n3_plot, t3_plot, ncol=3)