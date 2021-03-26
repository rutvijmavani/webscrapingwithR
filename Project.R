dataCrawl <- function(year){
  library(bitops)
  library(RCurl)
  library(XML)
  library(stringr)
  library(xlsx)
  
  site.url = "https://www.hindawi.com/journals/aaa/"
  
  j<-1
  articlepubDate <-vector()
  articleTitle <-vector()
  articleAbstract <-vector()
  articleFull <-vector()
  articleAffiliation<-vector()
  articleAuthor<-vector()
  articleCorresAuthor<-vector()
  if (year %in% seq(1996, 2021, by=1)){
    for(year in seq(year,2021,by=1)){
      URL = paste(site.url, "contents/", "year/", year, sep="")
      main.page = readLines(paste(URL, sep=""))
      options(warn=-1)
      doc = htmlParse(main.page, asText=TRUE)
      article.list <- xpathSApply(doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div", xmlValue)
      article.id.index <- gregexpr("Article ID ([0-9]+)", article.list)
      article.id = regmatches(article.list, article.id.index)
      print (article.id)
      for(id in article.id[[1]]){
        only.id <- regmatches(id,gregexpr("([0-9]+)", id))
        print (only.id)
        article.url <- paste(site.url, year, "/", paste(unlist(only.id), collapse = ""), "/", sep="")
        article.page.content <- readLines(article.url)
        atricle.doc <- htmlParse(article.page.content, asText=TRUE)
        article.title <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[1]/h1",xmlValue)
        print ("ARTICLE TITLE:")
        print (article.title)
        articleTitle[j]<- c(article.title)
        article.pub.date <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[3]/div[2]/div[3]/span[2]/text()")[[1]]
        print ("ARTICLE PUBLISHED DATE:")
        print (article.pub.date)
        articlepubDate[j]<-c(article.pub.date)
        article.abstract <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/article/div/div[1]/p[1]",xmlValue)
        print ("ARTICLE ABSTRACT:")
        print (article.abstract)
        articleAbstract[j]<-c(article.abstract)
        article.full.article <- xpathSApply(atricle.doc, "/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div[1]/div/div/article/div",xmlValue)
        print ("ARTICLE FULL LINK:")
        print (article.full.article)
        articleFull[j] <- c(article.full.article)
        article.auth.plus.affiliation <- xpathSApply(atricle.doc,"/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[2]", xmlValue)
        article.auth.affiliation <- xpathSApply(atricle.doc,"/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[2]/div", xmlValue)
        article.author <- str_remove_all(strsplit(article.auth.plus.affiliation,article.auth.affiliation,fixed = T)[[1]][1], "[0-9]")
        print ("ARTICLE's AUTHORS:")
        print (article.author)
        articleAuthor[j]<-c(article.author)
        print ("ARTICLE's AUTHOR's AFFILIATIONS:")
        print (article.auth.affiliation)
        articleAffiliation[j]<-c(article.auth.affiliation)
        article.corresponding.auth <- xpathSApply(atricle.doc,"/html/body/div[1]/div/div/main/div/div[3]/div[2]/div/div/div/div/div[2]/span/b/text()", xmlValue)
        print ("ARTICLE's CORRESPONDING AUTHOR:")
        print (article.corresponding.auth)
        articleCorresAuthor[j] <- c(article.corresponding.auth)
        print ("------------------------------------------------------------------------------------")
        j<-j+1
        
      }
      ArticlePubDate <- sapply(articlepubDate,xmlValue)
  #    ArticleFullArticle <-sapply(articleFull,xmlValue)
      res = data.frame(articleTitle,ArticlePubDate,articleAbstract,articleFull,articleCorresAuthor,articleAuthor,articleAffiliation)
      year<-year+1
      write.xlsx(res,"D:/google downloads/output.xlsx")
    }
  }else{
    print("Please enter a valid year...the articles are available from 1996 to 2021.")
  }
}

dataCrawl(2021)


