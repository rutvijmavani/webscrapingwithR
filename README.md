# webscrapingwithR
In this project, I will build a specialized R program to crawl, parse and extract useful information from online websites. 
I selected Abstract and Applied Analysis journal's website (https://www.hindawi.com/journals/aaa/contents/year/2021/) for scraping.
Given an input year, the objective is to extract all articles published in/after that year from this journal. As a start point, I will extract the following 8 fields for each article:Title, Authors, Author Affiliations, Correspondence Author, Publish Date, Abstract, Keywords, Full Paper (Text format).
Given an input year, the program is expected to crawl the journal’s website automatically, and parse and extract useful fields for each crawled article. The program is expected to store the extracted information into a plain file elegantly. (One column for one field.)
