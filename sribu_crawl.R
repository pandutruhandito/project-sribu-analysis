install.packages("XML"); library(XML);

# get first page
html <- htmlTreeParse("http://www.sribu.com/id/contests?page=1&sort_by=all",useInternalNodes = T)
i <- 1

# set variables
title <- c()
terms.confidential <- c()
terms.private <- c()
terms.guarantee <- c()
terms.extend <- c()
prize <- c()
brief <- c()
cat <- c()
entries.num <- c()

# loop
while (xpathSApply(html, "boolean(//tr[@class='content'])")) {
    print(i)
    #get title
    title <- c(title, xpathSApply(html, "//div[@class='contest-title']/a/text()", xmlValue)) 
    
    # terms
    terms <- list()
    for (k in 1:xpathSApply(html, "count(//tr[@class='content'])")) {
        temp.term <- xpathSApply(html, paste0("//tr[@class='content'][",k,"]//td[2]//img/@alt"))
        if (is.null(temp.term)) {
            temp.term <- ""
        }
        terms[[k]] <- temp.term
    }
    terms.confidential <- c(terms.confidential, sapply(terms,function(x) {sum(grepl("promo|confidential",tolower(x)))}))
    terms.private <- c(terms.private, sapply(terms,function(x) {sum(grepl("private",tolower(x)))}))
    terms.guarantee <- c(terms.guarantee, sapply(terms,function(x) {sum(grepl("guarantee",tolower(x)))}))
    terms.extend <- c(terms.extend, sapply(terms,function(x) {sum(grepl("extend",tolower(x)))}))
    
    # prize
    prize <- c(prize, xpathSApply(html, "//tr[@class='content']//td[6]/span[@class='usd']/text()", xmlValue))
    prize <- as.numeric(sub("\\$","",sub("\\)","",sub("\\(","",prize))))
    
    # url
    url <- xpathSApply(html, "//tr[@class='content']//td[2]/a/@href")
    url <- paste0("http://www.sribu.com",url)
    
    # category & brief length in words
    for (j in 1:length(url)) {
        if(grepl("overview",url[j])) { url[j] <- sub("overview","?tab=gallery",url[j]) }
        html.page <- htmlTreeParse(url[j], useInternalNodes = T)
        temp <- xpathSApply(html.page, "//div[@class='contest-info-bar-category']/p[2]/text()",xmlValue)
        cat <- c(cat,ifelse(is.null(temp),"",temp))    
        text <- xpathSApply(htmlTreeParse(sub("=gallery","=description",url[j],fixed=T), useInternalNodes = T), 
                            "//div[@class='right']//text()", xmlValue)
        brief <- c(brief,sum(nchar(text)-nchar(gsub(" ", "", text))))
    }
    # assign NA to brief if 0
    brief <- ifelse(brief==0,NA,brief)
    
    # entries num
    entries.num <- c(entries.num, xpathSApply(html, "//tr[@class='content']//td[5]/span[@class='entries']/text()", xmlValue))
    entries.num <- as.numeric(entries.num)
    
    # get next page
    i <- i + 1
    html <- htmlTreeParse(paste0("http://www.sribu.com/id/contests?page=",i,"&sort_by=all"),useInternalNodes = T)
}

# combine into dataframe
sribu <- data.frame(title=title, terms.confidential=terms.confidential, terms.extend=terms.extend, terms.guarantee=terms.guarantee,
                    terms.private=terms.private, prize=prize, brief=brief, cat=cat, entries.num=entries.num)