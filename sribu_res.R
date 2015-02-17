install.packages("XML"); library(XML);
install.packages("dplyr");library(dplyr);

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

# change variables
sribu$title <- with(sribu, as.character(title))

# inspect dataframe
str(sribu)
lapply(sribu, summary)

# inspect prize NA
sribu %>% filter(is.na(prize))
# turns out the actual prize is IDR 20.000.000 or USD 2.000
sribu$prize[is.na(sribu$prize)] <- 2000

# inspect brief NA
sribu %>% filter(is.na(brief))
sribu %>% filter(is.na(brief) & terms.confidential==0 & terms.private == 0)
# assume for now that these entries are all confidential
sribu[sribu$terms.confidential==0 & sribu$terms.private==0 & is.na(sribu$brief),]$terms.confidential <- 1
# plot brief against cat, prize, and terms
pairs(sribu %>% select(-title))
# library(ggplot2)
# ggplot(sribu) + geom_boxplot(aes(x=cat, y=brief))
# ggplot(sribu) + geom_point(aes(x=prize, y=brief))

# impute NA brief using kmeans; k=5
sribu.no.brief <- sribu %>% select(-c(title,brief,entries.num))
sribu.no.brief$cat <- ifelse(sribu.no.brief$cat=="",0,1)
mat.sribu <- apply(as.matrix(sribu.no.brief),2,scale)

test.sribu <- cbind(sribu,kmeans(x = mat.sribu, centers = 5)$cluster)
names(test.sribu)[10] <- "cluster"

# assign brief according to cluster
sribu.brief.no.na <- test.sribu %>% 
    filter(!is.na(brief)) %>% 
    select(cluster, brief) %>% 
    group_by(cluster) %>% 
    summarise(mean(brief))
names(sribu.brief.no.na)[2] <- "mean.brief"
sribu.fin <- merge(sribu.brief.no.na, test.sribu, by="cluster")
sribu.fin$brief <- ifelse(is.na(sribu.fin$brief),sribu.fin$mean.brief,sribu.fin$brief)

# predicting entries.num based on features. set train and test dataframe
set.seed(123)
train.split <- sample(1:nrow(sribu.fin),0.5*nrow(sribu.fin))
train <- sribu.fin[train.split,]
test <- sribu.fin[-train.split,]
# check distribution of target variables
hist(train$entries.num)
hist(test$entries.num)

# fit
model.sribu.train <- lm(entries.num~.,data=train %>% select(-c(title,cluster,mean.brief)))
# assess
summary(model.sribu.train)
# backward selection based on p-values
# remove terms.guarantee
model.sribu.train.1 <- lm(entries.num~.,data=train %>% select(-c(title,cluster,mean.brief,terms.guarantee)))
summary(model.sribu.train.1)
# remove terms.private
model.sribu.train.2 <- lm(entries.num~.,data=train %>% select(-c(title,cluster,mean.brief,terms.guarantee,
                                                                 terms.private)))
summary(model.sribu.train.2)
# remove terms.extend
model.sribu.train.3 <- lm(entries.num~.,data=train %>% select(-c(title,cluster,mean.brief,terms.guarantee,
                                                                 terms.private,terms.extend)))
summary(model.sribu.train.3)

# results
# original model
sribu.train.pred <- predict.lm(model.sribu.train,test %>% select(-c(title,cluster,mean.brief)))
# parsimonious model
sribu.train.pred.2 <- predict.lm(model.sribu.train.3,test %>% select(-c(title,cluster,mean.brief)))

# try remove 1 point extreme outlier
# remove 1 point Mobile Apps Design
sribu.fin.no.out <- sribu.fin[-c(which(sribu.fin$entries.num==1476),which(sribu.fin$cat=="Mobile Apps Design")),]
set.seed(123)
train.split.2 <- sample(1:nrow(sribu.fin.no.out),0.5*nrow(sribu.fin.no.out))
train.2 <- sribu.fin.no.out[train.split.2,]
test.2 <- sribu.fin.no.out[-train.split.2,]

model.sribu.train.no.out <- lm(entries.num~.,data=train.2 %>% select(-c(title,cluster,mean.brief)))
summary(model.sribu.train.no.out)
model.sribu.train.no.out.1 <- lm(entries.num~.,data=train.2 %>% select(-c(title,cluster,mean.brief,terms.guarantee)))
summary(model.sribu.train.no.out.1)
model.sribu.train.no.out.2 <- lm(entries.num~.,data=train.2 %>% select(-c(title,cluster,
                                                                          mean.brief,terms.guarantee,terms.private)))
summary(model.sribu.train.no.out.2)

# plot results
col.m <- colorRampPalette(c("orange","yellow","green"))
par(las=2,mar = c(5,9,2,2))
barplot(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = F),
        cex.names = 0.7,axisnames = T,
        names.arg = gsub("terms.","",gsub("cat","",names(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = F)))),
        horiz=T,
        col=col.m(24),
        xlim=c(-200,150),
        xlab="Impact towards # of entries",
        main="Sribu: factors that contribute to # of entries")
grid(nx = 24, ny = 24, col = "#808080", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[1],digits = 1),x=90,y=27.1,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[2],digits = 1),x=60,y=25.8,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[3],digits = 1),x=55,y=24.5,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[4],digits = 1),x=53,y=23.4,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[5],digits = 1),x=40,y=22.1,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[6],digits = 1),x=20,y=21,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[7],digits = 1),x=0,y=19.7,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[8],digits = 1),x=-3,y=18.6,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[9],digits = 1),x=-30,y=17.4,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[10],digits = 1),x=-40,y=16.2,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[11],digits = 1),x=-50,y=15.0,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[12],digits = 1),x=-60,y=13.8,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[13],digits = 1),x=-70,y=12.6,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[14],digits = 1),x=-80,y=11.5,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[15],digits = 1),x=-90,y=10.4,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[16],digits = 1),x=-100,y=9.1,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[17],digits = 1),x=-110,y=7.9,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[18],digits = 1),x=-120,y=6.7,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[19],digits = 1),x=-140,y=5.5,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[20],digits = 1),x=-150,y=4.3,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[21],digits = 1),x=-200,y=3.2,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[22],digits = 1),x=-210,y=1.9,cex = 0.7, font=2, pos = 4)
text(labels = round(sort(model.sribu.train.no.out.2$coefficients[-1],decreasing = T)[23],digits = 1),x=-210,y=0.7,cex = 0.7, font=2, pos = 4)





