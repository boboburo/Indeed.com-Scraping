#
#
#
library(rvest)

city = "Dublin"
job = 'Quantitative Analyst'

city<-sub(" ","+",city)
job <- sub(" ", "+", job)

finalSearchString<-paste0("http://ie.indeed.com/jobs?q=",job,"&l=",city)
indeedSearchResults <- read_html(finalSearchString)

#Get number of results eg. Jobs 1 to 10 of 155 
#Results 1 to 10 of 155
numJobs<-indeedSearchResults %>%
  html_nodes("#searchCount") %>%
  html_text()

#remove any commas from numJobs (for 1000's) and parse the numbers
numJobs<-gsub(",","",numJobs)

library(stringr)
#returns list of lists, only want top one.
numJobsList <- as.numeric(str_extract_all(numJobs,"\\(?[0-9]+\\)?")[[1]])

#Loop through the results and capture the job ad links in a data.table
library(data.table)
linkTable <- data.table(pageNum= integer(0), typeJob= character(0), linkAd = character(0))

for(i in 0:as.integer(numJobsList[3]/10))
{
  
  #Create the results page
  pageNo<-as.character(i*10)
  htmlString<-paste0(finalSearchString,"&start=",pageNo)
  print(htmlString)
  
  #start reading in the results
  resultsPage<-read_html(htmlString)
  
  #Extract the sponsoredJobs and the organicJobs
  sponsoredJobs <- html_nodes(resultsPage, css = "#resultsCol [data-tn-section = sponsoredJobs] a")%>%
    html_attr("href") 
  organicJob <- html_nodes(resultsPage, css = "#resultsCol [data-tn-component = organicJob] a")%>%
    html_attr("href")
  
  #Many links captured. Only those with "clk" are the job click link. Filter these.
  sponsoredJobs<-grep('clk', sponsoredJobs, value=TRUE)  
  organicJob<-grep('clk', organicJob, value=TRUE)  
  
  #Add the start of the URL "http://ie.indeed.com" to the links.
  sponsoredJobs<-lapply("http://ie.indeed.com",paste0,sponsoredJobs ,USE.NAMES = F)#add ie.indeed.com
  organicJob<-lapply("http://ie.indeed.com",paste0,organicJob ,USE.NAMES = F) #add ie.indeed.com
  
  #Store the results in a data table
  linkTableTemp<-data.table(pageNum=i*10,typeJob="sponsored",linkAd=sponsoredJobs[[1]])
  linkTable<-rbind(linkTable,linkTableTemp)
    linkTableTemp<-data.table(pageNum=i*10,typeJob="organic",linkAd=organicJob[[1]])
  linkTable<-rbind(linkTable,linkTableTemp)
  
  
  
}


#open one link and extract text
temp<-linkTable[1,linkAd]
job <- read_html(temp)

linkTable2<-linkTable[,linkAd:=paste0(linkAd,'"')]
linkTable2<-linkTable2[,linkAd:=paste0('"',linkAd)]


job2<-job %>% html_text()


#use some to keep only english words
library(tm)

jobVector<-VectorSource(job2)
corpus<-Corpus(jobVector)


