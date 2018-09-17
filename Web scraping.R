#load packages
library(xml2)
library(rvest)
library(dplyr)
library(purrr)

#compiling data

urll="https://www.ratemds.com/best-doctors/ct/?specialty=dentist&page=%d"

url="https://www.ratemds.com/best-doctors/ct/?specialty=dentist&page=2"

allurl<-url%>%read_html()%>%html_nodes(".search-item-doctor-link")%>%html_attr("href")%>%
  .[!duplicated(.)]

allurl[11:20]<-url%>%read_html()%>%html_nodes(".search-item-doctor-link")%>%html_attr("href")%>%
  .[!duplicated(.)]

allurl<-paste("https://www.ratemds.com",sep = "",allurl)

#%>%lapply(function(x) read_html(x)%>%html_nodes("body"))%>%  
#  Map(function(x,y) write_html(x,tempfile(y,fileext=".txt"),options="format"),.,
#     c(paste("tmp",1:length(.))))

link<-read_html("https://www.ratemds.com/doctor-ratings/113413/Dr-Leonard-Kundel-STAMFORD-CT.html")
link%>%html_nodes(".rating-comment-body span")%>%html_text()

x<-DF
x$Gender[i]=page%>%html_nodes(".search-item-info:nth-child(1) div")%>%html_text()

#compiling Gender
for (i in 1:length(allurl))
{
  page<-read_html(allurl[i])
  
  x$Ranking[i]=page%>%html_nodes(".col-sm-6 .search-item-info~ .search-item-info+ .search-item-info div")%>%html_text()
  
}

#compiling ranking with in speciality
for (i in 1:length(allurl))
{
  page<-read_html(allurl[i])
     
  x$Ranking[i]=page%>%html_nodes(".col-sm-6 .search-item-info~ .search-item-info+ .search-item-info div")%>%html_text()
  
  }

#compiling no of reviews

for (i in 1:length(allurl))
{
  page<-read_html(allurl[i])
  
  x$no_of_reviews[i]=page%>%html_nodes(".star-rating-count span span")%>%html_text()
  
  
}


DF<-map_df(1:2,function(i){
  
  page<-read_html(sprintf(urll,i))
  data.frame(Doctor=html_text(html_nodes(page,".search-item-doctor-link")))
})

z<-print.data.frame(data.frame(allurl),quote = F)
class(z)
z<-as.vector(as.matrix(z))

z<-paste(z,sep = "","?page=%d")

#compiling text reviews

for (i in 1:length(z)){
url2=z[i]

x$text_review[i]<-paste(unlist(map_df(1:x[i,5],function(r){
  
  page<-read_html(sprintf(url2,r))
  data.frame(Text=html_text(html_nodes(page,".rating-comment-body span")))
})),collapse =" ")
}

#compiling individual ratings

for (i in 1:length(z)){
  url2=z[i]
  
  x$ratings[i]<-paste(unlist(map_df(1:x[i,5],function(r){
    
    page<-read_html(sprintf(url2,r))
    data.frame(Text=html_text(html_nodes(page,".value")))
  })),collapse =" ")
}


#Compiling image data
for (i in 1:length(allurl))
{
  page<-read_html(allurl[i])
  
  x$image[i]=page%>%html_nodes(".doctor-profile-image")%>%html_attr("src")
  
}


# x<-x[,1:5]


#link%>%html_nodes(".value")%>%html_text()
#.value
#.rating-comment-body


#url2%>%read_html()%>%html_nodes(".rating-comment-body span")%>%html_text()
#paste(unlist(y), collapse =" ")


#calculations
x$no_of_reviews<-as.numeric(x$no_of_reviews)

x$calc<-ceiling(x$no_of_reviews/10)


#.modal-body div span:nth-child(1)
#link%>%html_nodes(".doctor-profile-image")%>%html_attr("src")


