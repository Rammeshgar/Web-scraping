

library(rvest)
library(data.table)
library(xml2)
library(dplyr)
library(ggplot2)
library(ggpubr)

##### 1 function for one link

get_kavasaki_detail <- function(url) {
  
  motorcycle <- read_html(url)
  
  data_list<-list()
  
  data_list[["url"]]<-url
  
  key<- motorcycle %>% 
    html_nodes(".spec-key.bold") %>% 
    html_text()
  
  value<- motorcycle %>% 
    html_nodes(".vs-specs-table-row .spec-value") %>%
    html_text()
  
  for (i in 1:length(key)) {
    data_list[[key[i]]]<-trimws(value[i])
  }
  df<-data.frame(data_list)
  return(df)
}

check<-get_kavasaki_detail("https://www.motorcycle.com/specs/kawasaki/on-off-road/2009/klr-tm/650/detail.html")


#-------------------------------------------------------------------------------
##### 2 collect all links

###one page

first<- read_html("https://www.motorcycle.com/specs/kawasaki.html?page_num=1")

inner_pages<-first %>% html_nodes(".card-link") %>% html_attr("href")

my_links<- paste0("https://www.motorcycle.com",inner_pages)

###all pages

all_links<-c()
for (i in 1:42) {
  t<- read_html(paste0("https://www.motorcycle.com/specs/kawasaki.html?page_num=",i))
  my_links<-t %>% html_nodes(".card-link") %>% html_attr("href")
  all_links<-c(all_links,my_links)
}

final_links<-paste0("https://www.motorcycle.com",all_links)

saveRDS(final_links,"all_links.rds")

##### 3 create data frame

my_list<- lapply(final_links,get_kavasaki_detail)
kawasaki_df<-rbindlist(my_list,fill = T)

names(kawasaki_df)[11]<-"Name"
names(kawasaki_df)[3]<-"Price"
names(kawasaki_df)[2]<-"Type"

kawasaki_df$Price<-gsub("\\$","",kawasaki_df$Price)
kawasaki_df$Price<-gsub("\\,","",kawasaki_df$Price)

kawasaki_df$Price<- as.numeric(as.character(kawasaki_df$Price))
kawasaki_df$Warranty<- as.numeric(as.character(kawasaki_df$Warranty))
print(sapply(kawasaki_df[3], class))

#kawasaki_df = subset(kawasaki_df, select = -c(Insurance,Finance) )
str(kawasaki_df)

#write.csv(kawasaki_df, "c:\\Users/admin/OneDrive/Documents/Rcodes/web scraping.csv",row.names = T)

#write.xlsx2(kawasaki_df, file = "myworkbook.xlsx", sheetName = "kawasaki data",
            ##col.names = TRUE, row.names = TRUE, append = FALSE)

# ------------------------------------------------------------------------------

##### 4 analyze and visualize data 
#kawasaki_df<-readRDS("kawasaaki.rds")


Top_fifty<-
  kawasaki_df %>% 
  arrange(-Price) %>% 
  head(50)


ggplot(Top_fifty, aes(Name, Price, color=Price))+
  geom_jitter()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 35,hjust = 1),
        axis.text.y = element_text(angle = 0,hjust = 1),
        axis.title.x = element_text(colour = "navy"), 
        axis.title.y = element_text(colour = "navy"))+
  scale_color_gradient(low = "pink2",high = "purple4")+
  labs(title = "Top Fifty The Most Expensive", 
       subtitle = "Theme=theme_Classic, plot=geom_point, price= in Dollor"
       , x= "Names", y= "Prices")  


types<-
  kawasaki_df %>% 
  arrange(Type)

ggplot(types,aes(Type, y = Price))+
  geom_boxplot()+
  theme_get()+
  theme(axis.text.x = element_text(angle = 35,hjust = 1, color = "orange3"),
        axis.text.y = element_text(angle = 0,hjust = 1, color = "black"),
        axis.title.x = element_text(colour = "navy"), 
        axis.title.y = element_text(colour = "navy"))+
  labs(title = "Average Prices For Different Categories", 
       subtitle = "Theme=theme_get, plot=geom_boxplot, price= in Dollor"
       , x= "Types", y= "Price")



least_heavy<-
  kawasaki_df %>% 
  arrange(Torque..Ft.Lbs.Nm.) %>% 
  head(20)

ggplot(least_heavy,aes(Name, Torque..Ft.Lbs.Nm., color= Torque..Ft.Lbs.Nm.))+
  geom_hex()+
  facet_grid(~Name)+
  theme_get()+
  theme(axis.text.x = element_text(angle = 35,hjust = 1, color = "orange3"),
        axis.text.y = element_text(angle = 35,hjust = 1, color = "black"),
        axis.title.x = element_text(colour = "navy"), 
        axis.title.y = element_text(colour = "navy"))+
  labs(title = "Average Prices For Different Categories", 
       subtitle = "Theme=theme_get, plot=geom_boxplot, price= in Dollor"
       , x= "Types", y= "Price")


p1<-ggplot(Top_fifty,aes(Warranty, Name, color = Price))+
  geom_count()+
  theme_update()+
  theme(axis.text.x = element_text(angle = 0,hjust = 1, color = "black"),
        axis.text.y = element_text(angle = 25,hjust = 1, color = "pink4"),
        axis.title.x = element_text(colour = "navy"), 
        axis.title.y = element_text(colour = "navy"))+
  scale_color_gradient(low = "skyblue",high = "purple4")+
  guides(fill=F)+
  labs(title = "Fifty Motorcycles with Highest Price by Warranty", 
       subtitle = "Theme=theme_update, plot=geom_count, 
       price= in Dollor, Warranty= in Month"
       , x= "Warranty", y= "Name")
  
Warranty<-
   kawasaki_df %>% 
   arrange(-Warranty) %>% 
   head(50)
  
p2<-ggplot(Warranty,aes(Price, Name, color = Price))+
  geom_count()+
  theme_update()+
  theme(axis.text.x = element_text(angle = 0,hjust = 1, color = "purple"),
        axis.text.y = element_text(angle = 25,hjust = 1, color = "cyan4"),
        axis.title.x = element_text(colour = "brown"), 
        axis.title.y = element_text(colour = "brown"))+
  scale_color_gradient(low = "orange",high = "red3")+
  guides(fill=FALSE)+
  labs(title = "Fifty Motorcycles with Highest Warranty(36) by Price", 
       subtitle = "Theme=theme_update, plot=geom_count, 
       price= in Dollor, Warranty= in Month"
       , x= "Price", y= "Name")

ggarrange(p1,p2,nrow = 2)

# make RDS file
saveRDS(kawasaki_df,'kawasaaki.rds')
 




  
  
