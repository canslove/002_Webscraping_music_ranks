# ==============================================================
# title : Analysis of Top Rank songs among different streaming sites
# author: Donghyun Kang
# output: Rmarkdown
# date  : Feb/13/2018
# ==============================================================
# ## Youtube starts from 9/15, so all analysis should start from 9/15/17
#   
#   1) concat all table with another column "source" which means origination such as 
#      'spotify', 'youtube', 'billboard' and 'billboard_album' <br>
#   2) make rankinng matching ratio per weak (find same title and calculate ranking based on title or artist <br>
#   3) visualize grapth for ranking transition per artist/title comapare to each companies <br>
#===============================================================
# Inspiration :
#  ---------------------------------------------------------------
# ==============================================================

library(dplyr)
library(ggplot2)
library(ggthemes) 
#library(data.table)
#library(dygraphs) # install.packages("dygraphs")
library(reshape2)
library(plotly)

#library(mapproj)
#library(tidyr)
#library(RSQLite)
#library(addinslist)
#library(RColorBrewer) 
#library(vcd) 
#library(xlsx) 
#library(foreign) 

setwd("C:/Users/SAMSUNG/Documents/A.MyDoc/A.DriveE/DataScience/NYCDSA/40_Projects/C02.WebScraping/Github/Analysis")
rm(list=ls())

# Run Parameters -------------------
show_plot = 0
INIT = 1
N_RANK = 100

# Data Read/Modify/Write --------------------------------

#raw.dt <- fread("../Data/data.csv", stringsAsFactors = F)
#raw.df <- as.data.frame(raw.dt)
df_spotify = read.csv('./datasets/spotify_wkly.csv', stringsAsFactors=FALSE)
df_youtube = read.csv('./datasets/youtubemusic.csv', stringsAsFactors=FALSE, header = FALSE,
                      col.names = c('start_date','end_date','rank','title','artist','views'))
df_billboard = read.csv('./datasets/acharts_singles.csv', stringsAsFactors=FALSE)
df_billboard_albums = read.csv('./datasets/acharts_albums.csv', stringsAsFactors=FALSE)
df_itunes = read.csv('./datasets/itunes.csv', stringsAsFactors=FALSE)

# check data Nas --------------------------------
df_list = c('df_spotify', 'df_youtube', 'df_billboard', 'df_billboard_albums', 'df_itunes')
print('Nas in df_spotify : ', sum(is.na(df_spotify)))
print('Nas in df_youtube : ', sum(is.na(df_youtube)))
print('Nas in df_billboard : ', sum(is.na(df_billboard)))
print('Nas in df_billboard_albums : ', sum(is.na(df_billboard_albums)))
print('Nas in df_itunes : ', sum(is.na(df_itunes)))
which( rowSums( is.na(df_spotify) ) != 0 )

# column name check --------------------------------
colnames(df_spotify)
colnames(df_youtube)
colnames(df_billboard)
colnames(df_billboard_albums)
colnames(df_itunes)

# column name change --------------------------------
colnames(df_billboard) = gsub("weeks", "prev_rank", colnames(df_billboard))
colnames(df_billboard_albums) = gsub("weeks", "prev_rank", colnames(df_billboard_albums))
  
# column name tolower --------------------------------
#names(df) <- tolower(names(df))

# reorder columns --------------------------------
df_spotify <- df_spotify[c("start_date", "end_date", "rank","title", "artist", "streams")]
#df_youtube <- df_youtube[c("start_date", "end_date", "rank","title", "artist", "views")]
df_billboard <- df_billboard[c("date", "rank","title", "artist", "prev_rank")]
df_billboard_albums <- df_billboard_albums[c("date", "rank","title", "artist", "prev_rank")]
df_itunes <- df_itunes[c("date", "release_date", "title", "artist", "genre")]

# Summary --------------------------------
summary(df_spotify)
summary(df_youtube)
summary(df_billboard)
summary(df_billboard_albums)
summary(df_itunes)

# Change Variable Data Type ----------------------------------
df_spotify_org <- df_spotify
df_spotify <- df_spotify %>% 
  mutate(start_date = as.Date(start_date, "%Y-%m-%d")) %>%
  mutate(end_date = as.Date(end_date, "%Y-%m-%d")) %>%
  mutate(rank = as.integer(rank)) %>%
  mutate(streams = as.integer(streams))
which( rowSums( is.na(df_spotify) ) != 0 )
if(sum(is.na(df_spotify))!=0){
  df_spotify <- na.omit(df_spotify)
}
summary(df_spotify)

df_youtube_org <- df_youtube
df_youtube <- df_youtube %>% 
  mutate(start_date = as.Date(start_date, "%Y-%m-%d")) %>%
  mutate(end_date = as.Date(end_date, "%Y-%m-%d")) %>%
  mutate(views = as.numeric(gsub(pattern = "M", replacement = "", x = views, ignore.case = F, fixed = T))*1000000)
which( rowSums( is.na(df_youtube) ) != 0 )
if(sum(is.na(df_youtube))!=0){
  df_youtube <- na.omit(df_youtube)
}
summary(df_youtube)

df_billboard_org <- df_billboard
df_billboard <- df_billboard %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))
which( rowSums( is.na(df_billboard) ) != 0 )
if(sum(is.na(df_billboard))!=0){
  df_billboard <- na.omit(df_billboard)
}
summary(df_billboard)

df_billboard_albums_org <- df_billboard_albums
df_billboard_albums <- df_billboard_albums %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))
which( rowSums( is.na(df_billboard_albums) ) != 0 )
if(sum(is.na(df_billboard_albums))!=0){
  df_billboard_albums <- na.omit(df_billboard_albums)
}
summary(df_billboard_albums)

df_itunes_org <- df_itunes


# reorder rows --------------------------------
df_spotify <- arrange(df_spotify, start_date, rank)
df_youtube <- arrange(df_youtube, start_date, rank)
df_billboard <- arrange(df_billboard, date, rank)
df_billboard_albums <- arrange(df_billboard_albums, date, rank)


# Data Snooping --------------------------------
head(df_spotify,5)
head(df_youtube,5)
head(df_billboard,5)
head(df_billboard_albums,5)
head(df_itunes,5)

# Data trim to synchronize -----------------------------------
ViewPeriod = c("2017-09-15","2018-01-26")
df_spotify = subset(df_spotify, select = -c(end_date))
df_spotify <- df_spotify %>%
  filter(start_date >= as.Date(ViewPeriod[1]) & start_date <= as.Date(ViewPeriod[2])) %>%
  filter(rank <= 100)
df_youtube = subset(df_youtube, select = -c(end_date))
colnames(df_billboard)[1] <- "start_date"
df_billboard$start_date = df_billboard$start_date-1 # date sync with spotify and youtube
df_billboard <- df_billboard %>%
  filter(start_date >= as.Date(ViewPeriod[1]) & start_date <= as.Date(ViewPeriod[2])) %>%
  filter(rank <= 100)
colnames(df_billboard_albums)[1] <- "start_date" 
df_billboard_albums$start_date = df_billboard_albums$start_date-1 # date sync with spotify and youtube
df_billboard_albums <- df_billboard_albums %>%
  filter(start_date >= as.Date(ViewPeriod[1]) & start_date <= as.Date(ViewPeriod[2])) %>%
  filter(rank <= 100)

# New column for source identification
source = rep('spotify', length(df_spotify$rank))
df_spotify$source = source

source = rep('youtube', length(df_youtube$rank))
df_youtube$source = source

source = rep('billboard', length(df_billboard$rank))
df_billboard$source = source

source = rep('billboard_albums', length(df_billboard_albums$rank))
df_billboard_albums$source = source

# Data Cleaning # remove multibyte string --------------------------------
df_spotify$title[1094] <- "Blame me"
df_spotify$title <- tolower(df_spotify$title)
df_youtube$title[166] <- "Let me down"
df_youtube$title[276] <- "Let me down"
df_youtube$title[353] <- "Let me down"
df_youtube$title[952] <- "Ready for it?"
df_youtube$title[1061] <- "Ready for it?"
df_youtube$title[1177] <- "Ready for it?"
df_youtube$title[1198] <- "Let me down"
df_youtube$title[1261] <- "Ready for it?"
df_youtube$title[1392] <- "Ready for it?"
df_youtube$title[1584] <- "Not Hot"
df_youtube$title <- tolower(df_youtube$title)
df_billboard$title <- tolower(df_billboard$title)

df_spotify$artist <- tolower(df_spotify$artist)
df_youtube$artist <- tolower(df_youtube$artist)
df_billboard$artist <- tolower(df_billboard$artist)

# Data join ---------------------------------------------------------------
df_sp = subset(df_spotify, select = -c(streams))
df_yt = subset(df_youtube, select = -c(views))
df_bb = subset(df_billboard, select = -c(prev_rank))
df_total = rbind(df_sp, df_yt, df_bb)

df_total$title <- tolower(df_total$title)
df_total$artist <- tolower(df_total$artist)


# Top 10 glance to compare in a week [facet_grid mode] -----------------------------------
date_range = c(as.Date("2017-09-15"), as.Date("2017-09-21"))
date_range = date_range#+ 7*2
rank_range = c(1, 10)

geom_User <- switch("Bar",
                    "Scatter" = geom_point(aes(color='green')),
                    "Bar" = geom_bar(aes(color='green'),stat="identity"),
                    "Histogram" = geom_histogram(aes(color='green', fill='yellow'),stat="identity"),
                    "Distribution" = geom_density(aes(fill='lime'),stat="identity"))

df_sel <- df_total %>%
  filter(start_date >= (date_range[1]) & start_date <= (date_range[2])) %>%
  filter(rank<=rank_range[2] & rank >= rank_range[1]) %>%
  mutate(rank_bar = as.numeric(11-rank))
write.csv(df_sel, file='write_csv/df_sel.csv') # for plotly

if(show_plot){
  p <- ggplot(df_sel, aes(x=-rank, y=rank_bar, label=title, group=source )) +
    geom_bar(aes(fill= source), stat = "identity") + #, position = "dodge") +
    ggtitle(paste0("Rankings in each Sources (",rank_range[1],"~",rank_range[2],")@week of ", date_range[1] )) +
    geom_text(size = 3.5, position = position_stack(vjust = 0.7)) + #1.2
    facet_grid(.~source)+
    xlab("Rank") +
    ylab("Track Name") +
    coord_flip() +
    theme_tufte()
  ggplotly(p)
}

# Top 10 glance to compare in a week [dodge mode] -----------------------------------
#   -> Arrange by rank per sources (but hard to distinguish)
if(show_plot){
  p <- ggplot(df_sel, aes(x=-rank, y=rank_bar)) +#, label= paste0(title,"(",artist,")" ))) +
    geom_bar(aes(fill= source), stat = "identity" , position = "dodge") +
    #geom_label(aes(fill = source), colour = "white", fontface = "bold") +
    geom_text(aes(x=-rank, y=rank_bar, label= paste0(title,"(",artist,")" ), group=source), 
              size = 3, position = position_dodge(width = 1.0)) +
    ggtitle(paste0("Rankings in each Sources (",rank_range[1],"~",rank_range[2],") in week of ", date_range[1] )) +
    xlab("Rank") +
    ylab("Track Name") +
    coord_flip() +
    theme_tufte()
  #resolution(df_sel$x)
  ggplotly(p)
}

# Top 10 glance to compare in a week [by artist] -----------------------------------
# Arranging by artist (rank is mixed but easy to see rank difference)
if(show_plot){
  p <- ggplot(df_sel, aes(x=artist, y=rank_bar)) + #, label=rank )) +
    geom_bar(aes(fill= source), stat = "identity", position = "dodge") +
    #geom_text(size = 3, position = position_stack(vjust = 0.3)) +
    geom_text(aes(x=artist, y=rank_bar, label= rank, group=source),
              size = 4, position = position_dodge(width = 0.9), vjust=-0.5, hjust= 1.0) +
    ggtitle(paste0("Rankings in each Sources (",rank_range[1],"~",rank_range[2],") in week of ", date_range[1])) +
    ylab("Rank") +
    xlab("Artist") +
    coord_flip() +
    theme_tufte()
  ggplotly(p)
}


# Wordcloud(p188, p184, 182) for artist and title -> To find who and which is most or least significant.
library(tm)
library(wordcloud)
library(RColorBrewer)
pal = brewer.pal(6,"RdGy")
d_artist_sp = tolower(df_sp$artist)
d_artist_yt = tolower(df_yt$artist)
d_artist_bb = tolower(df_bb$artist)

pal2 <- brewer.pal(8,"Dark2")
par(mfrow = c(1, 3), bg="black")
wordcloud(d_artist_sp, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal)
title("\n\n\n\n\n Spotify", cex.main = 2,   font.main= 4, col.main= "white")
wordcloud(d_artist_yt, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal2)
title("\n\n\n\n\n YouTube", cex.main = 2,   font.main= 4, col.main= "white")
wordcloud(d_artist_bb, min.freq = 30, max.words=Inf, scale = c(4,0.5), random.color = TRUE, colors = pal)
title("\n\n\n\n\n Billboard", cex.main = 2,   font.main= 4, col.main= "white")
title(main="\n\n\n\n Artists who are registered in Top100 more than 30 times",outer= T,
      cex.main = 2,   font.main= 2, col.main= "blue")
# ==> Now find the 10 artists who has a lot of frequency

# ==========================================================================================
# list up what singer were popular in common among three sites =============================
# ==========================================================================================

# Text mining for frequency calculation to use Wordcloud2 ----------------------
FreqMining <- function(INPUT){
  d_artist_sp1 <- tolower(INPUT)
  #d_artist_sp2 <- strsplit(d_artist_sp1, "\\W")
  d_artist_sp2 <- as.factor(d_artist_sp1)
  d_artist_sp3 <- unlist(d_artist_sp2)
  freq<-table(d_artist_sp3)
  freq1<-sort(freq, decreasing=TRUE)
  freq1_df <- as.data.frame(freq1)
  colnames(freq1_df) = c('word', 'freq')
  
  return(freq1_df)
}

tm_sp = FreqMining(d_artist_sp)
tm_yt = FreqMining(d_artist_yt)
tm_bb = FreqMining(d_artist_bb)
min_len = min(length(tm_sp$word), length(tm_yt$word), length(tm_bb$word) )
tm_all = cbind(tm_sp[1:min_len,],tm_yt[1:min_len,],tm_bb[1:min_len,])
write.csv(tm_all, file = "write_csv/tm_all.csv")
head(tm_all,20)

# Using inner_join (artists) ---
tm_sp_yt_injoin = inner_join(tm_sp, tm_yt, by = "word")
tm_all_injoin = inner_join(tm_sp_yt_injoin, tm_bb, by = "word")
colnames(tm_all_injoin) <- c("artist", "Spotify", "YouTube", "BillBoard")
write.csv(tm_all_injoin, file = "write_csv/tm_all_injoin.csv")

# Frequency mining (titles) Not Good for the results : Think more ---
tmt_sp = FreqMining(df_sp$title)
tmt_yt = FreqMining(df_yt$title)
tmt_bb = FreqMining(df_bb$title)
tmt_sp_yt_injoin = inner_join(tmt_sp, tmt_yt, by = "word")
tmt_all_injoin = inner_join(tmt_sp_yt_injoin, tm_bb, by = "word")
colnames(tmt_all_injoin) <- c("title", "Spotify", "YouTube", "BillBoard")
write.csv(tmt_all_injoin, file = "write_csv/tmt_all_injoin.csv")

# Wordcloud2 -----------------------------------------
#require(devtools)
#install_github("lchiffon/wordcloud2")
library(wordcloud2)

if(show_plot){
  #wordcloud2(data = tm_sp, size = 1.0)
  wordcloud2(tm_sp, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
  wordcloud2(tm_yt, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
  wordcloud2(tm_bb, size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
  wordcloud2(tm_sp, size = 1.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
  wordcloud2(tm_yt, size = 1.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
  wordcloud2(tm_bb, size = 1.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
  
  # wordcloud2 with shape
  #wordcloud2(tm_sp, size = 1, shape = 'star') # "cardioid", "diamond"
  
  # Lettercloud ---------------------------------------
  #letterCloud( demoFreq, word = "R", color='random-light' , backgroundColor="black")
  #letterCloud( demoFreq, word = "PEACE", color="white", backgroundColor="pink")
  letterCloud(tm_sp, word = "Spotify", color='random-light' , backgroundColor="black")
  letterCloud(tm_yt, word = "YouTube", color='random-light' , backgroundColor="black")
  letterCloud(tm_bb, word = "Billboard", color='random-light' , backgroundColor="black")
  
  # with backgroud image
  #wordcloud2(tm_sp, figPath = "./figures/twitter.png", size = 1.5,color = "skyblue")
  wordcloud2(tm_sp, figPath = "./figures/spotify.png", size = 1.5,color = "green")
  wordcloud2(tm_yt, figPath = "./figures/youtube.png", size = 1.5,color = "red")
  wordcloud2(tm_bb, figPath = "./figures/billboard.png", size = 1.5,color = "black")
  
  # Pie Charts  w/ Freq ==================================================================
  
  # frequency distribution
  #h = c(mean(tm_sp$freq),mean(tm_yt$freq),mean(tm_bb$freq))
  h = c(20,20,20)
  p <- ggplot(data = tm_sp, aes(x = word, y = freq, fill = word)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_hline(aes(yintercept = h[1])) +
    xlab("Artists") +
    ylab("Number of Frequency ranked in Top 100") +
    ggtitle(paste0("Artists and its Ranking frequency in Spotify Top100 (", ViewPeriod[1], "~", ViewPeriod[2] ,")")) +
    geom_text(aes(0, h[1], label=paste0('cutoff_frequency', as.character(format(round(h[1], 2), nsmall = 2))), hjust=-2, vjust=-1)) +
    theme_bw()
  q <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  q
  ggplotly(p)
  
  p <- ggplot(data = tm_yt, aes(x = word, y = freq, fill = word)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_hline(aes(yintercept = h[2])) +
    xlab("Artists") +
    ylab("Number of Frequency ranked in Top 100") +
    ggtitle(paste0("Artists and its Ranking frequency in YouTube Top100 (", ViewPeriod[1], "~", ViewPeriod[2] ,")")) +
    geom_text(aes(0, h[2], label=paste0('cutoff_frequency', as.character(format(round(h[2], 2), nsmall = 2))), hjust=-2, vjust=-1)) +
    theme_bw()
  q <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  q
  ggplotly(p)
  
  p <- ggplot(data = tm_bb, aes(x = word, y = freq, fill = word)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_hline(aes(yintercept = h[3])) +
    xlab("Artists") +
    ylab("Number of Frequency ranked in Top 100") +
    ggtitle(paste0("Artists and its Ranking frequency in Billboard Top100 (", ViewPeriod[1], "~", ViewPeriod[2] ,")")) +
    geom_text(aes(0, h[3], label=paste0('cutoff_frequency', as.character(format(round(h[3], 2), nsmall = 2))), hjust=-2, vjust=-1)) +
    theme_bw()
  q <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  q
  ggplotly(p)
}
# Word-Freq df cleaning cut-off = 30 freq. --
#title_freq = read.csv('./y_codeoutput/title_freq.csv', stringsAsFactors=FALSE)
#title_freq$word = tolower(title_freq$word)
tm_sp10over = tm_sp[-c(1:10),]
sum(tm_sp10over$freq)

tm_sp10 <- tm_sp[c(1:10),] %>% mutate(source = "spotify") # tm_sp10$source = "spotify"
tm_yt10 <- tm_yt[c(2:11),] %>% mutate(source = "youtube")
tm_bb10 <- tm_bb[c(1:10),] %>% mutate(source = "billboard")
title_freq = rbind(tm_sp10, tm_yt10, tm_bb10)
title_freq

# Add addition columns to data, needed for donut plot.
title_freq <- title_freq %>% 
  group_by(source) %>%
  mutate(fraction = freq/sum(freq)) %>%
  mutate(percentage = round(fraction*100)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n = -1))) %>%
  mutate(lbls = paste(word, percentage)) %>%  # add percents to labels 
  mutate(lbls = paste(lbls, "%", sep=""))

tmp_sp = as.numeric(unlist(as.list(title_freq[title_freq$source=="spotify","freq"])))
tmp_yt = as.numeric(unlist(as.list(title_freq[title_freq$source=="youtube","freq"])))
tmp_bb = as.numeric(unlist(as.list(title_freq[title_freq$source=="billboard","freq"])))
lbls_sp = title_freq$lbls[title_freq$source=="spotify"]
lbls_yt = title_freq$lbls[title_freq$source=="youtube"]
lbls_bb = title_freq$lbls[title_freq$source=="billboard"]

#par(mfrow = c(1, 3), backgroundColor = "black") # multiframe by row
par(mfrow = c(1, 3), backgroundColor = "yellow") # multiframe by row
pie(tmp_sp,labels = lbls_sp, col=rainbow(length(lbls_sp)), main="Spotify")
pie(tmp_yt,labels = lbls_yt, col=rainbow(length(lbls_yt)), main="YouTube")
pie(tmp_bb,labels = lbls_bb, col=rainbow(length(lbls_bb)), main="Billboard")

write.csv(tmp_sp, file='write_csv/tmp_sp.csv') # for plotly
write.csv(tmp_yt, file='write_csv/tmp_yt.csv') # for plotly
write.csv(tmp_bb, file='write_csv/tmp_bb.csv') # for plotly
write.csv(lbls_sp, file='write_csv/lbls_sp.csv') # for plotly
write.csv(lbls_yt, file='write_csv/lbls_yt.csv') # for plotly
write.csv(lbls_bb, file='write_csv/lbls_bb.csv') # for plotly


# * Slope chart(p112) -> show how ranking differ site from site
### ---- Title for each artists (head(tm_all_injoin,11))
titlelist <- function(artist_name){
  tmp <- df_total %>%
    # mutate(artist = tolower(artist)) %>%
    # mutate(title = tolower(title)) %>%  # invalid multibyte string 1094
    filter(artist == tolower(artist_name))
  list_tmp = unique(tmp$title)
  return(list_tmp)
}

singer_list = head(tm_all_injoin,11)[,1]
singer_list

tag_list = c()
for (i in 1:length(singer_list)){
  t_name = paste0('L_', strsplit(singer_list[i], ' ')[[1]][1])
  tag_list = c(tag_list,t_name)
  assign(t_name, unique(tolower(titlelist(singer_list[i]))) )
}
singer_list
tag_list
for (i in 1:length(tag_list)){
  print(get(tag_list[i]))
}

# rank trend comaprison b/w service provider
plot_trend <- function(at_sel, title_sel){
  df_total %>%
    filter(artist == at_sel) %>%
    filter(title %in% title_sel) %>%
    ggplot(aes(x=start_date, y=-1*rank, group = source)) +
    geom_point(aes(color = source)) +
    geom_line(aes(color = source)) +
    #theme_tufte() +
    #theme(plot.background = element_rect(fill = "black")) +
    #theme_wsj()+ scale_colour_wsj("colors6") +
    #theme_calc()+ scale_colour_calc() +
    theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black")) +
    ggtitle(paste0("Weekly Ranking Trend - ", at_sel,"(",title_sel,")")) + #Selected Artists") +
    xlab("DATE") + ylab("RANKING") +
    theme(plot.title = element_text(color="white", size=14, face="bold.italic"),
          axis.title.x = element_text(color="grey", size=12),#, face="bold"),
          axis.title.y = element_text(color="grey", size=12))#, face="bold"))
}

sel_list = cbind(singer_list, tag_list)
# --------------------
# singer_list
# --------------------
# "post malone"     
# "kendrick lamar"  
# "migos"           
# "lil uzi vert"    
# "xxxtentacion"    
# "kodak black"     
# "ed sheeran"     
# "camila cabello"  
# "taylor swift"    
# "imagine dragons" 
# "khalid"
# --------------------

for (songs in tag_list){
  print(paste("------- ", songs, "--------"))
  print(get(songs))
}
print(strrep("=", 50))
print(strrep("=", 50))
print("There are too many songs for one singer !!!")
print("Have to list up what songs were popular in common instead of singer !!")
print(strrep("=", 50))
print(strrep("=", 50))

# ==========================================================================================
# list up what songs were popular in common instead of singer ==============================
# ==========================================================================================
# Data cleaning function for removing (feat.) description in title ------------------------
cleaning_df_w_factor <- function(df){
  df[,] <- lapply(df, function(x) type.convert(as.character(x), as.is = TRUE))
  for (i in 1:dim(df)[1]){
    for (j in 1:dim(df)[2]){
      #for (j in seq(1,6,2)){
      a = gsub("[(].*[)]", "", df[i,j])
      b = gsub("[.]", "", a)
      df[i,j] = trimws(b, which = 'both')
    }
  }
  return(df)
}

cleaning_chr <- function(chr){
  for (i in 1:length(chr)){
      a = gsub("[(].*[)]", "", chr[i])
      b = gsub("[.]", "", a)
      chr[i] = trimws(b, which = 'both')
    }
  return(chr)
}

# Text mining for frequency calculation to use Wordcloud2 ----------------------
d_title_sp = tolower(df_sp$title)
d_title_yt = tolower(df_yt$title)
d_title_bb = tolower(df_bb$title)

d_title_sp = cleaning_chr(d_title_sp)
d_title_yt = cleaning_chr(d_title_yt)
d_title_bb = cleaning_chr(d_title_bb)

ttm_sp = FreqMining(d_title_sp)
ttm_yt = FreqMining(d_title_yt)
ttm_bb = FreqMining(d_title_bb)
min_len = min(length(ttm_sp$word), length(ttm_yt$word), length(ttm_bb$word) )
ttm_all = cbind(ttm_sp[1:min_len,],ttm_yt[1:min_len,],ttm_bb[1:min_len,])
write.csv(ttm_all, file = "write_csv/ttm_all.csv")
head(ttm_all,20)

# ttm_all_org = ttm_all
# #ttm_all = read.csv("write_csv/ttm_all.csv", stringsAsFactors = FALSE, header = TRUE)[-1]
# str(ttm_all) # factor levels exist - to update df, levels should be removed
# ttm_all = cleaning_df_w_factor(ttm_all)
# str(ttm_all) # factor levels removed.
# write.csv(ttm_all, file = "write_csv/ttm_all.csv")

# Using inner_join (artists) -------------------
ttm_sp_yt_injoin = inner_join(ttm_sp, ttm_yt, by = "word")
ttm_all_injoin = inner_join(ttm_sp_yt_injoin, ttm_bb, by = "word")
colnames(ttm_all_injoin) <- c("title", "Spotify", "YouTube", "BillBoard")
tmp_singer_sp = cleaning_df_w_factor(unique(df_sp[,c("title","artist")]))
ttm_all_injoin_artist = inner_join(ttm_all_injoin, unique(tmp_singer_sp), by = "title")
write.csv(ttm_all_injoin, file = "write_csv/ttm_all_injoin.csv")
write.csv(ttm_all_injoin_artist, file = "write_csv/ttm_all_injoin_artist.csv")

threshold = 14
title_in_common = paste0("over",threshold)
assign(title_in_common, ttm_all_injoin_artist %>% filter(Spotify > threshold & YouTube > threshold & BillBoard > threshold))
write.csv(get(title_in_common), file = "write_csv/title_in_common.csv")

# Word-Freq df cleaning cut-off = 30 freq. --
ttm_sp10over = ttm_sp[-c(1:10),]
sum(ttm_sp10over$freq)

head(ttm_all_injoin_artist,10)

# |||||||||||||||||||||||||||||
# ---- Another Problem ----
# |||||||||||||||||||||||||||||

print(strrep("=", 50))
print("There is another porblem !!!")
print("Same song name with differnt signer !!")
print("artist-title should be tied, then count the frequency !!")
print(strrep("=", 50))

# ==========================================================================================
# list up with artist-title combind data ==============================
# ==========================================================================================

title_sp <- cleaning_df_w_factor(df_sp) %>%
  select(start_date, rank, title, artist) %>%
  group_by(title, artist) %>%
  #distinct(artist) %>%
  summarise(freq=n()) %>%
  arrange(desc(freq))

title_yt <- cleaning_df_w_factor(df_yt) %>%
  select(start_date, rank, title, artist) %>%
  group_by(title, artist) %>%
  #distinct(artist) %>%
  summarise(freq=n()) %>%
  arrange(desc(freq))

title_bb <- cleaning_df_w_factor(df_bb) %>%
  select(start_date, rank, title, artist) %>%
  group_by(title, artist) %>%
  #distinct(artist) %>%
  summarise(freq=n()) %>%
  arrange(desc(freq))

title_sp_yt_injoin = inner_join(title_sp, title_yt, by = "title")
title_all_injoin = inner_join(title_sp_yt_injoin, title_bb, by = "title")
colnames(title_all_injoin) <- c("title", "spotify", "sp_freq", "youtube", "yt_freq", "billboard", "bb_freq")
write.csv(title_all_injoin, file = "write_csv/title_all_injoin.csv")

threshold = 14
title_in_common2 = paste0("over",threshold)
assign(title_in_common2, title_all_injoin %>% filter(sp_freq > threshold & yt_freq > threshold & bb_freq > threshold))
write.csv(get(title_in_common2), file = "write_csv/title_in_common2.csv")

if(show_plot) {
  wordcloud2(title_sp[,c("title","freq")], size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
  wordcloud2(title_yt[,c("title","freq")], size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
  wordcloud2(title_bb[,c("title","freq")], size = 1.0, backgroundColor = "black", minRotation = -pi/2, maxRotation = -pi/2)
}

# Word-Freq df cleaning cut-off = 30 freq. --
ttm_sp10 <- title_sp[c(1:10),c("title","freq")] %>% mutate(source = "spotify") # ttm_sp10$source = "spotify"
ttm_yt10 <- title_yt[c(1:10),c("title","freq")] %>% mutate(source = "youtube")
ttm_bb10 <- title_bb[c(1:10),c("title","freq")] %>% mutate(source = "billboard")
title_freq = rbind(ttm_sp10, ttm_yt10, ttm_bb10)
title_freq

# Add addition columns to data, needed for donut plot.
title_freq <- title_freq %>% 
  group_by(source) %>%
  mutate(fraction = freq/sum(freq)) %>%
  mutate(percentage = round(fraction*100)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n = -1))) %>%
  mutate(lbls = paste(title, percentage)) %>%  # add percents to labels 
  mutate(lbls = paste(lbls, "%", sep=""))

ttmp_sp = as.numeric(unlist(as.list(title_freq[title_freq$source=="spotify","freq"])))
ttmp_yt = as.numeric(unlist(as.list(title_freq[title_freq$source=="youtube","freq"])))
ttmp_bb = as.numeric(unlist(as.list(title_freq[title_freq$source=="billboard","freq"])))
lbls_sp = title_freq$lbls[title_freq$source=="spotify"]
lbls_yt = title_freq$lbls[title_freq$source=="youtube"]
lbls_bb = title_freq$lbls[title_freq$source=="billboard"]

par(mfrow = c(1, 3)) # multiframe by row
pie(ttmp_sp,labels = lbls_sp, col=rainbow(length(lbls_sp)), main="Spotify")
pie(ttmp_yt,labels = lbls_yt, col=rainbow(length(lbls_yt)), main="YouTube")
pie(ttmp_bb,labels = lbls_bb, col=rainbow(length(lbls_bb)), main="Billboard")

write.csv(ttmp_sp, file='write_csv/ttmp_sp.csv') # for plotly
write.csv(ttmp_yt, file='write_csv/ttmp_yt.csv') # for plotly
write.csv(ttmp_bb, file='write_csv/ttmp_bb.csv') # for plotly
write.csv(lbls_sp, file='write_csv/lbls_sp.csv') # for plotly
write.csv(lbls_yt, file='write_csv/lbls_yt.csv') # for plotly
write.csv(lbls_bb, file='write_csv/lbls_bb.csv') # for plotly

# * Slope chart(p112) -> show how ranking differ site from site

# rank trend comaprison b/w service provider
df_total_clean <- cleaning_df_w_factor(df_total)
df_total_clean <- df_total_clean %>% 
  mutate(start_date = as.Date(start_date, "%Y-%m-%d")) %>%
  mutate(rank = as.numeric(rank))

plot_trend <- function(at_sel, title_sel){
  a <- df_total_clean %>%
    filter(artist == at_sel) %>%
    filter(title == title_sel) #%>%
  #filter(title %in% title_sel) %>%
  p <- ggplot(a, aes(x=start_date, y=-1*rank, group = source)) +
    geom_point(aes(color = source)) +
    geom_line(aes(color = source)) +
    theme_wsj()+ scale_colour_wsj("colors6") +
    ggtitle(paste0("Weekly Ranking Trend - ", at_sel,"(",title_sel,")")) + #Selected Artists") +
    xlab("DATE") + ylab("RANKING") #+
  #theme(plot.title = element_text(color="white", size=14, face="bold.italic"),
  #      axis.title.x = element_text(color="grey", size=12),#, face="bold"),
  #      axis.title.y = element_text(color="grey", size=12))#, face="bold"))
  ggplotly(p)
}

# Title           |	Artist
# 1-800-273-8255  |	Logic
# bad at love     | Halsey
# bank account    | Savage
# believer        | Imagine Dragons
# bodak yellow    | Cardi B
# congratulations | Post Malone
# gucci gang      | Lil Pump
# havana          | Camila Cabello
# i get the bag   | Gucci Mane
# new rules       | Dua Lipa
# plain jane      | A$AP Ferg
# rockstar        | Post Malone
# roll in peace   | Kodak Black
# shape of you    | Ed Sheeran
# sorry not sorry | Demi Lovato
# thunder         | Imagine Dragons
# young dumb & broke  | Khalid
# mi gente        | J Balvin
# no limit        | G-Eazy

if(show_plot){
  plot_trend(tolower('Logic'),'1-800-273-8255')
  plot_trend(tolower('Halsey'),'bad at love')
  plot_trend(tolower('21 savage'),'bank account')
  plot_trend(tolower('Imagine Dragons'),'believer')
  plot_trend(tolower('Cardi B'),'bodak yellow')
  plot_trend(tolower('Post Malone'),'congratulations')
  plot_trend(tolower('Lil Pump'),'gucci gang')
  plot_trend(tolower('Camila Cabello'),'havana')
  plot_trend(tolower('Gucci Mane'),'i get the bag')
  plot_trend(tolower('Dua Lipa'),'new rules')
  plot_trend(tolower('A$AP Ferg'),'plain jane')
  plot_trend(tolower('Post Malone'),'rockstar')
  plot_trend(tolower('Kodak Black'),'roll in peace')
  plot_trend(tolower('Ed Sheeran'),'shape of you')
  plot_trend(tolower('Demi Lovato'),'sorry not sorry')
  plot_trend(tolower('Imagine Dragons'),'thunder')
  plot_trend(tolower('Khalid'),'young dumb & broke')
  plot_trend(tolower('J Balvin'),'mi gente')
  plot_trend(tolower('G-Eazy'),'no limit')
  
  plot_trend(tolower('Taylor swift'),'look what you made me do')
  plot_trend(tolower('Taylor swift'),'...ready for it?')
  plot_trend(tolower('Taylor swift'),'end game')
  plot_trend(tolower('Post Malone'),'candy paint')
  plot_trend(tolower('Ed Sheeran'),'perfect')
}

# ########
# EOF
# ########


# -------------------------
# Appendix for future use
#------------------------------------------------------------------

# Mosaic Plot
#   Divide the data according to different variables, and then use rectangles of different
#   sizes to represent different groups of data.

#install.packages("vcd")
# library(vcd)
# mosaic(Survived ~ Class + Sex, data=Titanic, shade=T,
#        highlighting_fill=c('red4',"skyblue"),
#        highlighting_direction="left")
# ------------------------------------------------------------


# * Mosaic map for each week per sites -> show how different the ranking is

# 1) Treemap(p75) for artist and title -> To find who and which is most or least significant.
# 2) Selected most / least artist/title 
#    line plot -> compare rankinng trend for 3 sites
#    radial plot(p151) -> compare 


# cf) Analysis by genre -> pie chart (p124,; p102,104,106,109)

# cf) bubble plot?

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started