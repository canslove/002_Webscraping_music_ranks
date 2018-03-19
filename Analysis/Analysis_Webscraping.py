
# coding: utf-8

# In[14]:


get_ipython().system('dir')


# In[3]:


import numpy as np
import pandas as pd


# In[25]:


df_spotify = pd.read_csv('spotify_wkly.csv')
df_youtube = pd.read_csv('youtubemusic.csv', names=['start_date','end_date','rank','title','artist','views'])
df_acharts = pd.read_csv('acharts_singles.csv')
df_acharts_albums = pd.read_csv('acharts_albums.csv')


# In[26]:


#!type spotify_wkly.csv
#!more youtubemusic.csv
#!more acharts_singles.csv
#!type acharts_albums.csv


# In[27]:


df_spotify.describe(include='all')


# In[28]:


df_spotify.head(5)


# In[29]:


type(df_spotify.start_date)


# In[40]:


a="2018-02-02"
print(type(a))
b=pd.to_datetime(a, format='%Y-%m-%d')
b
#print(type(b))


# In[41]:


type(df_spotify.start_date[1])
a =pd.to_datetime(df_spotify.start_date, format='%Y-%m-%d', errors='ignore')
type(a[1])


# In[42]:


df_spotify.start_date = pd.to_datetime(df_spotify.start_date, format='%Y-%m-%d', errors='ignore')
df_spotify.end_date = pd.to_datetime(df_spotify.end_date, format='%Y-%m-%d', errors='ignore')


# In[45]:


cols = df_spotify.columns.tolist
cols


# In[46]:


new_cols = ['start_date', 'end_date', 'rank', 'title', 'artist', 'streams']
df_spotify = df_spotify[new_cols]
df_spotify.head(5)


# In[47]:


df_spotify.describe(include='all')


# In[50]:


print(df_youtube.columns.tolist) # ['start_date','end_date','rank','title','artist','views'])
print(df_acharts.columns.tolist)
print(df_acharts_albums.columns.tolist)


# In[51]:


df_acharts = df_acharts[['date', 'rank', 'title', 'artist', 'weeks']]
df_acharts_albums = df_acharts_albums[['date', 'rank', 'title', 'artist', 'weeks']]


# In[52]:


print(df_youtube.start_date[0])
print(df_acharts.date[0])
print(df_acharts_albums.date[0])


# In[54]:


df_youtube.start_date = pd.to_datetime(df_youtube.start_date, format='%Y-%m-%d', errors='ignore')
df_youtube.end_date = pd.to_datetime(df_youtube.end_date, format='%Y-%m-%d', errors='ignore')
df_acharts.date = pd.to_datetime(df_acharts.date, format='%Y-%m-%d', errors='ignore')
df_acharts_albums.date = pd.to_datetime(df_acharts_albums.date, format='%Y-%m-%d', errors='ignore')


# In[57]:


df_spotify.head(5)


# In[58]:


df_youtube.head(5)


# In[59]:


df_acharts.head(5)


# In[60]:


df_acharts_albums.head(5)


# In[63]:


df_spotify = df_spotify.sort_values(by=['start_date','rank'], ascending=True, na_position='last')
df_youtube = df_youtube.sort_values(by=['start_date','rank'], ascending=True, na_position='last')
df_acharts = df_acharts.sort_values(by=['date','rank'], ascending=True, na_position='last')
df_acharts_albums = df_acharts_albums.sort_values(by=['date','rank'], ascending=True, na_position='last')


# In[64]:


df_spotify.head(5)


# In[65]:


df_youtube.head(5)


# In[66]:


df_acharts.head(5)


# In[67]:


df_acharts_albums.head(5)


# In[68]:


df_spotify.describe(include='all')


# In[69]:


df_youtube.describe(include='all')


# In[70]:


df_acharts.describe(include='all')


# In[71]:


df_acharts_albums.describe(include='all')


# ## Youtube starts from 9/15, so all analysis should start from 9/15/17
#   
#   1) concat all table with another column "source" which means origination such as 'spotify', 'youtube', 'billboard' and 'billboard_album' <br>
#   2) make rankinng matching ratio per weak (find same title and calculate ranking based on title or artist <br>
#   3) visualize grapth for ranking transition per artist/title comapare to each companies <br>

# In[97]:


#import datetime
target_date = pd.to_datetime(("2017-09-15"), format='%Y-%m-%d', errors='ignore')
target_date
#[x == target_date  for x in df_spotify.start_date if(x == target_date) ]
#[x == target_date for x in df_youtube.start_date if(x == target_date) ]
#[x == target_date  for x in df_acharts.start_date if(x == target_date) ]
#[x == target_date  for x in df_acharts_albums.start_date if(x == target_date) ]


# In[83]:


datetime.date(2017,9,15)


# In[ ]:


track_counts = data['track_name'].value_counts()
track = track_counts.index[-700]
(data.loc[data['track_name'] == track, ('date', 'streams')]
 .set_index('date')
 .iplot(kind='bar',
        yTitle='# Streams',
        title=track))


# In[ ]:


TOP_N = 200

data = data.sort_values(['artist', 'track_name', 'date'])
data['next_date'] = data.groupby(['artist', 'track_name'])['date'].shift(-1)

data['in_next_day'] = (data['next_date'] - data['date']).dt.days == 1
probabilities = data.groupby('date')['in_next_day'].sum().divide(TOP_N)

probabilities.iplot(title='Conditional Probability')

