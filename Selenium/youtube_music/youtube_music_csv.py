from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import csv
import re
import datetime

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\where\you\download\the\chromedriver.exe')
driver = webdriver.Chrome(r'..\chromedriver_win32\chromedriver.exe')

# date arithmetic for datetime indexing
max=datetime.date(2018,2,1)
min=datetime.date(2017,9,15)
#print(min,max)
n= int((max-min+datetime.timedelta(days=1))/datetime.timedelta(days=7))
#print(n)
start_date = min
end_date = start_date + datetime.timedelta(days=6)

datelist_sta = [str(min)]
datelist_end = [str(end_date)]

tmp = min
tmp2 = end_date

for i in range(n-1):
	tmp += datetime.timedelta(days=7)
	datelist_sta.append(str(tmp))    
	tmp2 += datetime.timedelta(days=7)
	datelist_end.append(str(tmp2))
#print(datelist_sta, datelist_end)
a = list(map(lambda s: "".join(s.split('-')), datelist_sta))
b = list(map(lambda s: "".join(s.split('-')), datelist_end))
# %3A20180119%3A20180125%3Aus
pages = list(map(lambda x, y, z, r, q: x+y+z+r+q, ['%3A']*20, a, ['%3A']*20, b, ['%3Aus']*20))

# Windows users need to open the file using 'wb'
#csv_file = open('youtubemusic.csv', 'wb') # wb is write file as binary mode, but here is problem with 'wb'
csv_file = open('youtubemusic.csv', 'w', encoding='UTF-8', newline='')
writer = csv.writer(csv_file)
# Page index used to keep track of where we are.
index = 0
idx=0
# Initialize two variables refer to the next button on the current page and previous page.
#prev_button = None
#current_button = None
for page in pages:
	# Go to the page that we want to scrape
	try:
		# We first need to make sure the button on the previous page is not available anymore.
		#if prev_button is not None:
		#	WebDriverWait(driver, 10).until(EC.staleness_of(prev_button))
		driver.get("https://artists.youtube.com/charts/tracks?chart_params_id=weekly" + page)
		time.sleep(10)
		print("Scraping Page number " + str(index))
		
		# Find all the reviews on the page
		#wait_review = WebDriverWait(driver, 10)
		#reviews = wait_review.until(EC.presence_of_all_elements_located((By.XPATH,
		#							'//li[@class="bv-content-item bv-content-top-review bv-content-review"]')))
		rows = driver.find_elements_by_xpath('//*[@id="chart-container"]/paper-card/div[2]/iron-pages/ytma-chart[1]/div[2]//div[@class="row style-scope ytma-chart"]')
		# for all row search, make sure the last directory should be searched all not noly one by using '//' instead of '/'
		# here, if you use /div[@class="row style-scope ytma-chart"], this is only one row not all rows.
		# so, if all rows are share common directory and attribute name, you can use '//div[@class="row style-scope ytma-chart"]' to find all as above'

		for row in rows:
			# Initialize an empty dictionary for each review
			review_dict = {}
			# Use relative xpath to locate the title, content, username, date.
			# Once you locate the element, you can use 'element.text' to return its string.
			# To get the attribute instead of the text of each element, use 'element.get_attribute()'

			# make sure to use .// not ./ (because, sub directroy tree cannot be seen directly)
			# And Just specify the last leaf with .//, because we don't know how may tags vary inbetween
			rank = row.find_element_by_xpath('.//div[@class="rank-num style-scope ytma-chart"]').text #div[1]
			title = row.find_element_by_xpath('.//div[@class="title metadata style-scope ytma-chart"]/a[1]').text #a[1]
			artist = row.find_element_by_xpath('.//div[@class="title metadata style-scope ytma-chart"]/a[2]').text #a[2]
			view = row.find_element_by_xpath('.//div[@class="views style-scope ytma-chart"]').text

			# just for verification for index=0, and last
			if(index==0 or index==n-1):
				print('='*50)
				print(datelist_sta[index])
				print(datelist_end[index])
				print(rank)
				print(title)
				print(artist)
				print(view)

			review_dict['start_date'] = datelist_sta[index].rstrip() # a[index]
			review_dict['end_date'] = datelist_end[index].rstrip() #b[index]
			review_dict['rank'] = rank.rstrip()
			review_dict['title'] = title.rstrip()
			review_dict['artist'] = artist.rstrip()
			review_dict['view'] = view.rstrip()
			#print('OK here!')

			writer.writerow(review_dict.values())

		index = index + 1

		# Locate the next button on the page.
		#wait_button = WebDriverWait(driver, 10)
		#current_button = wait_button.until(EC.element_to_be_clickable((By.XPATH,
		#							'//li[@class="bv-content-pagination-buttons-item bv-content-pagination-buttons-item-next"]')))
		#prev_button = current_button
		#current_button.click()
	except Exception as e:
		print(e)
		csv_file.close()
		driver.close()
		break
