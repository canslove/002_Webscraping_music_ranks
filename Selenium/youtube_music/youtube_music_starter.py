from selenium import webdriver
import time
import re
import datetime

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\the\chromedriver.exe')
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


index = 0
for page in pages:
	# Go to the page that we want to scrape
	try:
		driver.get("https://artists.youtube.com/charts/tracks?chart_params_id=weekly" + page)
		time.sleep(7)
		print("Scraping Page number " + str(index))
		index = index + 1
		# Find all the reviews. The find_elements function will return a list of selenium select elements.
		# Check the documentation here: http://selenium-python.readthedocs.io/locating-elements.html
		#reviews = driver.find_elements_by_xpath('//li[@class="bv-content-item bv-content-top-review bv-content-review"]')
		rows = driver.find_elements_by_xpath('//*[@id="chart-container"]/paper-card/div[2]/iron-pages/ytma-chart[1]/div[2]//div[@class="row style-scope ytma-chart"]')
		# for all row search, make sure the last directory should be searched all not noly one by using '//' instead of '/'
		# here, if you use /div[@class="row style-scope ytma-chart"], this is only one row not all rows.
		# so, if all rows are share common directory and attribute name, you can use '//div[@class="row style-scope ytma-chart"]' to find all as above'

		# print('='*50)
		# print(len(rows))

		# Iterate through the list and find the details of each review.
		for row in rows:
			# Initialize an empty dictionary for each review
			list_dict = {}
			# Use relative xpath to locate the title, content, username, date, rating.
			# Once you locate the element, you can use 'element.text' to return its string.
			# To get the attribute instead of the text of each element, use `element.get_attribute()`
			rank = row.find_element_by_xpath('.//div[@class="rank-num style-scope ytma-chart"]').text #div[1]
			# make sure to use .// not ./ (because, sub directroy tree cannot be seen directly)
			# And Just specify the last leaf with .//, because we don't know how may tags vary inbetween
			#title = row.find_element_by_xpath('.//div[3]/a[1]').text
			title = row.find_element_by_xpath('.//div[@class="title metadata style-scope ytma-chart"]/a[1]').text #a[1]
			# make sure to use .// not ./ (because, sub directroy tree cannot be seen directly)
			#artist = row.find_element_by_xpath('.//div[3]/a[2]').text #a[2]
			artist = row.find_element_by_xpath('.//div[@class="title metadata style-scope ytma-chart"]/a[2]').text #a[2]
			# make sure to use .// not ./ (because, sub directroy tree cannot be seen directly)		
			view = row.find_element_by_xpath('.//div[@class="views style-scope ytma-chart"]').text
			# make sure to use .// not ./ (because, sub directroy tree cannot be seen directly)		
			#content = review.find_element_by_xpath('//div[@class="bv-content-summary-body-text"]/p').text
			#content = review.find_elements_by_xpath('.//div[@class="bv-content-summary-body-text"]/p')
			#content = ''.join([x.text for x in content])

			print('='*50)
			print(rank)
			print(title)
			print(artist)
			print(view)
			#print(content)			

		# Locate the next button element on the page and then call `button.click()` to click it.
		#button = driver.find_element_by_xpath('//li[@class="bv-content-pagination-buttons-item bv-content-pagination-buttons-item-next"]')
		#button.click()
		#time.sleep(2)

	except Exception as e:
		print(e)
		driver.close()
		break