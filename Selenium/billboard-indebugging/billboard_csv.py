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

# gen the address such as "https://www.billboard.com/charts/hot-100/2018-02-03"
min=datetime.date(2017,1,7)
max=datetime.date(2018,2,3)
print(min,max)

date_ = min
pagedate =[str(min)]
while (date_ < max):
    date_ = date_ + datetime.timedelta(days=7)
    pagedate.append(str(date_))
#print(pagedate)
#len(pagedate)

# Windows users need to open the file using 'wb'
#csv_file = open('youtubemusic.csv', 'wb') # wb is write file as binary mode, but here is problem with 'wb'
csv_file = open('billboard.csv', 'w', encoding='UTF-8', newline='')
writer = csv.writer(csv_file)
# Page index used to keep track of where we are.
index = 0
idx=0
# Initialize two variables refer to the next button on the current page and previous page.
#prev_button = None
#current_button = None
for page in pagedate:
	# Go to the page that we want to scrape
	try:
		# We first need to make sure the button on the previous page is not available anymore.
		#if prev_button is not None:
		#	WebDriverWait(driver, 10).until(EC.staleness_of(prev_button))
		driver.get("https://www.billboard.com/charts/hot-100/" + page)
		time.sleep(7)
		print("Scraping Page for - https://www.billboard.com/charts/hot-100/" + page)
		
		# Find all the reviews on the page
		#wait_review = WebDriverWait(driver, 10)
		#reviews = wait_review.until(EC.presence_of_all_elements_located((By.XPATH,
		#							'//li[@class="bv-content-item bv-content-top-review bv-content-review"]')))
		rows = driver.find_elements_by_xpath('//*[@id="chart-container"]/paper-card/div[2]/iron-pages/ytma-chart[1]/div[2]//div[@class="row style-scope ytma-chart"]')
		# for all row search, make sure the last directory should be searched all not noly one by using '//' instead of '/'
		# here, if you use /div[@class="row style-scope ytma-chart"], this is only one row not all rows.
		# so, if all rows are share common directory and attribute name, you can use '//div[@class="row style-scope ytma-chart"]' to find all as above'
		date = page.find_element_by_xpath('.//*[@id="chart-nav"]/time').text
		rows = page.find_elements_by_xpath('//*[@id="main"]//article[@data-hovertracklabel="Song Hover-"]')

		for row in rows:
			# Initialize an empty dictionary for each review
			review_dict = {}
			# Use relative xpath to locate the title, content, username, date.
			# Once you locate the element, you can use 'element.text' to return its string.
			# To get the attribute instead of the text of each element, use 'element.get_attribute()'

			# make sure to use .// not ./ (because, sub directroy tree cannot be seen directly)
			# And Just specify the last leaf with .//, because we don't know how may tags vary inbetween
			rank = row.find_element_by_xpath('.//span[@class="chart-row__current-week"]').text
			title = row.find_element_by_xpath('.//h2[@class="chart-row__song"]').text #a[1]
			#artist = row.find_element_by_xpath('.//a[@class="chart-row__artist"]').text #a[2]
			#artist = row.find_element_by_xpath('.//span[@class="chart-row__artist"]').text #a[2]
			patterns = ['.//span[@class="chart-row__artist"]','.//a[@class="chart-row__artist"]']
			for pattern in patterns:
				# Return an empty string if the list is empty.
				print(pattern)
				artist = row.find_element_by_xpath(pattern).text #a[2]
				if artist:
					break
		

			# just for verification for index=0, and last
			if(index==0 or index==len(pagedate)-1):
				print('='*50)
				print(date)
				print(rank)
				print(title)
				print(artist)

			review_dict['date'] = date.rstrip()
			review_dict['rank'] = rank.rstrip()
			review_dict['title'] = title.rstrip()
			review_dict['artist'] = artist.rstrip()

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
