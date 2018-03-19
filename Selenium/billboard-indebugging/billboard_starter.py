from selenium import webdriver
import time
import re

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\the\chromedriver.exe')
driver = webdriver.Chrome(r'..\chromedriver_win32\chromedriver.exe')
# Go to the page that we want to scrape
#driver.get("https://www.billboard.com/charts/hot-100/2018-02-03")
driver.get("https://www.billboard.com/charts/hot-100/2017-01-07")
date = driver.find_element_by_xpath('.//*[@id="chart-nav"]/time').text
time.sleep(5)
# Page index used to keep track of where we are.
index = 1
# We want to start the first two pages.
# If everything works, we will change it to while True
#while True:
while index <= 2:
	try:
		print("Scraping Page number " + str(index))
		index = index + 1
		# Find all the reviews. The find_elements function will return a list of selenium select elements.
		# Check the documentation here: http://selenium-python.readthedocs.io/locating-elements.html
		#reviews = driver.find_elements_by_xpath('//*[@id="main"]/div[2]/div/div//article')
		#reviews = driver.find_elements_by_xpath('//*[@id="main"]/div[2]/div/div//article//div[@class="chart-row__primary"]')
		#title = driver.find_element_by_xpath('//*[@id="main"]/div[2]/div/div/article[2]/div[1]/div[2]/div[3]/div/h2').text
		#title = driver.find_element_by_xpath('//*[@id="main"]/div[2]/div/div/article[4]/div[1]/div[2]/div[3]/div//h2').text
		#title = driver.find_elements_by_xpath('//*[@id="main"]/div[3]/div/div/article[1]/div[1]/div[2]/div[3]/div/h2')
		rows = driver.find_elements_by_xpath('//*[@id="main"]//article[@data-hovertracklabel="Song Hover-"]')

		for row in rows:
			# Initialize an empty dictionary for each review
			list_dict = {}
			# Use relative xpath to locate the title, content, username, date, rating.
			# Once you locate the element, you can use 'element.text' to return its string.
			# To get the attribute instead of the text of each element, use `element.get_attribute()`
			# And Just specify the last leaf with .//, because we don't know how may tags vary inbetween

			rank = row.find_element_by_xpath('.//span[@class="chart-row__current-week"]').text
			title = row.find_element_by_xpath('.//h2[@class="chart-row__song"]').text #a[1]

			#artist = row.find_element_by_xpath('.//span[@class="chart-row__artist"]').text #a[2]
			patterns = ['.//span[@class="chart-row__artist"]','.//a[@class="chart-row__artist"]']
			for pattern in patterns:
				# Return an empty string if the list is empty.
				print(pattern)
				artist = row.find_element_by_xpath(pattern).text #a[2]
				if artist:
					break

			#for debugging
			print('='*50)
			print(len(rows))
			print(date)
			print(rank)
			print(title)
			print(artist)


		# # Iterate through the list and find the details of each review.
		# for review in reviews:
		# 	# Initialize an empty dictionary for each review
		# 	review_dict = {}
		# 	# Use relative xpath to locate the title, content, username, date, rating.
		# 	# Once you locate the element, you can use 'element.text' to return its string.
		# 	# To get the attribute instead of the text of each element, use `element.get_attribute()`
		# 	#title = review.find_element_by_xpath('.//h4[@class="bv-content-title"]').get_attribute('itemprop')
		# 	title = review.find_element_by_xpath('.//h4[@class="bv-content-title"]').text
		# 	#content = review.find_element_by_xpath('//div[@class="bv-content-summary-body-text"]/p').text
		# 	content = review.find_elements_by_xpath('.//div[@class="bv-content-summary-body-text"]/p')
		# 	content = ''.join([x.text for x in content])
		# 	print('='*50)
		# 	print(title)
		# 	#print(content)			
		# 	# Your code here



		# # Locate the next button element on the page and then call `button.click()` to click it.
		# button = driver.find_element_by_xpath('//li[@class="bv-content-pagination-buttons-item bv-content-pagination-buttons-item-next"]')
		# button.click()
		# time.sleep(2)

	except Exception as e:
		print(e)
		driver.close()
		break
