from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import csv
import re

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\where\you\download\the\chromedriver.exe')
driver = webdriver.Chrome(r'..\chromedriver_win32\chromedriver.exe')

driver.get("https://www.billboard.com/charts/hot-100/2018-02-10")

# Windows users need to open the file using 'wb'
# csv_file = open('reviews.csv', 'wb')
csv_file = open('billboard.csv', 'w', encoding='UTF-8', newline='')
writer = csv.writer(csv_file)
# Page index used to keep track of where we are.
index = 0
# Initialize two variables refer to the next button on the current page and previous page.
prev_button = None
current_button = None
while True:
	try:
		# We first need to make sure the button on the previous page is not available anymore.
		if prev_button is not None:
			WebDriverWait(driver, 10).until(EC.staleness_of(prev_button))

		print("Scraping Page number " + str(index))
		index = index + 1
		# Find all the reviews on the page
		wait_review = WebDriverWait(driver, 10)
		page = wait_review.until(EC.presence_of_all_elements_located((By.XPATH,
									'//*[@id="chart-nav"]/a[1]/i')))
		date = page.find_element_by_xpath('.//*[@id="chart-nav"]/time').text

		rows = page.find_elements_by_xpath('//*[@id="main"]//article[@data-hovertracklabel="Song Hover-"]')

		for row in rows:
			# Initialize an empty dictionary for each review
			review_dict = {}
			# Use relative xpath to locate the title, content, username, date.
			# Once you locate the element, you can use 'element.text' to return its string.
			# To get the attribute instead of the text of each element, use 'element.get_attribute()'
			rank = row.find_element_by_xpath('.//span[@class="chart-row__current-week"]').text
			title = row.find_element_by_xpath('.//h2[@class="chart-row__song"]').text #a[1]
			artist = row.find_element_by_xpath('.//a[@class="chart-row__artist"]').text #a[2]

			if(index==1):
				print('='*50)
				print(date)
				print(rank)
				print(title)
				print(artist)

			review_dict['date'] = date
			review_dict['rank'] = rank
			review_dict['title'] = title
			review_dict['artist'] = artist

			writer.writerow(review_dict.values())

		# Locate the next button on the page.
		wait_button = WebDriverWait(driver, 10)
		current_button = wait_button.until(EC.element_to_be_clickable((By.XPATH,
									'//*[@id="chart-nav"]/a[1]/i')))
		
		prev_button = current_button
		current_button.click()
	except Exception as e:
		print(e)
		csv_file.close()
		driver.close()
		break
