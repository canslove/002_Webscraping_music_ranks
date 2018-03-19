from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from db import Review
from datetime import date
import time
import re

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\where\you\download\the\chromedriver.exe')
driver = webdriver.Chrome(r'.\chromedriver_win32\chromedriver.exe')
driver.get("https://www.verizonwireless.com/smartphones/samsung-galaxy-s8")

# Page index used to keep track of where we are.
index = 1
prev_button = None
current_button = None
while True:
	try:
		# We first need to make sure the button on the previous page is not available anymo
		if prev_button is not None:
			WebDriverWait(driver, 10).until(EC.staleness_of(prev_button))

		print("Scraping Page number " + str(index))
		index = index + 1
		# Find all the reviews on the page
		wait_review = WebDriverWait(driver, 10)
		reviews = wait_review.until(EC.presence_of_all_elements_located((By.XPATH,
									'//li[@class="bv-content-item bv-content-top-review bv-content-review"]')))
		for review in reviews:

			# Use relative xpath to locate the title, content, username, date.
			# Once you locate the element, you can use 'element.text' to return its string.
			# To get the attribute instead of the text of each element, use 'element.get_attribute()'
			title = review.find_element_by_xpath('.//h4[@class="bv-content-title"]').text
			content = review.find_elements_by_xpath('.//div[@class="bv-content-summary-body-text"]/p')
			content = ''.join(map(lambda x: x.text, content))
			username = review.find_element_by_xpath('.//h3[@class="bv-author"]').text

			# We use date_ to avoid naming conflict with the date method from datetime.
			date_ = review.find_element_by_xpath('.//meta[@itemprop="datePublished"]').get_attribute('content')
			year, month, day = map(lambda x: int(x), date_.split('-'))
			date_ = date(year, month, day)

			rating = review.find_element_by_xpath('.//span[@class="bv-rating-stars-container"]/span').text
			rating = int(re.search('\d+', rating).group())

			# create a Review object that correponds to one row of our table.
			review = Review(username=username,
							title=title,
							content=content,
							date=date_,
							rating=rating)
			review.save()

		# Locate the next button on the page.
		wait_button = WebDriverWait(driver, 10)
		current_button = wait_button.until(EC.element_to_be_clickable((By.XPATH,
									'//li[@class="bv-content-pagination-buttons-item bv-content-pagination-buttons-item-next"]')))
		prev_button = current_button
		current_button.click()
	except Exception as e:
		print(e)
		driver.close()
		break
