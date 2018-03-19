from scrapy import Spider, Request
from acharts.items import AchartsItem
import datetime

class SpotifySpider(Spider):

	name = "acharts_spider"
	target = 1 # 1(single), 2(albums)

	allowed_urls = ["https://acharts.co/"]
	if(target == 1):
		start_urls = ["https://acharts.co/us_singles_top_100/2018/06"] # singles
	else:
		start_urls = ["https://acharts.co/us_albums_top_100/2018/06"] # albums

	def parse(self, response):
		num_weeks = range(1,53)

		#target : 1(single), 2(albums)
		target_url = "https://acharts.co/us_singles_top_100/"
		#target_url = "https://acharts.co/us_albums_top_100/"

		y2017 = [target_url + "2017/" + str(num).zfill(2) for num in num_weeks]
		y2018 = [target_url + "2018/" + str(num).zfill(2) for num in range(1,7)]
		pages = y2017 + y2018
		#print(page)

		for url in pages:
			yield Request(url, callback=self.parse_top)


	def parse_top(self, response):
		#rows = response.xpath('//*[@id="content"]/div/div/div/span/table/tbody/tr')
		rows = response.xpath('//*[@id="ChartTable"]/tbody/tr[@itemprop="itemListElement"]')
		#len(rows)

		wks = response.url
		if(wks.split("/")[-2] == "2017"):
			min = datetime.date(2017,1,7) # 1st week of 2017
		else:
			min = datetime.date(2018,1,6) # 1st week of 2018

		wks = int(wks.split("/")[-1])		
		date = min + datetime.timedelta(days = 7* (wks-1))
		date = str(date)

		for row in rows:
			rank = row.xpath('.//td[1]/span[@itemprop="position"]/text()').extract_first()
			#rank_stat = row.xpath('.//td[@class="cStats cMhidden"]/text()').extract_first().strip()
			title = row.xpath('.//td//a/span[@itemprop="name"]/text()').extract_first()
			#title = row.xpath('.//td[3]/a/span[@itemprop="name"]/text()').extract_first()
			artist = row.xpath('.//td//span//span/span[@itemprop="name"]/text()').extract_first()
			#artist = row.xpath('.//td[3]/span[1]/span/span[@itemprop="name"]/text()').extract_first()
			weeks = row.xpath('.//td[@class="cStats cShidden"]/text()').extract_first().strip()

			item = AchartsItem()
			item['date'] = date
			item['rank'] = rank
			item['title'] = title
			item['artist'] = artist
			item['weeks'] = weeks

			yield item