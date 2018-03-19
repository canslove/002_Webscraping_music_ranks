from scrapy import Spider, Request
from itunes.items import ItunesItem
import datetime

class ItunesSpider(Spider):

	name = "itunes_spider"

	allowed_urls = ["http://www.popvortex.com/music/charts/"]
	start_urls = ["http://www.popvortex.com/music/charts/top-100-songs.php"]

	def parse(self, response):
		rows1 = response.xpath('/html/body/div[1]/div[3]/div[1]//div[@class="feed-item music-chart row"]')
		rows2 = response.xpath('/html/body/div[1]/div[3]/div[1]//div[@class="feed-item music-chart row new-release"]')
		rows = rows1 + rows2
		#len(rows)

		date = response.xpath('/html/body/div[1]/div[3]/div[1]/p[4]/b/@content').extract_first()

		for row in rows:
			title = row.xpath('.//div/p/cite/a[@rel="nofollow noopener"]/text()').extract_first()
			artist = row.xpath('.//div/p/em[@class="artist"]/text()').extract_first()
			genre = row.xpath('.//div/ul//li/a/text()').extract_first()
			release_date = row.xpath('.//div/ul//li[2]/text()').extract_first()
			
			#import re
			#release_date = '-'.join(re.split(':|,|\s ',release_date))

			item = ItunesItem()
			item['date'] = date
			item['title'] = title
			item['artist'] = artist
			item['genre'] = genre
			item['release_date'] = release_date

			yield item