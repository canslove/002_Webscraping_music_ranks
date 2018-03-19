from scrapy import Spider, Request
from spotify_wkly.items import SpotifyWklyItem


class SpotifyWklySpider(Spider):

	name = "spotify_wkly_spider"
	allowed_urls = ["https://spotifycharts.com/"]
	start_urls = ["https://spotifycharts.com/regional/us/weekly/latest/"]

	# 1st region/us 
	def parse(self, response):
		datelist_top = response.xpath('//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[3]')
		
		links = datelist_top.xpath('./ul/li/@data-value').extract() # all date link(yyyy-mm-dd)

		pages = ["https://spotifycharts.com/regional/us/weekly/" + link for link in links]
		#print(page)

		for url in pages:
			yield Request(url, callback=self.parse_top)


	def parse_top(self, response):
		rows = response.xpath('//*[@id="content"]/div/div/div/span/table/tbody/tr')

		date = response.url
		date = date.split("/")[-1]
		start_date = date.split('--')[0]
		end_date = date.split('--')[1]

		for row in rows:
			#rank = row.xpath('./td[2]/text()').extract_first()
			rank = row.xpath('./td[@class="chart-table-position"]/text()').extract_first()
			#title = row.xpath('./td[4]/strong/text()').extract_first()
			title = row.xpath('./td[@class="chart-table-track"]/strong/text()').extract_first()
			artist = row.xpath('./td[@class="chart-table-track"]/span/text()').extract_first()
			artist = artist.replace("by ", "")
			streams = row.xpath('./td[@class="chart-table-streams"]/text()').extract_first()
			streams = int("".join(streams.split(",")))

			item = SpotifyWklyItem()
			item['start_date'] = start_date
			item['end_date'] = end_date
			item['rank'] = rank
			item['title'] = title
			item['artist'] = artist
			item['streams'] = streams

			yield item