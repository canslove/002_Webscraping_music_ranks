from scrapy import Spider, Request
from spotify.items import SpotifyItem


class SpotifySpider(Spider):

	name = "spotify_spider"
	allowed_urls = ["https://spotifycharts.com/"]
	start_urls = ["https://spotifycharts.com/regional/us/daily/latest/"]

	# 1st region/us 
	def parse(self, response):
		datelist_top = response.xpath('//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[3]')
		
		if(1):
			links = datelist_top.xpath('./ul/li/@data-value').extract() # all date link(yyyy-mm-dd)
		else: # 'mm/dd/yyyy --> yyyy-mm-dd'
			links = datelist_top.xpath('./ul/li/text()').extract() # all date text (yyyy/mm/dd)

			def mod_form(str_):
				s = str_.split('/')
				s = [s[2],s[0],s[1]]
				return '-'.join(s)

			links = list(map(mod_form,links))

		pages = ["https://spotifycharts.com/regional/us/daily/" + link for link in links]
		#print(page)

		for url in pages:
			yield Request(url, callback=self.parse_top)


	def parse_top(self, response):
		rows = response.xpath('//*[@id="content"]/div/div/div/span/table/tbody/tr')

		date = response.url
		date = date.split("/")[-1]

		for row in rows:
			#rank = row.xpath('./td[2]/text()').extract_first()
			rank = row.xpath('./td[@class="chart-table-position"]/text()').extract_first()
			#title = row.xpath('./td[4]/strong/text()').extract_first()
			title = row.xpath('./td[@class="chart-table-track"]/strong/text()').extract_first()
			artist = row.xpath('./td[@class="chart-table-track"]/span/text()').extract_first()
			artist = artist.replace("by ", "")
			streams = row.xpath('./td[@class="chart-table-streams"]/text()').extract_first()
			streams = int("".join(streams.split(",")))

			item = SpotifyItem()
			item['date'] = date
			item['rank'] = rank
			item['title'] = title
			item['artist'] = artist
			item['streams'] = streams

			yield item