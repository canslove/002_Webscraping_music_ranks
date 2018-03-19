# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class SpotifyWklyItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
	start_date = scrapy.Field()
	end_date = scrapy.Field()
	rank = scrapy.Field()
	title = scrapy.Field()
	artist = scrapy.Field()
	streams = scrapy.Field()
