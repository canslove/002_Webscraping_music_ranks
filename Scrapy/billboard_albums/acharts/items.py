# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy

target = 2 # 1(single), 2(albums)
# change intto acharts_spider.py (url)
# change into pipelines.py (csv)

class AchartsItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
	date = scrapy.Field()
	rank = scrapy.Field()
	title = scrapy.Field()
	artist = scrapy.Field()
	weeks = scrapy.Field()