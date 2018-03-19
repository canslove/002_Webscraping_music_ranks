USER_AGENT_LIST = [
    'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7',
    'Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:16.0) Gecko/16.0 Firefox/16.0',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_3) AppleWebKit/534.55.3 (KHTML, like Gecko) Version/5.1.3 Safari/534.53.10'
]
# proxy for polipo
# HTTP_PROXY = 'http://127.0.0.1:8123'
# proxy for privoxy
HTTP_PROXY = 'http://127.0.0.1:8118'

# Enable or disable downloader middlewares
DOWNLOADER_MIDDLEWARES = {
    'scraper_name.middlewares.RandomUserAgentMiddleware': 400,
    'scraper_name.middlewares.ProxyMiddleware': 410,
    'scrapy.downloadermiddlewares.useragent.UserAgentMiddleware': None
}