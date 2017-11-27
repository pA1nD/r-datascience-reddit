import scrapy

class MySpider(scrapy.Spider):
    name = "myspider"

    def __init__(self, filename=None):
        print("HIII*****************8")
        if filename:
            with open(filename, 'r') as f:
                print("HIII*****************8")
                self.start_urls = f.read().splitlines()
        print(self.start_urls)
