import scrapy
from scrapy.selector import Selector
from scrapy.http import HtmlResponse

# call spider with
# scrapy crawl myspider -a filename=urls_test.txt

class MySpider(scrapy.Spider):
    name = "myspider"

    def __init__(self, filename=None):
        print("HIII*****************8")
        if filename:
            with open(filename, 'r') as f:
                print("HIII*****************8")
                self.start_urls = f.read().splitlines()
        f = open("titles.txt","w")
        f.close()
        self.i = 0
        print(self.start_urls)

    def parse(self, response):
        page = response.url.split("/")[-2]
        body = response.body
        #print(body)
        title = ''.join(Selector(text=body).xpath('//title/text()').extract())
        print(title)
        title =  "".join([line.strip() for line in title.strip().splitlines()])
        csvLine = response.url + "; " + title + "\n"
        f = open("titles.txt","a");
        f.write(csvLine)
        f.close()
