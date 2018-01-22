#-*-coding:utf-8-*-
import  requests
from lxml import etree
import jieba
header ={
    "Accept":"text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding":"gzip, deflate, br",
    "Accept-Language":"zh-CN,zh;q=0.8,en;q=0.6",
    "Connection":"keep-alive",
    "Host":"movie.douban.com",
    "Referer":"https://movie.douban.com/subject/26883064/reviews?start=20",
    "Upgrade-Insecure-Requests":"1",
    "User-Agent":"Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36"
}
def getPageNum(url):
    if url:
        req = requests.get(url,headers=header)
        html = etree.HTML(req.text)
        pageNum = html.xpath(u"//div[@class='paginator']/a[last()]/text()")[0]
    return pageNum
def getContent(url):
    if url:
        req = requests.get(url, headers=header)
        html = etree.HTML(req.text)
        data = html.xpath(u"//div[@class='short-content']/text()")
    return data
 
def getUrl(pageNum):
    dataUrl= []
    for i in range(1,int(pageNum)):
        if pageNum >= 1:
            url ="https://movie.douban.com/subject/26883064/reviews?start=%d" %(((i - 1) *20),)
            dataUrl.append(url)
    return dataUrl
if __name__ == '__main__':
    url = "https://movie.douban.com/subject/26883064/reviews?start=0"
    pageNum =getPageNum(url)
    data = getUrl(pageNum)
    datas = []
    dic = dict()
    for u in data:
        for d in getContent(u):
            jdata = jieba.cut(d)
            for i in jdata:
                if len(i.strip()) > 1:
                     datas.append(i.strip())
    for i in datas:
        if datas.count(i) > 1:
            dic[i] = datas.count(i)
    for key,values in dic.items():
        print "%s===%d" %(key,values)