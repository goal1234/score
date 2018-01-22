#!/usr/bin/python
# -*-coding:utf-8-*-
# Python:        2.7
# Platform:      Mac
# Author:        wucl(wucl202000@hotmail.com)
# Program:       爬取豆瓣电影
# Version:       0.1
# History:       2017.8.21
 
#还有一个比较大的问题是如果爬了几次，IP会被豆瓣封掉
from bs4 import BeautifulSoup
import urllib2, json, random, sys
 
reload(sys)
sys.setdefaultencoding('utf-8')
 
def get_data(url):
    my_headers = [
        'Mozilla/5.0 (Windows NT 5.2) AppleWebKit/534.30 (KHTML, like Gecko) Chrome/12.0.742.122 Safari/534.30',
        'Mozilla/5.0 (Windows NT 5.1; rv:5.0) Gecko/20100101 Firefox/5.0',
        'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.2; Trident/4.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727; .NET4.0E; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET4.0C)',
        'Opera/9.80 (Windows NT 5.1; U; zh-cn) Presto/2.9.168 Version/11.50',
        'Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN) AppleWebKit/533.21.1 (KHTML, like Gecko) Version/5.0.5 Safari/533.21.1',
        'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; .NET4.0E; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET4.0C)']
    header = {"User-Agent": random.choice(my_headers)}
    req = urllib2.Request(url, headers=header)
    html = urllib2.urlopen(req).read()
    data = json.loads(html)['data']
    return data
 
def get_movieInfo(url):
    my_headers = [
        'Mozilla/5.0 (Windows NT 5.2) AppleWebKit/534.30 (KHTML, like Gecko) Chrome/12.0.742.122 Safari/534.30',
        'Mozilla/5.0 (Windows NT 5.1; rv:5.0) Gecko/20100101 Firefox/5.0',
        'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.2; Trident/4.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727; .NET4.0E; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET4.0C)',
        'Opera/9.80 (Windows NT 5.1; U; zh-cn) Presto/2.9.168 Version/11.50',
        'Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN) AppleWebKit/533.21.1 (KHTML, like Gecko) Version/5.0.5 Safari/533.21.1',
        'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.04506.648; .NET CLR 3.5.21022; .NET4.0E; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET4.0C)']
    header = {"User-Agent": random.choice(my_headers)}
    req = urllib2.Request(url, headers=header)
    html = urllib2.urlopen(req).read()
    soup = BeautifulSoup(html, 'html.parser')
    movie = {}
    movie['Name'] = soup.find('span',property="v:itemreviewed").text
    movie['Year'] = soup.find('span',class_='year').text
    movie['Rate'] = soup.find('strong', property="v:average").text
    movie['Runtime'] = soup.find('span', property="v:runtime").text
    movie['Summary'] = soup.find('span', property='v:summary').text
    movie['URL'] = url
 
    movie['Directors'] = ''
    directors = soup.find_all('a', rel="v:directedBy")
    for director in directors:
        movie['Directors'] += director.text
        movie['Directors'] += '  '
 
    movie['Stars'] = ''
    stars = soup.find_all('a', rel="v:starring")
    for star in stars:
        movie['Stars'] += star.text
        movie['Stars'] += '  '
 
    movie['Category'] = ''
    categorys = soup.find_all('span', property="v:genre")
    for category in categorys:
        movie['Category'] += category.text
        movie['Category'] += '  '
 
    return movie
 
def get_urls():
    base_url = 'https://movie.douban.com/j/new_search_subjects?sort=R&range=1,10&tags=%E7%94%B5%E5%BD%B1&start='
    urls=[]
    nu = 0
    while True:
        print nu
        url = base_url + str(nu)
        data = get_data(url)
#        print data
        if len(data) == 0:
            break
        for i in data:
            urls.append(i['url'])
        nu += 20
    return urls
 
if __name__ == '__main__':
    urls = get_urls()
    f = open('movieinfo.txt','w+')
    for url in urls:
        try:
            movie = get_movieInfo(url)
            movie_js = json.dumps(movie)
            f.write(movie_js)
            f.write('\n')
            f.flush()
        except:
            print url
            continue
    f.close()