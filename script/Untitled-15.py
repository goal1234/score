# -*- coding: utf-8 -*-
from requests.packages.urllib3.exceptions import InsecureRequestWarning
import threading
import requests
import time
import csv
import os
import re

def getPage(html, url, headers, params = {}, referer = ''):
    flags = True
    if (url[:5] == 'https'):
        flags = False
    headers['Referer'] = referer
    response = html.get(url, headers = headers, params = params, verify = flags)
    page = response.content
    return page

def find(string, page, flags = 0):
    pattern = re.compile(string, flags = flags)
    results = re.findall(pattern, page)
    return results

def work(html, url, headers, cnt):
    tmp = ''
    for q in url:
        if q != '\\':
            tmp = tmp + q
    url = tmp
    page = getPage(html, url, headers)
    types = find(r'<span property="v:genre">(.+?)</span>', page)
    global mutex, rec
    mutex.acquire()
    print cnt
    for item in types:
        if rec.has_key(item):
            rec[item] += 1
        else:
            rec[item] = 1
    mutex.release()

def init():
    html = requests.session()
    doubanUrl = 'https://movie.douban.com'
    headers={'User-Agent':'Mozilla/5.0 (compatible; MSIE 5.5; Windows NT)'}
    page = getPage(html, 'https://movie.douban.com/j/search_subjects', headers, params = {'type': 'movie', 'tag': '热门', 'sort': 'time', 'page_limit': '400', 'page_start': '0'})
    results = find(r'"rate":"(.+?)",.+?"title":"(.+?)","url":"(.+?)"', page)
    urls = [item[2] for item in results]
    rates = [item[0] for item in results]
    titles = [item[1] for item in results]

    for i in xrange(len(urls)):
        for j in xrange(i + 1, len(urls)):
            if (rates[i] < rates[j]):
                rates[i], rates[j] = rates[j], rates[i]
                urls[i], urls[j] = urls[j], urls[i]
                titles[i], titles[j] = titles[j], titles[i]

    with open('douban.csv', 'wb') as csvfile:
        spamwriter = csv.writer(csvfile, dialect = 'excel')
        for i in xrange(len(rates)):
            spamwriter.writerow([titles[i], urls[i], rates[i]])
    global mutex, rec
    mutex = threading.Lock()
    rec = {}
    jobs = []
    cnt = 0
    for i in xrange(len(urls)):
        cnt += 1
        job = threading.Thread(target = work, args = (html, urls[i], headers, cnt))
        job.start()
        jobs.append(job)

    for job in jobs:
        job.join()

    with open('douban_type.csv', 'wb') as csvfile:
        spamwriter = csv.writer(csvfile, dialect = 'excel')
        for key in sorted(rec.keys(), reverse = True):
            spamwriter.writerow([key, rec[key]])

if __name__ == '__main__':
    requests.packages.urllib3.disable_warnings(InsecureRequestWarning)
    init()