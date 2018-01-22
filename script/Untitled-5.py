#!/usr/bin/env python
#coding:utf-8
import re,sys
import urllib
from bs4 import BeautifulSoup
global r_url

def hq_url():
    so_url = "http://movie.douban.com/subject_search?search_text="
    data = urllib.urlopen(so_url+gjz).read()
    r = re.findall(r'<a class="nbg" href=(.*?) onclick',data)
    r_url = re.sub('"','',r[0])
    ymdata = urllib.urlopen(r_url).read()
    soup = BeautifulSoup(ymdata)
    wz = soup('span',{'property':'v:summary'})
    title = re.findall(r'name="title" value="(.*?)"',ymdata)
    zy = re.findall(r'name="desc" value="(.*?)"',ymdata)
    imdb = re.findall(r':</span> <a href="(.*?)" target=',ymdata)

    shijian = re.findall(r'<span property="v:runtime" content="109">(.*?)</span>',ymdata)
    print u"IMDB电影网链接"
    print imdb
    print u"豆瓣电影链接"
    print  r_url
    print '*'*70
    print title[0]
 print zy[0]
    print "电影简介"
    print '*'*70
    print wz
if __name__=='__main__':
    gjz=raw_input("请输入电影名:  ").strip()
    hq_url()