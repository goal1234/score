# _*_ coding: utf-8 _*_
#---------------------------------------
#   程序：抓取 豆瓣妹子 图片
#   版本：0.1
#   作者：liu jia
#   日期：2014-01-07
#   语言：Python 2.7
#   说明：只是一个简单的测试，借鉴了之前开源中国的程序，为之加上了模拟请求，防止被服务器拒绝请求
#---------------------------------------

import urllib2
import os
import cookielib
from bs4 import BeautifulSoup
cookie = cookielib.CookieJar()
cookie_handler = urllib2.HTTPCookieProcessor(cookie)

header = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.72 Safari/537.36'
}

path = os.getcwd()
new_path = os.path.join(path, u'豆瓣妹子')
if not os.path.isdir(new_path):
    os.makedirs(new_path, 7777)

page_number = 1


def get_image():
    global page_number
    url = 'http://www.dbmeizi.com/?p=%s' % page_number
    site_url = 'http://www.dbmeizi.com'
    request = urllib2.Request(url=url, headers=header)
    opener = urllib2.build_opener(cookie_handler)
    response = opener.open(request)
    page = response.read()

    soup = BeautifulSoup(page)
    for image in soup.findAll('img'):
        if not image:
            exit()
        file_handler = urllib2.urlopen(site_url+image['data-bigimg']).read()
        with open(u'豆瓣妹子/'+image['data-bigimg'][-11:], 'wb') as empty_file:
            empty_file.write(file_handler)
            print image['data-bigimg']

    page_number += 1
    print '第%s页下载完毕，现在开始下载第%s页' % (page_number,page_number+1)
    get_image()

get_image()


# _*_ coding: utf-8 _*_
#---------------------------------------
#   程序：把本地文件上传到七牛云服务器
#   版本：0.1
#   作者：liu jia
#   日期：2014-01-07
#   语言：Python 2.7
#   说明：只是一个简单的测试，并且，为了自己的安全 :)
#---------------------------------------

import qiniu.conf
import sys
import os
#登录后从#https://portal.qiniu.com/setting/key获取
qiniu.conf.ACCESS_KEY = "xxxxxxxxxxxxxxxxxxx" 
qiniu.conf.SECRET_KEY = "xxxxxxxxxxxxxxxxxxx"

import qiniu.io
import qiniu.rs
policy = qiniu.rs.PutPolicy('xxxxx')# 空间名即bucket_name
uptoken = policy.token()

extra = qiniu.io.PutExtra()
extra.mime_type = "image/jpeg"

path = os.getcwd()
print path
picture_path = os.path.join(path, u'豆瓣妹子')
for item in os.listdir(picture_path):
    item = picture_path+'\\'+item
    ret, err = qiniu.io.put_file(uptoken, None, item, extra)
    print item+'---------uploaded'
if err is not None:
    sys.stderr.write('error: %s ' % err)
    exit()