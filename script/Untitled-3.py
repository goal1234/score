#encoding: utf-8
import requests
from pyquery import PyQuery as pq
import csv

attrs = [u'超链接', u'名称', u'评分', u'导演', u'编剧', u'主演', u'类型', u'制片国家/地区', u'语言', u'上映日期', u'片长', u'又名', u'IMDb链接']

'''
获取电影详情
'''
def attch_info(info, text, key, value):
    text = text.strip(' ')
    if text:
        if text in attrs:
            if key and value:
                info[key] = ' '.join(value)
            key = text
            value = []
        else:
            value.append(text)

    return info, key, value


'''
解析电影信息
'''
def parse_movie_info(text, info):
    key = None
    value = []

    for e in text.split(':'):
        e = e.strip()
        pos = e.rfind(' ')
        if -1 == pos:
            info, key, value = attch_info(info, e, key, value)
        else:
            info, key, value = attch_info(info, e[:pos], key, value)
            info, key, value = attch_info(info, e[pos:], key, value)

    if key not in info:
        info[key] = ' '.join(value)


'''
解析电影页面
'''
def crawl_info(url):
    info = {}
    print url
    response = requests.get(url)
    page = pq(response.content)
    content = page('div#content').eq(0)

    info[u'超链接'] = url
    info[u'名称'] = content('h1 span').eq(0).text()
    info[u'评分'] = content('div.rating_wrap strong.rating_num').text()

    info_text = content('div#info').text()
    parse_movie_info(info_text, info)

    return info


'''
获取电影列表
'''
def crawl(query_text, count):
    start = 0
    rt_list = []
    isStop = False
    url = 'https://movie.douban.com/subject_search?start={start}&search_text={query_text}&cat=1002'
    while True:
        response = requests.get(url.format(query_text=query_text.encode('utf-8', 'ignore'), start=start))
        page = pq(response.content)
        links = page('div#content table a').not_('.nbg')
        if len(links) == 0:
            isStop = True

        for link in links:
            href = pq(link).attr['href']
            rt_list.append(crawl_info(href))
            start += 1
            if len(rt_list) >= count:
                isStop = True
                break

        if isStop:
            break

    return rt_list


'''
写入文件
'''
def write_to_file(lines, path):
    with open(path, 'wb') as fhandler:
        writer = csv.writer(fhandler)
        writer.writerow(map(lambda x: x.encode('gbk', 'ignore'), attrs))
        for line in lines:
            row = []
            for key in attrs:
                row.append(line.get(key, '').encode('gbk', 'ignore'))
            writer.writerow(row)


if __name__ == '__main__':

    query_text = raw_input(u"请输入关键字:".encode('utf-8', 'ignore'))
    count = raw_input(u"请输入爬取得数据量:".encode('utf-8', 'ignore'))

    query_text = query_text.strip().decode('utf-8') if query_text.strip() else u'长城'
    count = int(count) if count.isdigit() else 10

    print u'关键字:{query_text}, 数量:{count}'.format(query_text=query_text, count=count)

    rt_list = crawl(query_text, count)
    write_to_file(rt_list, 'result.csv')


作者：imsilence
链接：https://www.jianshu.com/p/7eceedb39f3b
來源：简书
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。