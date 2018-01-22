#-*- coding: UTF-8 -*-  
import sys  
import time  
import urllib  
import urllib2  
import requests  
#import numpy as np  
from bs4 import BeautifulSoup  
from openpyxl import Workbook  
  
reload(sys)  
sys.setdefaultencoding('utf8')  
  
  
  
#Some User Agents  
hds=[{'User-Agent':'Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.1.6) Gecko/20091201 Firefox/3.5.6'},\  
{'User-Agent':'Mozilla/5.0 (Windows NT 6.2) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.12 Safari/535.11'},\  
{'User-Agent': 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)'}]  
  
  
def book_spider(book_tag,page_num):  
    book_list=[]  
    items_each_page=15  
    page_count=0  
    while True:  
        url='http://www.douban.com/tag/'+urllib.quote(book_tag)+'/book?start='+str(page_count*items_each_page)      
        try:  
            req=urllib2.Request(url,headers=hds[page_num%len(hds)])  
            source_code=urllib2.urlopen(req).read()  
            plain_text=str(source_code)  
        except(urllib2.HTTPError, urllib2.URLError), e:  
            print e  
          
        soup=BeautifulSoup(plain_text)  
          
        list_soup=soup.find('div',{'class':'mod book-list'})  
        #print list_soup  
        print u'正在下载第%d页'%(page_count+1)  
        for book_info in list_soup.findAll('dd'):  
            title=book_info.find('a',{'class':'title'}).string.strip()  
            desc=book_info.find('div',{'class':'desc'}).string.strip()  
            desc_list=desc.split('/')  
            #print desc_list  
            book_url=book_info.find('a',{'class':'title'}).get('href')  
            #print book_url  
  
            try:  
                author_info='作者/译者：' + '/'.join(desc_list[0:-3])  
            except:  
                author_info='作者/译者：暂无'   
            try:  
                pub_info = '出版信息： ' + '/'.join(desc_list[-3:])  
            except:  
                pub_info='出版信息:暂无'  
            try:  
                rating=book_info.find('span',{'class':'rating_nums'}).string.strip()  
            except:  
                rating=0.0  
             
            people_num=get_people_num(book_url)  
              
             
            book_list.append([title,rating,people_num,author_info,pub_info])  
  
        page_count+=1  
         
        if page_count==page_num:  
            break  
  
    return book_list  
  
  
  
  
def get_people_num(url):  
    #url='http://book.douban.com/subject/6082808/?from=tag_all' # For Test  
    try:  
        req = urllib2.Request(url, headers=hds[2])  
        source_code = urllib2.urlopen(req).read()  
        plain_text=str(source_code)     
    except (urllib2.HTTPError, urllib2.URLError), e:  
        print e  
    soup = BeautifulSoup(plain_text)  
    people_num=soup.find('div',{'class':'rating_sum'}).findAll('span')[1].string.strip()  
    return people_num  
  
  
  
  
def print_to_excel(book_tag,page_num):  
    book_list=book_spider(book_tag,page_num)  
    book_list=sorted(book_list,key=lambda x:x[1],reverse=True)  
  
    wb=Workbook()  
    ws=wb.create_sheet()  
    ws.append(['序号','书名','评分','评价人数','作者','出版社'])  
    count=1  
    for item in book_list:  
        ws.append([count,item[0],item[1],item[2],item[3],item[4]])  
        count+=1  
    save_path='book-list-'+book_tag.decode()+'.xlsx'  
    wb.save(save_path)  
  
  
book_tag='编程'  
page_num=10  
print_to_excel(book_tag,page_num) 