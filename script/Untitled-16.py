    # -*- coding: utf-8 -*-  
    import urllib2  
    import re  
    from bs4 import BeautifulSoup  
    import sys  
    reload(sys)  
    sys.setdefaultencoding("utf-8")  
      
    class movies:  
        def __init__(self,baseUrl):  
            self.url=baseUrl  
            self.title=[]  
            self.info=[]  
      
        def getInfo(self):  
            try:  
                con=''  
                for i in range(0,250,25):  
                    response=urllib2.urlopen(self.url+str(i)+'&fileter=')  
                    con=con+response.read()  
            except urllib2.URLError,e:  
                if hasattr(e,"reason"):  
                    print u"链接失败，错误原因",e.reason  
            html_text=con  
            soup=BeautifulSoup(html_text,'html.parser',from_encoding='UTF-8')  
      
            link_temp=soup.find_all('a',href=re.compile(r'subject'))  
            link_node=[]  
            for i in range(0,500,2):  
                link_node.append(link_temp[i])  
      
            file = open('F:/PythonFile/movies.txt', 'w')  
            for enter in link_node:  
                try:  
                    res=urllib2.urlopen(enter['href'])  
                    print res.getcode()  
                    infomation=res.read()  
                    soup1=BeautifulSoup(infomation,'html.parser',from_encoding='UTF-8')  
                    self.info=soup1.find_all('div',id=re.compile(r'info'))  
                    self.title=soup1.find_all('title')  
      
                    each_info = self.info  
                    each_title = self.title  
      
                    for link_t in each_title:  
                        file.write(link_t.get_text())  
                        file.seek(0, 2)  
      
                    for link_i in each_info:  
                        file.write(link_i.get_text())  
                        file.seek(0, 2)  
                except urllib2.URLError,e:  
                    print "找不到这个网页"  
            file.close()  
      
    url='https://movie.douban.com/top250?start='  
    mov=movies(url)  
    mov.getInfo()  