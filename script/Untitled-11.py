#coding=utf-8
from selenium import webdriver
from time import sleep
 
class DoubanPopularBook:
 
    def __init__(self):
        self.dr = webdriver.Chrome()
        self.popular_books_list = self.get_douban_popular_books()
 
    def get_douban_popular_books(self):
        self.dr.get('https://book.douban.com/')
        sleep(3)
        popular_books_list = [] #定义一个空list用于存放获取的书籍信息
        i = 0
        while i < 10:
            book_info = self.dr.find_elements_by_css_selector("[class='list-col list-col2 list-summary s']>li")[i].text #通过css用class属性和标签li组合来获取书籍所有文本信息
            popular_books_list.append(book_info.split('\n')) #向空list追加书籍信息用并换行符隔开
            i += 1
        popular_books_list.sort(key=lambda x:float(x[1][0:3]), reverse=True) #用sort中key方法根据书籍评分从高到低进行排序
        #popular_books_list = sorted(popular_books_list, key=lambda book: book[1][0:3], reverse=True)
        return popular_books_list
 
    def get_popular_books_rank_file(self):
        self.file_title = '豆瓣最受关注图书榜之评分排行'
        self.file = open(self.file_title + '.txt', 'wb')
        for item in self.popular_books_list:
            separate_line = '~~~~~~~~~~~~~~~~~~~~~~~~\n'
            self.file.write(separate_line.encode('utf-8'))
            self.file.write(('书籍名称:'+item[0]+'\n').encode('utf-8'))
            self.file.write(('评分:'+item[1]+'\n').encode('utf-8'))
            self.file.write((item[2]+'\n').encode('utf-8'))
            self.file.write(('体裁:'+item[3]+'\n').encode('utf-8'))
            if item[4] == '有电子书':
                self.file.write(('一句话评论:'+item[5]+'\n').encode('utf-8'))
            else:
                self.file.write(('一句话评论:'+item[4]+'\n').encode('utf-8'))
        self.file.close()
 
 
    def quit(self):
        self.dr.quit()
 
if __name__ == '__main__':
    popular_books = DoubanPopularBook()
    popular_books.get_popular_books_rank_file()
    popular_books.quit()
     
方法二：
 
#coding=utf-8
from selenium import webdriver
from time import sleep
 
class DoubanPopularBook:
 
    def __init__(self):
        self.dr = webdriver.Chrome()
        self.popular_books_list = self.get_douban_popular_books()
 
    def get_douban_popular_books(self):
        self.dr.get('https://book.douban.com/')
        sleep(3)
        popular_books_list = [] #定义一个空list用于存放获取的书籍信息
        i = 0
        while i < 10:  #总共10本书
            book_name = self.dr.find_elements_by_xpath("//h4[@class='title']/a")[i].text #定位书籍名称
            book_grade = self.dr.find_elements_by_css_selector('.average-rating')[i].text #定位评分
            book_auther = self.dr.find_elements_by_xpath("//p[@class='author']")[i].text #定位作者
            book_genre = self.dr.find_elements_by_css_selector('.book-list-classification')[i].text #定位体裁
            book_comment = self.dr.find_elements_by_css_selector('.reviews')[i].text #定位一句话评论
            popular_books_list.append([book_name, book_grade, book_auther, book_genre, book_comment]) #向空list追加书籍信息
            i += 1 #每本书籍间隔为1
        popular_books_list = sorted(popular_books_list, key=lambda x:float(x[1]), reverse=True) #用sorted方法按评分从高到低排序
        return popular_books_list
 
    def get_popular_books_rank_file(self):
        self.file_title = '豆瓣最受关注图书榜之评分排行'
        self.file = open(self.file_title + '.txt', 'wb')
        for item in self.popular_books_list:
            separate_line = '~~~~~~~~~~~~~~~~~~~~~~~~\n'
            self.file.write(separate_line.encode('utf-8'))
            self.file.write(('书籍名称：'+item[0]+'\n').encode('utf-8'))
            self.file.write(('评分：'+item[1]+'\n').encode('utf-8'))
            self.file.write((''+item[2]+'\n').encode('utf-8'))
            self.file.write(('体裁：'+item[3]+'\n').encode('utf-8'))
            self.file.write(('一句话评论：'+item[4]+'\n').encode('utf-8'))
        self.file.close()
 
    def quit(self):
        self.dr.quit()
 
if __name__ == '__main__':
    popular_books = DoubanPopularBook()
    popular_books.get_popular_books_rank_file()
    popular_books.quit()