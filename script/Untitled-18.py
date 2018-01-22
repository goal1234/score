import smtplib
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.MIMEMultipart import MIMEMultipart
from email import Encoders,Utils
from email.Header import Header
#author Rocky Chen
class MailAtt():
def __init__(self,smtp_server,from_mail,password,to_mail):
self.server=smtp_server
self.username=from_mail.split("@")[0]
self.from_mail=from_mail
self.password=password
self.to_mail=to_mail

def send_txt(self,filename):
self.smtp=smtplib.SMTP()
self.smtp.connect(self.server)
self.smtp.login(self.username,self.password)
self.msg=MIMEMultipart()
self.msg['to']=self.to_mail
self.msg['from'] =self.from_mail
self.msg['Subject']="Convert"
self.filename=filename+ ".txt"
self.msg['Date']=Utils.formatdate(localtime = 1)
content=open(self.filename.decode('utf-8'),'rb').read()
print content
self.att=MIMEText(content,'base64','utf-8')
self.att['Content-Type']='application/octet-stream'
#self.att["Content-Disposition"] = "attachment;filename="%s"" %(self.filename.encode('gb2312'))
self.att["Content-Disposition"] = "attachment;filename="%s"" % Header(self.filename,'gb2312')
print self.att["Content-Disposition"]
self.msg.attach(self.att)


然后是创建一个抓取知乎文章的爬虫函数

import smtplib
import time,os
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from bs4 import BeautifulSoup
from email.MIMEBase import MIMEBase
from email import Encoders
import urllib2,urllib,time,datetime,codecs,sys,re,sys
from mail_template import MailAtt
reload(sys)
sys.setdefaultencoding('utf-8')
#author Rocky Chen

def getAnswer(answerID):
host="http://www.zhihu.com"
url=host+answerID
print url
user_agent="Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"
header={"User-Agent":user_agent}
req=urllib2.Request(url,headers=header)
resp=urllib2.urlopen(req)

#save2file("temp.txt",resp.read())
#title=

#resp=open("temp.txt",'r')
bs=BeautifulSoup(resp,"html.parser")
title=bs.title

filename_old=title.string.strip()
print filename_old
filename = re.sub('[/:*?"<>|]','-',filename_old)
save2file(filename,title.string)
title_content=title.string

answer=[]

detail=bs.find("div",class_="zm-editable-content")

save2file(filename,"nn--------------------Detail----------------------nn")
#save detail content

for i in detail.strings:
#print i
#clean_tag=i.strings
#print i.strings
#print i
save2file(filename,unicode(i))


answer=bs.find_all("div",class_="zm-editable-content clearfix")
k=0
for each_answer in answer:

save2file(filename,"nn-------------------------answer %s -------------------------nn" %k)
for a in each_answer.strings:
#clean_a_tag=a.strings
#print a
save2file(filename,unicode(a))
k+=1


smtp_server='smtp.126.com'
from_mail='www.rcdisk.com'
password='www.rcdisk.com'
to_mail='www.rcdisk.com@kindle.cn'
send_kindle=MailAtt(smtp_server,from_mail,password,to_mail)
#sub_folder=os.path.join(os.getcwd(),"content")
#filename_path=os.path.join(sub_folder,filename+".txt")
print filename
send_kindle.send_txt(filename)



if __name__=="__main__":
sub_folder=os.path.join(os.getcwd(),"content")
if not os.path.exists(sub_folder):
os.mkdir(sub_folder)

os.chdir(sub_folder)
id=sys.argv[1]
id_link="/question/"+id
getAnswer(id_link)

print "Done" 