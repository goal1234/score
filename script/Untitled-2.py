    # -*- coding:utf-8 -*-  
    # 提取各标签列表页到excel  
    import time  
    import os.path  
    from tool.ExcelManager import listFiles,readExcel,writeExcel  
    from tool import TagManager   
    start = time.clock()  
    putplace = 'books'  
    # 判断存放位置是否存在  
    if os.path.exists(putplace):  
        pass  
    else: # 否则新建  
        print('新建图书提取存放excel处：'+putplace)  
        os.makedirs(putplace)  
    taglist = readExcel('web/booktag.xlsx') # 读取标签列表  
    del taglist[0]  
    # 对于每个标签  
    for tag in taglist:  
        # 图书按照标签存放于文件夹中  
        mulu=putplace+'/'+tag[0]  
        if os.path.exists(mulu):  
            pass  
        else:  
            os.makedirs(mulu)  
      
        excelpath = mulu+'/'+tag[1]+'.xlsx'  
        # 存在处理过的excel文件则跳过  
        if os.path.exists(excelpath):  
            print(excelpath+'已经存在')  
            continue  
      
        tagbooks = [['书籍名','URL入口','图片地址','出版信息','评价星数']] # 该标签所有书存放处  
        path = 'web/'+tag[0]+'/'+tag[1] # 构造读取文件夹入口  
        print('本地提取：'+path)  
        # 查找目录下已经抓取的Html  
        files = listFiles(path)  
        # 遍历分析  
        for i in files:  
            file = path+'/'+i  
            print('提取：'+file)  
            content = open(file,'rb').read()  
            books = TagManager.makeBookListInfo(content) # 提取图书列表  
            tagbooks.extend(books)#把书放进去  
      
        # 将信息写入本地文件中  
        writeExcel(excelpath,tagbooks)  
        print('写入成功：'+excelpath)  
    end = time.clock()  
    print("提取图书列表总共运行时间 : %.03f 秒" %(end-start))  