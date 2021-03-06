

#记录下_φ(❐_❐✧ 人丑就要多读书
---
## 步骤:
    1．确定分类变量。
	2．异常数据处理 -> 剔除，填充，插值等进入重复步骤。
	3．计算KS, IV，WOE的值
		- 分类的标签在其他数据上分的好不好， 标签区别其他变量的强度
	4．建立模型，变量选取AUC,p,bias
	5．在区间内出现（0,1）出现频率的情况进行切割（分箱）               
	6．交叉验证，时间内外
	7．寻找点进行woe归一化					 
	8．将归一化的结果进行回归
	9．求取odds和评分
	10．得到评分卡片  
	11．模型校准
---
## 向别人学习
### R版本

[build scorecard with R](https://github.com/frankhlchi/R-scorecard)

[scorecard](https://github.com/ShichenXie/scorecard)

[GGCM Model Performance Scorecard](https://github.com/greatgray/scorecard)

[GGCM Scorecard](https://github.com/greatgray/ggcm-scorecard)

[Credit risk scorecards development using R](https://github.com/Cary-Chan/CASE_Credit-risk-scorecards-development-using-R)

---

### Python版本
[ScoreCardModel](https://github.com/data-science-tools/ScoreCardModel)

[PerformanceScoreCard](https://github.com/arpitg91/PerformanceScoreCard)

[WoE Transformation](https://github.com/boredbird/woe)

[Credit-Card-Scorcard](https://github.com/LukeNisbet/Credit-Card-Scorcard)

[scorecard](https://github.com/weiwujiang/scorecard)
[ScorecardBox](https://github.com/chinasiten/ScorecardBox/tree/master/core)


---
## 文章
[Building a Scorecard in Practice](http://www.aiecon.org/conference/2008/CIEF/Building%20a%20Scorecard%20in%20Practice.pdf)

[Credit Scoring Modellingfor Retail Banking Sector](http://www.doc88.com/p-9532309785147.html)

[Credit Scoring, Statistical Techniques and Evaluation Criteria](http://www.doc88.com/p-2873904363616.html)



---
## 备注

R里面的glm.fit()是通过irls进行的参数估计，SAS是通过mle进行的参数估计。