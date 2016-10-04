Building the Next Word Prediction Engine from scratch. 
========================================================
author: D.Dakhno
date: 02. Oct. 2016
autosize: true
font-family: 'Helvetica'
css: bootstrap.cerulean.css
transition: rotate
width: 1920
height: 1080



JHU Data Science Capstone Project (in cooperation with SwitfKey)



Challenges of real world English analysis or "Garbage In, Garbage Out"
========================================================

**Have fun! 頑張ってね～～**


*Hit Chart Elegy (音楽番付哀歌)  
Just Like Great Britain’s–Unfortunately Bibi on Fox News Две ракеты были выпущены по южной территории #Израиль из #СекторГаза  
★ 12月30日 Awoi Instore at Zeal Link Shibuya`  
...fermented red beancurd or hong fu ru 红腐乳...  
...to order their debut EP, 플레이걸의 24時.  
Xi Lu Han is the full name of Lu Han (루한 / 鹿 晗)  
..LOL we may be getting an adlut~~~~♥♥ I hope !  
He loves YOU!(((: #His<3isbettter*


Regular English by itself offers fast endless plenty of grammatical, syntactical and lexical forms and combinations with extremely 
rich  semantical background. In its function of *lingua franca* in the times of globalisation and Internet, it multiplies the variability of the lexical forms an analysts has to deal with.

The most challenging step in the course of analysis of a given text [HC corpus](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) for the purposes of prediction is therefore
preprocessing of the text, trying to remove the "garbage", reduce the variablity but avoiding extreme bias as result. The corpus includes patterns 
from different segments of Internet (news, blogs, Twitter).

The main steps of text preprocessing in this porject were:
- removing numbers
- trimming excessive white spaces
- removing lines with words from languages like chinese, japanese, russian, hebrew and so on
- converting the punctuation marks to standard where possible
- removing "words" consisting of non-letter characters (hashtags accepted)
- reducing variability through leaving out articles (the, a , an)

Predictive model and algorithm
========================================================

 [N-Gramms](https://en.wikipedia.org/wiki/N-gram) model has been choosen as a basis for prediction engine. An n-gram is a contiguous sequence of n items from a given sequence of text or speech. N-gram models are widely used in statistical natural language processing. They are typically are collected 
 from a text or speech corpus. So did we as well ([HC Corpus](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip), available over Coursera/SwiftKey). 

The following priciples were ipmlemented for building the n-gram databases:
- Building data bases for mono, bi-, tri-, four- and pentagragms (for future usage as one-, two-, three- and four-word predictors and in-word predictor). 
- End of the sentence or the line  was used ass a natural break point building of n-gram (with respect to the nature of the text corpus - one message per line).
- Data bases were packed by the length in the R data.table format, including the predictor (all words of n-gram but last), outcome (last word of n-gram) and the detected frequency. The format is directly useable by prediction engine.
- Because of high memory consumption only 15% of the text corpus can be processed at a time. With random sampling in 21 processing iterations has been reached the coverage of above 97% of lines . The n-gram databases were then merged into one per n-gram length.
- Working data bases were created, trimming the over-represented predictors (like "to be") to upper 150 outcomes by frequence and removing extermely rare combinations.

Using the data bases created, following predictive model has been implemented:
- Maximal length of the expression took as a predictor is four words.
- As the first step, the decision is made towards either next-word or in-word search (trailing blanks in the input string?), a flag for possible 
article as a last word will be set. 
- For next-word search the line  trimmed the same way as the text corpus is used as the subject of exact search in the data tables, from longer to shorter predictors downwards. Each of the following steps to be made, when preceding one still brings nothing.
- Exact search after trimming by endings like "'ll", "'re" and so on.
- False input of the last word (typo?) assumed; the search is to be repeated iteratively, shortening the last word by one letter.
- The original predictor expression is shortened by the first word; the rest becomes the subject of exact search and steps, listed above.
- The last word standing goes into exact search in the used words database, then into in-word prediction.
- The in-word prediction make the search in the data base used words and following in the reference word list for the words, beginning with the 
given pattern. 
- The relative frequency of the findings in the data bases and evtl. preceding n-gram (for in-word prediction) counts by the sorting of the output (descending). The arrangement will be represented as suggestions in the front end.



Implementation and features
========================================================

![Application screenshot](/home/d/R-Work/CapstoneProject/figures/app_screenshot.png)

#### *No comment*

Benchmarking prediction engine
========================================================


Benchmarking of statistical and technical performance of the next word prediction engine was conducted using the [N-grams](http://www.ngrams.info/intro.asp) based on the 
[Corpus of Contemporary American English](http://corpus.byu.edu/coca/) (million most frequent pentagrams,  case insensitive).


```
    Mbyte nr.mono   nr.bi   nr.tri  nr.four nr.penta accuracy by_ranking
1  932.87  292005 1576301  4535312  3652975  5320886   0.5981     0.5548
2 4141.19  422469 3379408 16324126 21729539 20934044   0.6009     0.5319
3 9078.07  861463 5360592 32419383 48965451 49787933   0.6546     0.5281
4 8262.96  292005 1576301 16324126 48965451 49787933   0.6523     0.5247
5 1603.60  861463 5360592 16324126  3652975  5320886   0.6058     0.5572
  mean_time median_time sd_time
1    0.0087       0.005  0.0317
2    0.0082       0.005  0.0414
3    0.0087       0.004  0.0799
4    0.0071       0.004  0.0321
5    0.0102       0.005  0.0789
```
