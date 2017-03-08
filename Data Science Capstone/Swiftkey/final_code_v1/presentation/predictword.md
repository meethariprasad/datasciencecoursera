Predict Next Word
========================================================
author: Hari Prasad
date: March-07-2017
autosize: true

Objective & Final Product
========================================================

This project is intended to create a product which can predict next word based on the words entered earlier(you might have already seen it in modern keyboards in mobile!).  

Following links will take you to the application and codebase, and following slides after this briefly explain how this product has been built.

- [Application](https://meethariprasad.shinyapps.io/predictword/): This link takes you to shinyapps where product is hosted.
- [Code](https://github.com/meethariprasad/datasciencecoursera/tree/master/Data%20Science%20Capstone/Swiftkey/final_code): This link takes you to github repository where final code is hosted.


Data Load and Cleansing
========================================================
- 7% of Random Sample from Twitter, Blog and News Data are used.
- Data has been cleansed with special character removal, spell checks, remove stop words and repeated phrases.
- This cleansed data is further used to create models to predict words.

Model Evaluvation: Simple Ngram Model
========================================================
- Cleansed Data is loaded to create ngrams from 2 to 6.
- An algorithm is written to check the frequency of matching query and corresponding n(th) gram word starting from 6 gram and if not then going to next ngram all the way up to bigram.
- Most occuring word will be predicted first, followed by others.

Model Evaluvation: GloVe Algorithm
========================================================
- [GloVe](http://nlp.stanford.edu/pubs/glove.pdf) is an unsupervised learning algorithm for obtaining vector representations for words. [Text2Vec](http://text2vec.org/glove.html) package has GloVe Implementation.
- The query will be checked for nearest word vectors and word vector with cosintope distance will be considered as most probable word and next distance and so on.
- Final model is combination of Glove & Backoff Ngram Model.
