# This dataset consists of a training set of 2.38 million sentences, 
# a test set of 420.000 sentences, divided in 7 categories: “Books”, 
# “Clothing, Shoes & Jewelry”, “Electronics”, “Health & Personal Care”, 
# “Home & Kitchen”, “Movies & TV” and “Sports & Outdoors”.

from python_utils import download_file

url_train = 'https://mxnetstorage.blob.core.windows.net/public/nlp/categories_train_big.csv'
url_test = 'https://mxnetstorage.blob.core.windows.net/public/nlp/categories_test_big.csv'
print("Downloading file %s" % url_train)
download_file(url_train)
print("Downloading file %s" % url_test)
download_file(url_test)
