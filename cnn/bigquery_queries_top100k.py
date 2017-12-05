from google.cloud import bigquery

subs = ['politics','POLITIC','news','worldnews','technology','techsupport','programming','askscience','science','food','soccer','sports','nba','pokemon','relationships','de']
sqlQueries = []
for i in subs:
    query = 'select * from [mercurial-feat-186802:reddit_top_results.0] where subreddit="' + i+'" LIMIT 100000'    sqlQueries.append(query)
    print(query)

resultsAr = []
def query_reddit_top():
    client = bigquery.Client()
    for query in sqlQueries:
        query_job = client.query(query)

        results = query_job.result()
        #resultsAr.append(results
        with open("query_results","a") as f:
            for row in results:
                f.write(row)