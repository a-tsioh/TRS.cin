import pandas as P
import psycopg2 as pg2
import re

db = pg2.connect(dbname="lils")

dataframe = P.read_sql_query("SELECT * FROM lectures WHERE langue='hokkien' AND text != '' ",db)

for i, row in dataframe.iterrows():
    hj = row['forme'].decode('utf8')
    tl = row['text'].decode('utf8')
    tl = tl.replace(u'i\u030d', u'\u0131\u030d') # remove the dot on the i when 8th tone
    norm = row['normalisation'].split("_")
    keys = "".join(norm)
    print "\t".join([keys, hj, "1"]).encode("utf8")
    print "\t".join([keys, tl, "1"]).encode("utf8")
    print "\t".join([re.sub("[0-9]", "", keys), hj, "1"]).encode("utf8")
    print "\t".join([re.sub("[0-9]", "", keys), tl, "1"]).encode("utf8")


