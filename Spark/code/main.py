import pandas as pd
import numpy as np
import findspark
import pyspark
from pyspark import SparkContext
from pyspark.sql.session import SparkSession
from pyspark import SparkConf

sc = SparkContext('local')
spark = SparkSession(sc)

if __name__ == '__main__':
    print('Lets start with Spark!')

    raw_data = spark.read.load("../data/raw_data/raw_data.csv",format="csv", sep=",", inferSchema="true", header="true")
    print(raw_data.show(2))