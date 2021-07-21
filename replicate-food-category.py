# replicate food categorization script in python

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

os.getcwd()
os.chdir("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

# read data
product = pd.read_csv("data/from-bigpurple/product_dim.csv", sep=";", header=None,usecols=[0,1,3],
                      names=['dw_product','dw_productgroup','product'])
group = pd.read_csv('data/from-bigpurple/product_group_det.csv',sep=';',header=None,usecols=[0,2],
                    names=['dw_productgroup','group'])
product = (pd.merge(product,group,on='dw_productgroup')
            .query('product.str.contains("^(?!.*(NEW|PEPSI)).*$",na=False)',engine='python'))
"""
# or do the second part of the operation like this, which doesnt involve regular expression
product = product[~product['product'].str.contains('NEW')]
"""
del group

# read correcyed string file
strings = pd.read_csv('data/menu-matching/product-names_unique_substrings_bow_corrected.csv',
                      usecols=[0,3])