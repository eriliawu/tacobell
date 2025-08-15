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
            .query('product.str.contains("^(?!.*(AWR| AW|AW,|AW |BYB|KFC|LJS|PH |PIZZA HUT|TCBY|ICBIY|KRYSTAL|* NEW PRODCT ADDED BY|COMBO|FRANCHISE LOCAL MENU|SPECIAL PROMOTION|NEW ITEM|TB I\'M ALL EARS|DO NOT ALTER THIS ITEM|BORDER SWEAT SHIRT|TB I\'M THINKING YOU ME|CFM DOWNLOAD 1|TB HELLO FRIEND|CANADA BATMAN CUP INDIVIDUAL|DELETED ITEM, DO NOT USE|CLEV INDIANS/TB BANDANNA 1.4|CFM DOWNLOAD 2|TB I\'M THINKING YOU ME DINNER|CANADA BATMAN CUP W/PURCHASE|TB HELLO FRIENDGC REFUND|TB EAT IN CAR)).*$",na=False)',engine='python'))
"""
# or do the second part of the operation like this, which doesnt involve regular expression
product = product[~product['product'].str.contains('NEW')]
"""
del group

# read correcyed string file
strings = pd.read_csv('data/menu-matching/product-names_unique_substrings_bow_corrected.csv',
                      usecols=[0,3])