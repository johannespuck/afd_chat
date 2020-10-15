# -*- coding: utf-8 -*-
"""
Created on Mon Sep 16 09:59:00 2019

@author: DerJo
"""

import pandas as pd


filepath="C:/Users/Jo/Dropbox/Textanalyse/afd_chat/"

afd_chat=pd.read_csv(filepath + "afd.chat.protocol.v2.csv",sep=";")
afd_chat.text_plain.head
afd_chat.text_plain.value_counts()
mandy_text = afd_chat[afd_chat.verfasser_plain == "Mandy Afd"]
andre_text= afd_chat[afd_chat.verfasser_plain == "Andre Poggenburg"]

verfasser=afd_chat.verfasser_plain.unique()
print(verfasser)

# Anzahl von posts pro verfasser
post_count=list((range(1,len(verfasser)+1)))
j=0
for i in verfasser:
    post_count[j]=(afd_chat[afd_chat.verfasser_plain == i].verfasser_plain.value_counts())
    j=j+1
    
# eleganter gelöst als mit for-loop
post_count2 = afd_chat.groupby(["verfasser_plain"])
post_count3=post_count2.size()
# speicher resultat als dataframe
post_count4 = pd.DataFrame(post_count3, columns = ["post_count"])
post_count4.sort_values("post_count", inplace=True, ascending=False)
post_count4["post_count"].head(10)


# words of interest
woi=("asyl","flücht","euro","deutsch","stolz","national","patriot","merkel","höcke","petry","lucke")
woi_count=[]

for x in woi:
    
    j=0
    word_count=0
    
    word_count=afd_chat['text_plain'].str.lower().str.count(x).sum()
    
    print(x)        
    print(word_count)
    woi_count.append([x,word_count])

woi_count = pd.DataFrame(woi_count, columns = ["term","Frequency"])

                        
