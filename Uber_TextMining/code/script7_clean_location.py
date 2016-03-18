import numpy as np 
import string
import re
import us
import json
import pandas as pd 
import sys, os
import nltk
from nltk.corpus import stopwords
from nltk.book import *

# this script aims to clean the locations.
def clean_punc (str):
    return ("").join(c for c in str if c not in set(r'!"#$%&\'()*+-./:;<=>?@[\\]^_`{|}~'))

def clean_punc1 (str):
    return ("").join(c for c in str if c not in set(r'!"#$%&\'()*+-./:,;<=>?@[\\]^_`{|}~'))

def clean_text(text):
    clean_text = text.split(" ")
    ret = [i.strip() for i in clean_text]
    return ret

def split_strip(loc_str):
    tmp = loc_str.split(",")
    ret = [a.strip() for a in tmp]
    return ret
 
def US_loc_lower(loc_list):
    tmp = [clean_punc(loc.lower()) for loc in loc_list]
    word_ls = [split_strip(loc) for loc in tmp]
    return word_ls

#if any element in list A match any elements in set B
def bool_loc(listA, setB):
    ret = any([a in setB for a in listA])
    return ret

def loc_ind(listA, set_list):
    for myset in set_list:
        if bool_loc(listA,myset):
            tp = list(myset)
            if len(tp[0])==2:
                return tp[0]
            else:
                return tp[1]
    return "tbd"   


def loc_ind2(listA, set_list):
    for myset in set_list:
        if bool_loc(listA,myset):
            tp = list(myset)
            if len(tp[0])<=5:
                return tp[1]
            else:
                return tp[0]

def write_state_ind(id_dict): 
    with open("../data/loc_state.json", "w") as f:
        json.dump(id_dict, f, indent=4)


# ind = np.array(id_dict["oh"])
# all_text = np.array([j['text'] for j in uber['statuses']])
# all_text1 = np.array([clean_text(st) for st in all_text])
# content_state = sum(all_text1[ind],[])
# clean_content = [clean_punc(w.lower()) for w in content_state if w.lower() not in stopwords]
# clean_content2 = [i for i in clean_content if i not in no_sense_list]
# fdist = FreqDist(clean_content2)
# fdist.most_common(10)

def keywords_state(state_name, id_dict, text_list,stopwords):
    if id_dict[state_name]==None :
        return None
    ind = np.array(id_dict[state_name])
    content_state = sum(text_list[ind],[])
    clean_content = [clean_punc1(w.lower()) for w in content_state if w.lower() not in stopwords]
    clean_content2 = [i for i in clean_content if i not in no_sense_list]
    return FreqDist(clean_content2).most_common(1)

# list of america and similar
list_us = ["american", "us", "usa", "united states", "america", "unite states"]

#dictionary of states and it abbrev
list_states = {
        'AK': 'Alaska',
        'AL': 'Alabama',
        'AR': 'Arkansas',
        'AS': 'American Samoa',
        'AZ': 'Arizona',
        'CA': 'California',
        'CO': 'Colorado',
        'CT': 'Connecticut',
        'DC': 'District of Columbia',
        'DE': 'Delaware',
        'FL': 'Florida',
        'GA': 'Georgia',
        'GU': 'Guam',
        'HI': 'Hawaii',
        'IA': 'Iowa',
        'ID': 'Idaho',
        'IL': 'Illinois',
        'IN': 'Indiana',
        'KS': 'Kansas',
        'KY': 'Kentucky',
        'LA': 'Louisiana',
        'MA': 'Massachusetts',
        'MD': 'Maryland',
        'ME': 'Maine',
        'MI': 'Michigan',
        'MN': 'Minnesota',
        'MO': 'Missouri',
        'MP': 'Northern Mariana Islands',
        'MS': 'Mississippi',
        'MT': 'Montana',
        'NA': 'National',
        'NC': 'North Carolina',
        'ND': 'North Dakota',
        'NE': 'Nebraska',
        'NH': 'New Hampshire',
        'NJ': 'New Jersey',
        'NM': 'New Mexico',
        'NV': 'Nevada',
        'NY': 'New York',
        'OH': 'Ohio',
        'OK': 'Oklahoma',
        'OR': 'Oregon',
        'PA': 'Pennsylvania',
        'PR': 'Puerto Rico',
        'RI': 'Rhode Island',
        'SC': 'South Carolina',
        'SD': 'South Dakota',
        'TN': 'Tennessee',
        'TX': 'Texas',
        'UT': 'Utah',
        'VA': 'Virginia',
        'VI': 'Virgin Islands',
        'VT': 'Vermont',
        'WA': 'Washington',
        'WI': 'Wisconsin',
        'WV': 'West Virginia',
        'WY': 'Wyoming'
}

#top 30 cities in the US and its state (based on the population)
cities = {
        "New York": "NY",
        "Los Angeles": "CA",
        "Chicago": "IL", 
        "Houston": "TX",
        "Philadelphia": "PA",
        "Phoenix": "AZ",  
        "San Antonio": "TX", 
        "San Diego": "CA", 
        "Dallas":  "TX",  
        "San Jose": "CA",  
        "Indianapolis": "IN", 
        "Jacksonville": "FL",  
        "San Francisco": "CA",  
        "Austin": "TX",  
        "Columbus": "OH", 
        "Fort Worth": "TX",  
        "Louisville Jefferson": "KY", 
        "Charlotte": "NC", 
        "Detroit": "MI", 
        "El Paso": "TX", 
        "Memphis": "TN",
        "Nashville-Davidson": "TN",
        "Baltimore": "MD",
        "Boston":  "MA",
        "Seattle": "WA",
        "Washington":  "DC",
        "Denver": "CO",
        "Milwaukee": "WI",
        "Portland": "OR",
        "Las Vegas": "NV"
}

#get data from uber tweets file
with open ("/Users/Xinyue_star/TwitterProject/data/uber_all.json") as infile:
   uber = json.load(infile)

loc_list = [status['user']['location'] for status in uber['statuses']]
print(len(loc_list))

all_text = [j['text'] for j in uber['statuses']]
all_text = np.array([clean_text(st) for st in all_text])
stopwords = nltk.corpus.stopwords.words('english')
no_sense_list = set(['nyc','ubered','memphistn'
    ,'httpstcoqars4abcwt','lasvegas','johnh343'
    ,'travel','code','ride','uber','-','rt'
    ,'chicago','','amp','drivers','driver'
    ,'get','way','via','new','use','next','free'
    ,'vegas','honoluluhi','dont','out','httpstcopauixm5chy','item'])


#load data: It is the location list of all the tweets
# with open("../data/uber_loc.json") as infile:
#     loc_list = json.load(infile)

# write the json file
# with open ("/Users/Xinyue_star/TwitterProject/data/justsee1.json", "w") as f:
#     json.dump(loc_list, f , indent=4)




# BUILD A STATE:INDEX DICTIONARY
# all states name sets, transfer into lower case
states_bv = [i.lower() for i in list_states.keys()]
states_l = [i.lower() for i in list_states.values()]
states = list(zip(states_bv,states_l))
states_set = [set(st) for st in states]
# return something like-------------
# [{'va', 'virginia'},{'mp', 'northern mariana islands'},...]

id_dict = dict.fromkeys(states_bv)
id_dict["tbd"] =None
list_tmp = US_loc_lower(loc_list)

for i in range(len(list_tmp)):
    loc = loc_ind(list_tmp[i],states_set)
    if id_dict[loc]==None:
        id_dict[loc]=[i]
    else:
        id_dict[loc].append(i)

# write_state_ind(id_dict)
# with open ("/Users/Xinyue_star/TwitterProject/data/dictsee1.json", "w") as f:
#     json.dump(id_dict, f , indent=4)

# find all the data in the usa and create a data frame
junk_data = np.array(id_dict["tbd"])
junk_loc = np.array(list_tmp)[junk_data]
a = np.array([bool_loc(a, set(list_us)) for a in junk_loc])
df3 = pd.DataFrame(index = junk_data[a])

a = id_dict.pop("tbd")
inds = list(id_dict.values())
states_nm = list(id_dict.keys())

ind_state = []
state_name = []
for i in range(len(inds)):
    if inds[i] is not None:
        tmp = [states_nm[i]]*len(inds[i])
        ind_state.extend(inds[i])
        state_name.extend(tmp)
df1 = pd.DataFrame({'STATE': state_name}, index = ind_state)
df1_sorted = df1.sort_index()


#find out top 30 cities and create a dataframe
citynames1 = [ct.lower() for ct in cities.keys()]
city_state1 = [st.lower() for st in cities.values()]
citynames = [[i] for i in citynames1]
citynames[citynames.index(['new york'])].extend(['nyc'])
citynames[citynames.index(['los angeles'])].extend(['la'])
citynames[citynames.index(["san francisco"])].extend(["sf"])
citynames[citynames.index(["washington"])].extend(["dc"])
citynames[citynames.index(["las vegas"])].extend(["vegas"])
city_set = [set(ct) for ct in citynames]

 
ind_city = []
for i in range(len(list_tmp)):
    loc = loc_ind2(list_tmp[i], city_set)
    if loc is not None:
        ind_city.extend([(i,loc)])

inmd = list(zip(*ind_city))
ind_city = list(inmd[0])
city_name = list(inmd[1])

df2 = pd.DataFrame({'CITY': city_name}, index = ind_city)

#merge df1 and df2
df = pd.concat([df1_sorted,df2],axis=1)

# fix missing state
# create a new dictionary with lowercase everything. Therefore we can find out the missing values
dict_new_city = dict(zip(citynames1,city_state1))
df["STATE"][df["STATE"].isnull()]= [dict_new_city[i] for i in df[df["STATE"].isnull()]["CITY"]]

#add one more column for the most frequent word for each state
keyword_loc = [keywords_state(state, id_dict, all_text,stopwords) for state in list(id_dict.keys())]
df3 = pd.DataFrame({'STATE':list(id_dict.keys()), 'KEYWORD':[i[0][0] if i != None else None for i in keyword_loc],
                   'FREQ':[i[0][1] if i != None else None for i in keyword_loc]}, index=None)
#merge df3
df_with_usa = pd.concat([df,df3],axis=1)
df_with_usa['COUNTRY'] = pd.Series(['USA']*df_with_usa.shape[0], index=df_with_usa.index)

df4 = pd.DataFrame({'CITY':citynames1,'STATE':city_state1})
# write out to .csv
df.to_csv("/Users/Xinyue_star/TwitterProject/data/location.csv")
df_with_usa.to_csv("/Users/Xinyue_star/TwitterProject/data/location_usa.csv")
df3.to_csv("/Users/Xinyue_star/TwitterProject/data/keywordsInfo.csv")
df4.to_csv("/Users/Xinyue_star/TwitterProject/data/city_state.csv")
