# -*- coding: utf-8 -*-

# import modules
import requests
import pandas as pd

# create request header
headers = {"User-Agent": "victorbenitogr@gmail.com"}

# get all companies data
companyTickers = requests.get(
    "https://www.sec.gov/files/company_tickers.json",
    headers = headers
    )

# review response / keys
print(companyTickers.json().keys())

# format response to dictionary and get first key/value
firstEntry = companyTickers.json()['0']
firstEntry

# parse CIK // without leading zeros
directCik = companyTickers.json()['0']['cik_str']
directCik

# dictionary to dataframe
companyData = pd.DataFrame.from_dict(companyTickers.json(),
                                     orient='index')
companyData

# add leading zeros to CIK
companyData['cik_str'] = companyData['cik_str'].astype(
                           str).str.zfill(10)

# review data
print(companyData[:1])

cik = companyData[0:1].cik_str[0]

# get company specific filing metadata
filingMetadata = requests.get(
    f'https://data.sec.gov/submissions/CIK{cik}.json',
    headers=headers
    )

# review json 
print(filingMetadata.json().keys())
filingMetadata.json()['filings']
filingMetadata.json()['filings'].keys()
filingMetadata.json()['filings']['recent']
filingMetadata.json()['filings']['recent'].keys()

# dictionary to dataframe
allForms = pd.DataFrame.from_dict(
             filingMetadata.json()['filings']['recent']
             )
allForms.head(10)
# review columns
allForms.columns
allForms[['accessionNumber', 'reportDate', 'form']].head(50)

# 10-Q metadata
allForms.iloc[11]

# get company facts data
companyFacts = requests.get(
    f'https://data.sec.gov/api/xbrl/companyfacts/CIK{cik}.json',
    headers=headers
    )

#review data
companyFacts.json().keys()
companyFacts.json()['facts']
companyFacts.json()['facts'].keys()

# filing metadata
companyFacts.json()['facts']['dei'][
    'EntityCommonStockSharesOutstanding']
companyFacts.json()['facts']['dei'][
    'EntityCommonStockSharesOutstanding'].keys()
companyFacts.json()['facts']['dei'][
    'EntityCommonStockSharesOutstanding']['units']
companyFacts.json()['facts']['dei'][
    'EntityCommonStockSharesOutstanding']['units']['shares']
companyFacts.json()['facts']['dei'][
    'EntityCommonStockSharesOutstanding']['units']['shares'][0]

# concept data // financial statement line items
companyFacts.json()['facts']['us-gaap']
companyFacts.json()['facts']['us-gaap'].keys()

# different amounts of data available per concept
companyFacts.json()['facts']['us-gaap']['AccountsPayable'] # ! Deprecated
companyFacts.json()['facts']['us-gaap']['Revenues']
companyFacts.json()['facts']['us-gaap']['Assets']

# get company concept data
companyConcept = requests.get(
    (
    f'https://data.sec.gov/api/xbrl/companyconcept/CIK{cik}'
     f'/us-gaap/Assets.json'
    ),
    headers=headers
    )
cik
# review data
companyConcept.json().keys()
companyConcept.json()['units']
companyConcept.json()['units'].keys()
companyConcept.json()['units']['USD']
companyConcept.json()['units']['USD'][0]

# parse assets from single filing
companyConcept.json()['units']['USD'][0]['val']

# get all filings data 
assetsData = pd.DataFrame.from_dict((
               companyConcept.json()['units']['USD']))

# review data
assetsData.columns
assetsData.form
assetsData

# get assets from 10Q forms and reset index
assets10Q = assetsData[assetsData.form == '10-Q']
assets10Q = assets10Q.reset_index(drop=True)

assets10Q

# Export assets10Q as an xlsx file
assets10Q.to_excel("./data/assets10Q.xlsx", index=False)

# plot 
assets10Q.plot(x='end', y='val')