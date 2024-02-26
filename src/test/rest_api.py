# pip install -U sec-edgar-api

from sec_edgar_api import EdgarClient

# Specify user-agent string to pass to SEC to identify
# requests for rate-limiting purposes
edgar = EdgarClient(user_agent="<ITESM> <AA01232580@tec.mx>")
edgar

# Get submissions for Apple with the additional paginated files
# appended to the recent filings to prevent the need for extra
# manual pagination handling
edgar.get_submissions(cik="320193")

# Get submissions for Apple without automatic pagination handling,
# which requires manual handling of the paginated files (not recommended)
edgar.get_submissions(cik="320193", handle_pagination=False)

# Get company concept for Apple
apple = edgar.get_company_concept(cik="320193", taxonomy="us-gaap", tag="AccountsPayableCurrent")

# Get company facts for Apple
edgar.get_company_facts(cik="320193")

# Get one fact for each reporting entity in specified
# calendar period (Q1 2019)
edgar.get_frames(taxonomy="us-gaap", tag="AccountsPayableCurrent", unit="USD", year="2019", quarter=1)

# * ---

# Get company concept for Apple
apple = edgar.get_company_concept(cik="320193", taxonomy="us-gaap", tag="AccountsPayableCurrent")

import pandas as pd

# Assuming 'apple' is a class 'dict'
apple_dict = dict(apple)

appledf = pd.DataFrame.from_dict(apple_dict)

# Continue with the rest of your code
appledf.to_excel("./data/apple.xlsx", index=False)
appledf
print(apple.__class__)

