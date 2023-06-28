################################
import snscrape.modules.twitter as sntwitter
import pandas as pd

# What search terms are you interested in
search_this = ['Cassava nematodes', 'Yam nematodes', 'Nematode management']

# Data frame Initialization
df = pd.DataFrame()

for term in search_this:
    tweets = [] # This list holds the Tweet data - initialize it, first.
    # Im interested in scraping 100 tweets related to each search term
    for i, tweet in enumerate(sntwitter.TwitterSearchScraper(f'{term} lang:en').get_items()):
        if i+1 > 100: 
            break
        tweets.append(tweet)
    term_df = pd.DataFrame(tweets) # Important step - Create a data frame from the list of tweet data
    term_df['term'] = term # Now you can add your search term to the data frame
    df = df.append(term_df) 

df = df.reset_index(drop=True) # Reset tindex

df.head(10)
df.to_csv('C:/NSPP_Workshop/All_Nema_twts.csv') # Save file to s designated folder







######################
# What search terms are you interested in
search_this = ['Cassava nematodes', 'Yam nematodes', 'Nematode management']

# Data frame Initialization
df = pd.DataFrame()

for term in search_this:
    
    tweets = [] # This list holds the Tweet data - initialize it, first.
    # Im interested in scraping 100 tweets related to each search term
    for i, tweet in enumerate(sntwitter.TwitterSearchScraper(f'{term} lang:en').get_items()):
        if i+1 > 100: 
            break
          
        tweets.append({
            'date': tweet.date,
            'content': tweet.rawContent,
            'username': tweet.user.username,
            'term': term
        })
        
    term_df = pd.DataFrame(tweets) # Important step - Create a data frame from the list of tweet data
    term_df['term'] = term # Now you can add your search term to the data frame
    df = df.append(term_df) 

df = df.reset_index(drop=True) # Reset tindex

df.head(10)
df.to_csv('C:/NSPP_Workshop/All_ai_twts1.csv') # Save file to s designated folder



import snscrape.modules.twitter as sntwitter
import pandas as pd

# Define the search terms that you're interested in
search_terms = ['Cassava nematodes', 'Yam nematodes', 'Nematode management']

# Initialize a data frame
df = pd.DataFrame()

for term in search_terms:
    # Initialize a list to hold Tweet data
    tweets = []

    # Use snscrape to scrape 100 tweets related to each term
    for i, tweet in enumerate(sntwitter.TwitterSearchScraper(f'{term} lang:en').get_items()):
        if i+1 > 100: 
            break
        tweets.append({
            'date': tweet.date,
            'content': tweet.content,
            'username': tweet.user.username,
            'term': term
        })

    # Create a data frame from the list of tweet data
    term_df = pd.DataFrame(tweets)

    # Append this term's data frame to the main data frame
    df = df.append(term_df)

# Reset the main data frame's index
df = df.reset_index(drop=True)

df.head(10)
df.to_csv('C:/NSPP_Workshop/All_ai_twts1.csv') # Save file to s designated folder



