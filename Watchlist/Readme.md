# Instructions :

Watchlist service. This service will allowa user to create a personalised list of on-demand assets (e.g. tv show episodes and movies) that they can watch later. Customers will add items to their watchlist by selecting assets in any NowTVclient device (mobile, web, smartTV etc.)

The Watchlist services hould be implemented as an API. A single watchlist item is represented by a 5 digit alphanumeric string (called  a contentID) that  is  unique  to  a  specific  asset. The client teams  will  send contentIDs to the new Watchlist service. 

# Acceptance Criteria

- Customer can add contentIDs to their Watchlist
- Customer can delete contentIDs from their Watchlist
- Customer can see contents they added in their Watchlist
- Customer cannot see another customer’s Watchlist
- Each customer is represented by a unique 3 digit alphanumeric string
- The Watchlist items should be stored in memory
- The API should produce and consume JSON

# Examples:
Given a customer with id `123` and an empty Watchlist
When the customer adds ContentIDs `zRE49`, `wYqiZ`, `15nW5`, `srT5k`, `FBSxr` to their watchlist
Then their Watchlist is returned it should only includeContentIDs `zRE49`, `wYqiZ`, `15nW5`, `srT5k`, `FBSxr`

Given a customer with id `123` and a Watchlist containing ContentIDs `zRE49`, `wYqiZ`, `15nW5`, `srT5k`, `FBSxr`
When they remove ContentID `15nW5` from their Watchlist
Then their Watchlist should only contain ContentIDs `zRE49`, `wYqiZ`, `srT5k`, `FBSxr`

Given two customers, one with id `123` and one with id `abc`
And corresponding Watchlists containing ContentIDs `zRE49`, `wYqiZ`, `srT5k`, `FBSxr` and `hWjNK`, `U8jVg`, `GH4pD`, `rGIha` respectively
When customer with id `abc` views their Watchlist they should only see ContentIDs`hWjNK`, `U8jVg`, `GH4pD`, `rGIha`