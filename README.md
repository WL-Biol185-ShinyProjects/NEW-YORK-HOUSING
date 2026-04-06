# NEW-YORK-HOUSING
A interactive Shiny app that dives into the housing market in NYC Metro Areas: Westchester County, Nassua County, 
and NYC. This app was designed and built using data from Redfin from the years 2015-2025. The app's
users are able to idenitfy overall market trends, regional trnds, Year-over-Year/Month-over-Month trends, 
visualize the data through a interactive heat map, use our predictive model to see where housing prices
are heading, and also choose user specific preferences to see which location is best for the 
individual. 


## Members

Ryan McGovern, 
Perry Nuckols 

### Tab Overview 

#### Market Overview:
  -Explore this tab to determine market trends for Median Price, Days on the Market
  inventory, and overall market trends
  
#### Region Overview:
  -Similar to the market tab, users can determine different market trends based on more 
  data specific to the region. 
  
#### Heat Map:
  -Users have access to an interactive heat map that allows users to visualize 
  the market data in map form. 
  
#### Affordability Calculator:
  -User are able to input their specific preferences in areas such as life stage, 
  environment, school district ratings, leisure preferences, and public transportation. 
  It also allows users to input annual household income, down payment, interest rates, 
  monthly insurance and tax, and loan term. It then takes all of that data and gives a 
  suggestion as well as ratings for the different locations. 
  
#### Price Prediction Model: 
  -Shows users a predicted housing price for the different regions based on a liner 
  predictive model. 
  
### Acknoledgements and Citations:
#### Data: 
Redfin(2025)- https://www.redfin.com/news/data-center/

#### AI Usage:
Claude (Anthropic)- https://claude.ai/new

#### Images: 
Home Page:"https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Southwest_corner_of_Central_Park%2C_looking_east%2C_NYC.jpg/1280px-Southwest_corner_of_Central_Park%2C_looking_east%2C_NYC.jpg

Affordability Calculator:
Nassau County

Under $600K: https://images.unsplash.com/photo-1570129477492-45c003edd2be?w=600&q=80
$600K+: https://images.unsplash.com/photo-1600585154340-be6161a56a0c?w=600&q=80

New York City

Under $700K: https://images.unsplash.com/photo-1555636222-cae831e670b3?w=600&q=80
$700K+: https://images.unsplash.com/photo-1486325212027-8081e485255e?w=600&q=80

Westchester

Under $700K: https://images.unsplash.com/photo-1568605114967-8130f3a36994?w=600&q=80
$700K+: https://images.unsplash.com/photo-1600596542815-ffad4c1539a9?w=600&q=80


There are also 4 fallback images used when no region is specified (based on price tier only):

Under $400K: photo-1568605114967-8130f3a36994
$400K–$700K: photo-1570129477492-45c003edd2be
$700K–$1M: photo-1600596542815-ffad4c1539a9
$1M+: photo-1613977257363-707ba9348227




