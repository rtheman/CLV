**RECIPE || Customer Lifetime Value (CLV)**

[[Work In Progress]] 

------------------------------------------------------------------------
------------------------------------------------------------------------


Calculating Customer Lifetime Value (CLV) with Recency, Frequency, and Monetary (RFM)
------------------------------------------------------------------------

CLV, "the present value of future cash flow attributed to a customer's tenure with a company", can be determined by variety of approaches.  Based on a simple [equation](http://www.r-bloggers.com/calculating-customer-lifetime-value-with-recency-frequency-and-monetary-rfm/), one can calculate the cumulative profit (value) from a customer based on assumptions such as retention rate and historical profit margin from customers.  However, a customer's retention can be influenced by factors such as demographics (age, geography, education background), behavior (Recency, Purchase Frequency, Monetary Contribution), competitions, peer influences, etc..  As a result, a dynamic approach should be taken to account for such variations.

In this repo, I calculated CLV by predicting the retention rate (r) of customers' future purchasing cycle using Logistic Regression based on his/her Recency of purchase, purchase Frequency, and Monetary contribution from past purchases.

------
### RFM
By definition, RFM represents:

 - R(ecency): how recently did customer purchase?
 - F(rquency): how often do customer purchase?
 - M(onetary Value): how much do they spend (each time on average)?

The determination of Recency, Frequency, and Monetary of a customer, should provide insights to the following:

 1. How to segment customers to determine who are more likely to response to ads / to purchase?
 2. Which type of customers to send ads in order to breakeven and make profit?

------
### Data
The CDNow data set (download at [here](http://brucehardie.com/datasets/)), consists of 69,659 transaction records by 23,570 unique customers, captures the purchase records between Jan 1997 and June 1998.

------
### Code

 - Source code ([R](/2_Code/RFM_v0.2.R))

------
### References
This project was largely based on the work done by Jack Han at [Data Apple](http://www.dataapple.net/?p=84).  However, new libraries/functions are used to update data transformation and visualization of data such as dplyr and ggviz.

Other references used:

 - [Customer Lifetime Value (CLV) – A Methodology for Quantifying and Managing Future Cash Flows, David C. Ogden](http://www.sas.com/content/dam/SAS/en_ca/User%20Group%20Presentations/Montreal-Business-Analytics-Forum/DavidOgden-CustomerLifetimeValue-Nov2009.pdf)



*created by [stackedit.io](https://stackedit.io/editor#fn:stackedit)*
