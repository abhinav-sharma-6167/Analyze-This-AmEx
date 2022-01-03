# American-Express-Analyze-This- 2016
This was a PAN-IIT data science competition organized by American Express in 2016 wherein my submission ranked 5th on the leaderboard. The competition was attended by more than 900 students, across all IITs and BITs campuses in India. Following is the problem description, as provided and summary of the approaches undertaken.

### Problem Background :

The Island of Hoenn is gearing up for upcoming polls. Citizens are waiting with bated breath as news agencies reveal their predictions on which party is likely to emerge victorious. Much to the disappointment of all the citizens, there are discrepancies in these poll predictions amongst news channels. So many inconsistent predictions did not go down well with an inquisitive bunch of students. Wondering how difficult it might be to crack it, comes their 'Eureka' moment. An idea to create their own ‘Start Up’ to analyze poll sentiments and predict the winner. A start up called - Analyze This. They gather data for a sample of citizens of Island of Hoenn and get started. Information on historical voting pattern, rally attendance and demographics is what they have at hand to predict the winner amongst the 5 competing parties.

Can you help these students crack this puzzle? Do you have it in you to start your own Analyze This?

### Problem Statement :

You have to predict the party for which each citizen will vote for in the upcoming polls.

### Data for Analysis :

Following files can be downloaded for your analysis.

Training_Dataset.csv: This data has all the information for a sample of citizens. The information includes: a. Past history of voting for all citizens b. Who they will vote for in upcoming polls c. Donation, Rally Attendance d. Demographics of these citizens

Leaderboard_Dataset.csv: This data has information on the historical vote for a different set of citizens, along with donation, rally attendance and demographics. The vote in the upcoming polls is not present in the data.

Final_Dataset.csv: This data has information on the historical vote for a different set of citizens, along with donation, rally attendance and demographics. The vote in the upcoming polls is not present in the data.

Please note that you can make multiple submissions corresponding to the Leaderboard Dataset. However, for the Final dataset you can submit only one solution

Data_Dictionary.xlsx: This sheet will give you the description of all the variables contained in the 3 datasets above.


### Analysis and Modelling :
- Outlier treatment
- Missing Values Imputation with categorical averages
- Feature engineering based on interaction effects amidst variables
- Benchmark solutions using XGBoost
- Multiple experiements with varies features and models such as SVMs
- Ensembled final solution from outputs of best XGB, several one-class SVMs and few MLPs.


### Evaluation Criteria :

Leader board Submission The dataset for Leader board evaluation would be evaluated on the basis of the estimation that you provide. The score is calculated as: a. If Actual Vote = Predicted Vote and Actual Vote = Historical Vote, then score = 50 b. If Actual Vote = Predicted Vote and Actual Vote ^= Historical Vote, then score = 100 c. If Actual Vote ^= Predicted Vote and Actual Vote = Historical Vote, then score = 0 d. If Actual Vote ^= Predicted Vote and Actual Vote ^= Historical Vote, then score = -50

Your final score will depend on the following parameters: 1. 20% weight for highest score achieved on LeaderBoard submission 2. 60% weight for score achieved on Final Dataset 3. 20% weight for the technique(s) chosen and the associated reasons for the same.
