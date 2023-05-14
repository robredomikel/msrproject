README file for the replication of this project:

CONTENTS:
	
	1. DATA-MINING PROCEDURES

		1.1. Mining projects files with initial confounders from GitHub API 
		1.2. Mining registered ASF projects in SonarCloud from its API
		1.3. Mining issues from ASF repositories in Jira and GitHub
		1.4. Mining commits from ASF projects in GitHub.
 
	2. PREPROCESSING

		2.1. Merge of common issues in Jira and GitHub per project.
		2.2. Velocity calculation from issue data.
		2.2. Pruning for the final dataset.
		2.3. Variable unit change for some of the columns.

	3. DATA ANALYSIS (With specifications on what data changes to perform)

		3.1. Regression Analysis performed in R.

----------------------------------------------------------------------------------------------------------------------

1. DATA-MINING PROCEDURES (All the content is described in the code)

	* NOTE: Make the structure of the folders in the same way displayed in figshare so that the code works, or else manage on your own the locations through the code.
		Subsequent csv files made out from the crawlers will be stored in the mentioned folders until the merge stage.

	1.1. Mining projects files with initial confounders from GitHub API
	
		- Use notebook apacheGitHub.ipynb
		- Remember to get create a token in (https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).


	1.2. Mining registered ASF projects in SonarCloud from its API
	
		- Execute sonarQubeCrawler.ipynb 
		- Remember to use create a token in (https://docs.sonarcloud.io/advanced-setup/web-api/).

	1.3. Mining issues from ASF repositories in Jira and GitHub

		- Use notebook issueCrawlerGithub.ipynb for issues tracked in GitHub and jiraCrawler.ipynb for issus tracked in Jira. 
		  (No need to use token with Atlassian for Jira issues)

	1.4. Mining commits from ASF projects in GitHub.

		- Use commitCrawler.ipynb to crawl over the considered repositories and mine their commits. In addition it will handle the name difference for projects using SQ
		  since their names in SonarCloud differ from how they are stated in GitHub.

2. PREPROCESSING (Considering that all the folders)

	2.1. Merge of common issues in Jira and GitHub per project.

		- Execute collectionMerge.ipynb until "VELOCITY CALCULATION DATA ADDITION STAGE".

	2.2. Velocity calculation from issue data.

		- Execute "calculate_velocity.py" Python file to calculate the development velocity. It can be either done in the command line, or 
		  adding its' function call in the notebook.
	
	2.2. Pruning for the final dataset.
	
		- Continue executing the rest of functions in the collectionMerge.ipynb file mentioned before until the end.

	2.3. Variable unit change for some of the columns. 
		
		- Through the python file format_data_fot_analysis.py the format of the projectFinalFile.csv dataset is modified for the an easier analysis in R.
		  In this way, you can find SQ_repos_manualCheck_lastAnalysisDate.csv as the final output which is later used in the R file to be explained.

3. DATA ANALYSIS (With specifications on what data changes to perform)

	3.1. Regression Analysis performed in R.

		- With the mentioned changes performed in the final dataset run the analysis with the multiRegression.R file. It's highly recommended
		  to create a new R project in the working directory and then run the script.  
