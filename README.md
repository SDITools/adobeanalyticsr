# adobeanalyticsr

AdobeAnalyticR package gets data using the Adobe Analytics API 2.0. The API is the same as what powers
Analysis Workspace and therefore is best used in conjunction with the Workspace UI. There are many advantages to api 2.0 over 1.4. 


## Current recommended setup

  1. Get the following variables from the OAuth project. (see Creating an OAuth Client below)

      * `AA_CLIENT_ID` - OAuth client id found in the Adobe Developer Console
      * `AA_CLIENT_SECRET` -  OAuth client secret key found in the Adobe Developer Console

  2. Authenticated using 'aa_token()'
  
      * `AA_COMPANY_ID` - Using the function 'get_me()' you can see all the companies you have access to.
      * `AA_REPORTSUITE_ID` - Using the function 'aa_get_reportsuites(company_id = '')' you can see all the report suite ids you can use within the chosen companuy.

After defining these 4 variables in the .Renviron file, restart your session.  After reloading
the adobeanalyticsr library, you should be able to run your first query using 'aa_get_metrics()'

### Creating an OAuth Client

1. It is essential to create an OAuth Client in the Adobe Developer Console. https://www.adobe.io/apis/experiencecloud/analytics/docs.html#!AdobeDocs/analytics-2.0-apis/master/create-oauth-client.md.  
  
  **To create an OAuth client:**

    1. Navigate to the following URL: https://console.adobe.io/integrations.
    2. Click the New Integration button.
    3. Select the Access an API option and then click Continue.
    4. Under the Experience Cloud section select Adobe Analytics and then select OAuth integration and then click Continue.
    5. Select New integration and then click Continue.
    6. Fill out the 
        a. name = whatever makes sense for your organization
        b. description = whatever descriptions you choose
        c. redirect URIs - 'https://adobeanalyticsr.com/token/result.html'
        d. click Create Integration.

There are three ways to add the four variables to to your environment.  
1. The first way is the easiest.  Make sure to have each one of them readily available and then call the `aa_creds()` function. It will ask you for each one and save it to the system environment.
2. The second way is more manual but you essentially do the same thing the function does but simply add it yoruself. Make sure to use the exact environment argument name that is listed above and save it to the system environment using "Sys.setenv('ARGUMENT_NAME' = 'VALUE')".  Do this for each of the for arguments and you will be all set.
3. When you call a function that requires the use of one of the variables, provide it in the function call. For example `aa_token(client_id, client_secret)`.


https://console.adobe.io/home