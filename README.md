# adobeanalyticsr
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

<img src="man/figures/logo.png" align="right" width = "200"/>

## R Client for Adobe Analytics API 2.0

Connect to the Adobe Analytics API v2.0, which powers Analysis Workspace. The package was developed with the analyst in mind and will continue to be developed with the guiding principles of iterative, repeatable, timely analysis. New features are actively being developed and we value your feedback and contribution to the process. Please submit bugs, questions, and enhancement requests as [issues in this Github repository](https://github.com/benrwoodard/adobeanalyticsr/issues).
  
### Install the package

```
# Install devtools from CRAN
install.packages("devtools")

# Install adobeanayticsr from github
devtools::install_github('benrwoodard/adobeanalyticsr') 

# Load the pacakge
library(adobeanalyticsr) 
```

### Current setup process overview

There are four setup steps required to start accessing your Adobe Analytics data. The following steps are each outlined in greater detail in the following sections:

  1. Create an Adobe Console API Project
  2. Create and add the OAuth arguments to your `.Renviron` file.
  3. Get your authorization token by using the function `aw_token()`.
  4. Get your `company_id` by using the function `get_me()`.

#### 1. Create an Adobe Console API Project

Regardless of how many different Adobe Analytics accounts you will be accessing, you only need a single Adobe Console API project (you will still need to have working user credentials for each account you want to access, but the Adobe Console API project is just the way you then get access to authenticate using those user credentials; yes...confusing!):

  1. Navigate to the following URL: https://console.adobe.io/integrations.
  2. Click the **Create New Project** button.
  3. Click the **Add API** button.
  4. Select the Experience Cloud product icon and then choose **Adobe Analytics** and click **Next**.
  5. Select the  OAuth option and then click **Next**.
  6. Select **Web** as the platform where you want the integration.
  7. Add Default redirect URI <code>https://adobeanalyticsr.com/token_result.html</code>*
  8. Add Redirect URI pattern <code>https://adobeanalyticsr\.com/token_result\.html</code>*
  
\* This is simply a helper site I've set up in order to make it easier to generate a token. The site does not store any information.

_If you are knowledgeable of the ins and outs of this auth process and feel you have a better way to explain and/or set this up, please [submit an issue](https://github.com/benrwoodard/adobeanalyticsr/issues) or create a pull request with your recommendation!_

**Creating an Adobe Console API Project in under 60 seconds**
<img src="man/figures/createoauthproject.gif" align="center" />

#### 2. Set up the .Renviron file

This file is essential to keeping your information secure. It also speeds up analysis by limiting the number of arguments you need to add to every function call.

  1. If you do not have an `.Renviron` file (if you have never heard of this file you almost certainly don't have one!), then create a new file and save it with the name `.Renviron`. You can do this from within the RStudio environment and save the file either in your `Home` directory (which is recommended; click on the **Home** button in the file navigator in RStudio and save it to that location) _or_ within your project's working directory.
  
  2. Get the following variables from the OAuth project and add them to the file* (see _Creating an Adobe Console API Project_ above):

      * `AW_CLIENT_ID` - OAuth client id found in the Adobe Developer Console
      * `AW_CLIENT_SECRET` - OAuth client secret key found in the Adobe Developer Console

  3. (Optional) Add `AW_COMPANY_ID` and `AW_REPORTSUITE_ID` variables once you know them (how to find available values for these two variables is described in step 4 below).
      
After adding these 2-4 variables to the `.Renviron` file and saving it, restart your R session (**Session > Restart R** in RStudio) and reload `adobeanalyticsr` (`library(adobeanalyticsr)`). 

\* The format of variables in the `.Renviron` file is straightforward. If you add all four of the variables above, they would simply each be their own line in the file: 

```
AW_CLIENT_ID = "[OAuth client ID]"
AW_CLIENT_SECRET = "[OAuth client secret]"
AW_COMPANY_ID = "[Company ID]"
AW_REPORTSUITE_ID = "[RSID]"
```

#### 3. Get your authorization token

The token is actually a lonnnnng alphanumeric string that is the what ultimately enables you to access your data:

1. In the console, enter `aw_token()` and press _<Enter>_.
2. A browser window should appear that prompts you to log in to your Adobe Marketing Cloud account.
3. Log in.
4. You will then be redirected to `adobeanalyticsr.com`* and a screen that displays your token.*
5. Copy the token (only).
6. Return to the R console. You should see a prompt to **Enter authorization code:**
7. Paste the token you copied and press _<Enter>_.

This token will expire every 24 hours, so you will have to periodically repeat this step.

\* Again, this is simply a helper site I've set up in order to make it easier to generate a token. The site does not store any information.

#### 4. Confirm the process worked and get available company_id and reports

The last step is to get your `company_id` (or, if you have access to multiple accounts, get the `company_id` you want to work with). This is also an excellent way to confirm that everything is set up correctly:

1. In the console, type `get_me()` and press _<Enter>_.
2. You should then see a list of all of the companies you have access to. The **globalCompanyId** value is what you will use to specify the company that you want to access. If you are working solely (or primarily) with one company, then consider adding the `company_id` as `AW_COMPANY_ID` in your `.Renviron` file as described in step 2 above. Otherwise, you can specify it within each function call that requires it.
3. If you will be working solely (or primarily) with a single report suite, then consider adding the report suite ID (RSID) as `AW_REPORTSUITE_ID` in your file as described in step 2 above. You can retrieve all of the report suite IDs for a given `company_id` using the function `aw_get_reportsuites(company_id = '[the company ID for account of interest]')`.

If you added any values to your `.Renviron` file (and then saved it!), then restart your R session (**Session > Restart R** in RStudio) and reload `adobeanalyticsr` (`library(adobeanalyticsr)`). 

#### 5. Pull some data!

You now should be set to start pulling data, typically starting with `aw_get_metrics()` and `aw_get_dimensions()` to get a list of available dimension and metric IDs, and then `aw_freeform_table()` to pull some data!
