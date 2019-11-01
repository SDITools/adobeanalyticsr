# adobeAnalyticsR

This is intended as a POC on how a AAv2 package could work, including OAuth2.

This is a barebones skeleton for a package based on Tim Wilson's experiments with the Adobe Analytics v2.0 API here:
http://rpubs.com/tgwilson/adobe-analytics-v2-rstats-basic-example

## Current Setup Requirements

It requires setting the following environment arguments:

* `AA_COMPANY_ID`
* `AA_CLIENT_ID`
* `AA_CLIENT_SECRET`
* `ADOBE_API_RSID` (optional)

## Getting Auth to Work

This is not the long-term plan by any means. But, what seems to work to some extent:

1. Run `options(httr_oob_default=TRUE)`
2. Call the `aa_token()` function
3. If no `aa.oauth` file exists already, then you will be prompted to log in to Adobe
4. Once you log in, you will be redirected to a (busted) localhost page
5. Copy the `code=` value from the URI and enter that as the authorization code in the console.
6. You should now have an `aa.oauth` token in your working directory

The OAuth2 information is found here:
https://www.adobe.io/apis/experiencecloud/analytics/docs.html#!AdobeDocs/analytics-2.0-apis/master/create-oauth-client.md

Example of one person struggling with OAuth2 for this:  https://www.reddit.com/r/rstats/comments/c3n1t0/making_an_oauth_call_using_httr/

One option for creating an HTTPS localhost: https://dev.to/rhymes/really-easy-way-to-use-https-on-localhost-341m.

A wrinkle with OAuth2 and Adobe IO is that Adobe IO _requires_ a **Default direct URI** and **Redirect URI pattern** that use HTTPS. So, so far, it's challenging to get the OAuth token created without making an HTTPS localhost. Running `mkcert` might be one option around that: https://github.com/FiloSottile/mkcert.


