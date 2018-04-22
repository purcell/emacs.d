Implementation of the OAuth 2.0 draft.

The main entry point is `oauth2-auth-and-store' which will return a token
structure.  This token structure can be then used with
`oauth2-url-retrieve-synchronously' or `oauth2-url-retrieve' to retrieve
any data that need OAuth authentication to be accessed.

If the token needs to be refreshed, the code handles it automatically and
store the new value of the access token.
