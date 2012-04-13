# Friendly time

Friendly time formats (so far, just one).

### Human

Fuzzy time in relation to now.

~~~ { .haskell }
humanReadableTime :: UTCDate -> IO String
-- ==> "just now", "1 minute ago", "wednesday", etc
~~~
