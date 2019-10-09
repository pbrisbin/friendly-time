**ARCHIVED**: this project is unmaintained and has been archived. See [#13](https://github.com/pbrisbin/friendly-time/issues/13) for a paper trail of the decision to do so, as well as some attempts at finding a new maintainer, which fizzled. As an alternative, I recommend [`Formatting.Time.diff`](https://hackage.haskell.org/package/formatting-6.3.7/docs/Formatting-Time.html#g:5).

# Friendly time

Friendly time formats (so far, just one).

### Human

Fuzzy time in relation to now.

```hs
humanReadableTime :: UTCDate -> IO String
-- => "just now", "1 minute ago", "wednesday", etc
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
