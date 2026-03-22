Show the current translation status. Run:

1. List all implemented modules:
```
find lib -name base.js -path "*/lib/base.js" | sed 's|lib/||;s|/lib/base.js||' | sort
```

2. Run the full test suite:
```
npm test
```

3. Show a summary: how many modules exist, how many tests pass, and what the next unimplemented dependencies are for key targets (dgesv, dpotrf, dgeev).
