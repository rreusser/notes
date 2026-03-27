---
name: blahpack-coverage
description: Run test coverage analysis
argument-hint: <routine>
---

Run test coverage analysis. Run:

```
node --test --experimental-test-coverage 'lib/**/test/test*.js' 'lib/*.test.js'
```

Parse the output and present a summary table of base.js coverage per module. Flag any module with line coverage below 90% as needing attention. If the user asks about a specific module, show its uncovered lines and suggest what test cases would cover them.
