---
name: blahpack-status
description: Show the current translation status
argument-hint: <routine>
---

Show the current translation status.

1. Regenerate the implementation queue (authoritative source of remaining work):
```
node bin/gen-queue.js > QUEUE.md
```

2. Show the summary line from the top of QUEUE.md (implemented vs remaining counts).

3. List all implemented modules:
```
find lib -name base.js -path "*/lib/base.js" | sed 's|lib/||;s|/lib/base.js||' | sort
```

4. Show the top items from each tier in QUEUE.md (the next things to implement).

5. If a routine was just completed, check its box in QUEUE.md (`- [ ]` → `- [x]`).
