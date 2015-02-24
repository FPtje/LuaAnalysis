# Updating gluaparser

```
# Add the remote once:
git remote add parser git@github.com:FPtje/GLuaParser.git

# Use this command to update:
git fetch parser &&
rm analysis/src/GLua -rf &&
git read-tree --prefix=analysis/src -u parser/master:src
```
