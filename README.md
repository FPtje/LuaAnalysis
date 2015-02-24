# Updating gluaparser

```
# Add the remote once:
git remote add parser git@github.com:FPtje/GLuaParser.git

# Use this command to update:

git fetch parser &&
git rm -rf analysis/src/GLua &&
git read-tree --prefix=analysis/src -u parser/master:src
```
