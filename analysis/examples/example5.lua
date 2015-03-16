a = true
weirdTable = {
  [1] = {
    [a] = {
        ["v: " .. _VERSION] = function()
            if false then x = 5 end
            return 3
        end
    }
  }
}
b = weirdTable[1][a]["v: " .. _VERSION]()
return b
