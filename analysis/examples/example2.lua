x = 1
b = function(f)
    if f then
        a = f
    else
        a = f
    end

    return a
end

x = b(true)
y = x

if not x then
    print(y)
end

g = true

while not g do
    print(y)
end
