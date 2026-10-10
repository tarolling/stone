// a program that builds lists but never prints still compiles
xs = [1, 2]
xs.append(3)
words = ["a"]
words.append(str(xs[2]))
[]
