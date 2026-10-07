// a program split into modules: geometry.st sits next to the geometry/ directory, and text/
// holds modules without a file of its own
use geometry
use geometry.shapes
use geometry.vec.dot
use text.words.count as count_words
use text.pad

print(geometry.origin())
print(shapes.area(2), shapes.perimeter(3))
print(dot([1, 2, 3], [4, 5, 6]))

while not eof();
    line = input()
    print(pad.left(str(count_words(line)), 3) + " | " + line.strip())

// a local may shadow an imported name
def describe(pad);
    ret "pad is " + str(pad)

print(describe(7))
