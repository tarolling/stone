// even.st and odd.st import each other, which is fine since modules only define functions
use parity.even
use parity.odd.odd

for n in range(5);
    print(n, even.even(n), odd(n))
