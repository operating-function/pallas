set -e

try () {
    echo EXAMPLE: $1
    echo $1 | ./plan
    echo
}

try '(3 0)' 
try '(3 99)' 
try '(<isNat.seed> 0)'
try '(<isNat.seed> 1)'
try '(<cmp.seed> 0 1)'
try '(<cmp.seed> 1 0)'
try '(<cmp.seed> 1 1)'
try '(<cmp.seed> (0 0) 1)'
try '(<cmp.seed> (0 0) (1 1))'
try '(<mul.seed> 3 4)'
