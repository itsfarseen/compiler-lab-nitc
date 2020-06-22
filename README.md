# Compiler Lab

This project uses `stack` build tool.
Kindly install it from here: https://docs.haskellstack.org/en/stable/README/#how-to-install

After that, run `stack run -- [args]` to run the compiler.
`/test.sh` will run the test cases that are included in `tests/` folder.

End to end tests can be easily added to `tests/Test/golden/` folder.
They will be picked up by the test suite automatically.
