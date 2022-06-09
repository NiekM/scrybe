# synthesis

## Execute

* Run `stack exec -- synthesis-exe` to run the application.
* With `stack exec -- synthesis-exe --verbose` you will see the same result,
  with more logging.

## Run tests

### Run all tests

* `stack test`

### Only benchmarks

* `stack test --test-arguments='--match benchmark'`

## Run profiling

* `stack --profile build --ghc-options="-fprof-auto-top"`
* `stack --profile exec -- synthesis-exe +RTS -p`
