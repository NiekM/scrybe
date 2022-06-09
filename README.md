# synthesis

## Execute

* Run `stack exec -- synthesis-exe` to run the application.
* With `stack exec -- synthesis-exe --verbose` you will see the same result,
  with more logging.

## Run tests

`stack test`

## Run profiling

* `stack --profile build --ghc-options="-fprof-auto-top"`
* `stack --profile exec -- synthesis-exe +RTS -p`
