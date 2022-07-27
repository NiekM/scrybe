# synthesis

## Execute

* Run `stack exec -- synthesis-exe` to run the application.
* With `stack exec -- synthesis-exe --verbose` you will see the same result,
  with more logging.

## Run tests

### Run all tests

* `stack test`
* `stack test --test-arguments='--match=benchmark'`

## Benchmarks

NOTE: we have to run `chcp 65001` beforehand or use `--verbosity=0`, because
powershell cannot display some characters.

* `stack bench --benchmark-arguments='myth'`
* `stack bench --benchmark-arguments='lambda2'`

* `stack bench --ba='--verbosity=0 --output=results/bench.html'`

* `stack bench --ba='--match=pattern --verbosity=0 --output=results/bench_myth.html myth'`

## Run profiling

* `stack --profile build --ghc-options="-fprof-auto-top"`
* `stack --profile exec -- synthesis-exe +RTS -p`
