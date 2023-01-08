# subulisp-rs
[![Build][build-shiled]][build-url]
[![Contributors][contributors-shield]][contributors-url]
[![Issues][issues-shield]][issues-url]
[![Codecov][codecov-shield]][codecov-url]
[![MIT][license-shield]][license-url]

my plain lisp implementation.

## Features

### REPL
```
$ cargo run
> (+ 1 2)
3
> (* (+ 1 2) (- 3 5))
-6
```

### Batch
```
$ cat ./samples/fibo.lisp 
(
(def fibo (n) (if (<= (n) 1.0) 1.0 (+ (fibo (- (n) 1.0)) (fibo (- (n) 2.0)))))
(fibo 10.0)
)
$ cat ./samples/fibo.lisp  | cargo run
(() 89)
```

## License
[MIT](https://github.com/ar90n/subulisp-rs/blob/main/LICENSE)

[build-shiled]: https://img.shields.io/github/actions/workflow/status/ar90n/subulisp-rs/ci-testing.yml
[build-url]: https://github.com/ar90n/subulisp-rs/actions/workflows/ci-testing.yml
[contributors-shield]: https://img.shields.io/github/contributors/ar90n/subulisp-rs.svg?style=flat
[contributors-url]: https://github.com/ar90n/subulisp-rs/graphs/contributors
[issues-shield]: https://img.shields.io/github/issues/ar90n/subulisp-rs.svg?style=flat
[issues-url]: https://github.com/ar90n/subulisp-rs/issues
[license-shield]: https://img.shields.io/github/license/ar90n/subulisp-rs.svg?style=flat
[license-url]: https://github.com/ar90n/subulisp-rs/blob/main/LICENSE
[codecov-shield]: https://codecov.io/gh/ar90n/subulisp-rs/branch/main/graph/badge.svg?token=8GKU96ODLY
[codecov-url]: https://codecov.io/gh/ar90n/subulisp-rs
