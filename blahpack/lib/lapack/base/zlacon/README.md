<!--

@license Apache-2.0

Copyright (c) 2025 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->

# zlacon

> Estimates the 1-norm of a square complex matrix A using reverse communication.

<section class="usage">

## Usage

```javascript
var zlacon = require( '@stdlib/lapack/base/zlacon' );
```

#### zlacon( N, V, strideV, X, strideX, EST, KASE )

Estimates the 1-norm of a square complex matrix A using reverse communication.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **V**: input array `V`.
-   **strideV**: stride length for `V`.
-   **X**: input array `X`.
-   **strideX**: stride length for `X`.
-   **EST**: input array `EST`.
-   **KASE**: input array `KASE`.

#### zlacon.ndarray( N, V, strideV, offsetV, X, strideX, offsetX, EST, KASE )

Estimates the 1-norm of a square complex matrix A using reverse communication, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetV**: starting index for `V`.
-   **offsetX**: starting index for `X`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlacon()` corresponds to the [LAPACK][lapack] level routine [`zlacon`][lapack-zlacon].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlacon = require( '@stdlib/lapack/base/zlacon' );

// TODO: Add examples
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[lapack]: https://www.netlib.org/lapack/explore-html/

[lapack-zlacon]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlacon.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->