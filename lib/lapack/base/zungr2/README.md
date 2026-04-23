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

# zungr2

> Generates an M-by-N complex matrix Q with unitary columns from an RQ factorization determined by `zgerqf` (unblocked algorithm).

<section class="usage">

## Usage

```javascript
var zungr2 = require( '@stdlib/lapack/base/zungr2' );
```

#### zungr2( order, M, N, K, A, LDA, TAU, strideTAU, WORK, strideWORK )

Generates an M-by-N complex matrix Q with unitary columns, defined as the product of K elementary reflectors as returned by `zgerqf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( 4 );
var TAU = new Complex128Array( 1 );
var WORK = new Complex128Array( 2 );

var info = zungr2( 'column-major', 2, 2, 0, A, 2, TAU, 1, WORK, 1 );
// returns 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of Q (M >= 0).
-   **N**: number of columns of Q (N >= M).
-   **K**: number of elementary reflectors (0 <= K <= M).
-   **A**: input matrix stored as a [`Complex128Array`][@stdlib/array/complex128].
-   **LDA**: leading dimension of `A`.
-   **TAU**: scalar factors of reflectors as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideTAU**: stride length for `TAU`.
-   **WORK**: workspace as a [`Complex128Array`][@stdlib/array/complex128] of length at least M.
-   **strideWORK**: stride length for `WORK`.

#### zungr2.ndarray( M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK )

Generates an M-by-N complex matrix Q with unitary columns using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( 4 );
var TAU = new Complex128Array( 1 );
var WORK = new Complex128Array( 2 );

var info = zungr2.ndarray( 2, 2, 0, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
// returns 0
```

The function has the following additional parameters:

-   **M**: number of rows of Q.
-   **N**: number of columns of Q.
-   **K**: number of elementary reflectors.
-   **A**: input matrix as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **TAU**: scalar factors as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **WORK**: workspace as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zungr2` generates Q = H(1)^H \* H(2)^H \* ... \* H(K)^H, where each H(i) is an elementary reflector of the form `H(i) = I - tau(i) * v * v^H`.
-   On entry, the last K rows of A must contain the reflector vectors as returned by `zgerqf`. On exit, A contains the M-by-N matrix Q.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zungr2 = require( '@stdlib/lapack/base/zungr2' );

// Generate a 2x2 identity matrix (K=0):
var A = new Complex128Array( 4 );
var TAU = new Complex128Array( 1 );
var WORK = new Complex128Array( 2 );

var info = zungr2.ndarray( 2, 2, 0, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
// returns 0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
