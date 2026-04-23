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

# zgeqr2p

> Computes a complex QR factorization with non-negative diagonal elements (unblocked algorithm)

<section class="usage">

## Usage

```javascript
var zgeqr2p = require( '@stdlib/lapack/base/zgeqr2p' );
```

#### zgeqr2p( order, M, N, A, LDA, TAU, strideTAU, WORK, strideWORK )

Computes a complex QR factorization with non-negative diagonal elements (unblocked algorithm)

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
var TAU = new Complex128Array( 2 );
var WORK = new Complex128Array( 2 );

zgeqr2p( 'column-major', 2, 2, A, 2, TAU, 1, WORK, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.

#### zgeqr2p.ndarray( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK )

Computes a complex QR factorization with non-negative diagonal elements (unblocked algorithm), using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
var TAU = new Complex128Array( 2 );
var WORK = new Complex128Array( 2 );

zgeqr2p.ndarray( 2, 2, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zgeqr2p` is identical to [`zgeqr2`][@stdlib/lapack/base/zgeqr2] except that the diagonal elements of `R` are guaranteed to be real and non-negative.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zgeqr2p = require( '@stdlib/lapack/base/zgeqr2p' );

var A = new Complex128Array( [ 1.0, 0.5, 2.0, -0.5, 3.0, 0.25, 4.0, 0.0 ] );
var TAU = new Complex128Array( 2 );
var WORK = new Complex128Array( 2 );

zgeqr2p( 'column-major', 2, 2, A, 2, TAU, 1, WORK, 1 );
console.log( A );
console.log( TAU );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
