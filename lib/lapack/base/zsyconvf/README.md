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

# zsyconvf

> Converts the factorization output format used in zsytrf for complex symmetric matrices

<section class="usage">

## Usage

```javascript
var zsyconvf = require( '@stdlib/lapack/base/zsyconvf' );
```

#### zsyconvf( order, uplo, way, N, A, LDA, e, strideE, IPIV, strideIPIV, offsetIPIV )

Converts the factorization output format used in zsytrf for complex symmetric matrices

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( 4 );
var E = new Complex128Array( 2 );
var IPIV = new Int32Array( [ 0, 1 ] );

zsyconvf( 'column-major', 'upper', 'convert', 2, A, 2, E, 1, IPIV, 1, 0 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies the operation type.
-   **way**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **IPIV**: output array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

#### zsyconvf.ndarray( uplo, way, N, A, strideA1, strideA2, offsetA, e, strideE, offsetE, IPIV, strideIPIV, offsetIPIV )

Converts the factorization output format used in zsytrf for complex symmetric matrices, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( 4 );
var E = new Complex128Array( 2 );
var IPIV = new Int32Array( [ 0, 1 ] );

zsyconvf.ndarray( 'upper', 'convert', 2, A, 1, 2, 0, E, 1, 0, IPIV, 1, 0 );
```

The function has the following additional parameters:

-   **uplo**: specifies the operation type.
-   **way**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `E`.
-   **IPIV**: output array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   When `way='convert'`, the routine extracts the off-diagonal elements of the 2x2 blocks of `D` from `A` into `E`, zeros them in `A`, and rewrites the second entry of each 2x2 `IPIV` pair to be self-pivoting.
-   When `way='revert'`, the routine performs the inverse operation, reinserting `E` into `A` and restoring the original `IPIV` encoding.
-   `A` and `E` are `Complex128Array`s; `IPIV` is an `Int32Array` of 0-based indices where negative values encode 2x2 pivot blocks.
-   No conjugation is performed: this routine treats `A` as symmetric, not Hermitian.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zsyconvf = require( '@stdlib/lapack/base/zsyconvf' );

var N = 3;
var A = new Complex128Array( N * N );
var E = new Complex128Array( N );
var IPIV = new Int32Array( [ 0, 1, 2 ] );

var info = zsyconvf( 'column-major', 'upper', 'convert', N, A, N, E, 1, IPIV, 1, 0 );
console.log( info );
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
