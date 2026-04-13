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

# zlarfb_gett

> Applies a complex Householder block reflector to a triangular-pentagonal matrix

<section class="usage">

## Usage

```javascript
var zlarfb_gett = require( '@stdlib/lapack/base/zlarfb_gett' );
```

#### zlarfb_gett( order, ident, M, N, K, T, LDT, A, LDA, B, LDB, WORK, LDWORK )

Applies a complex Householder block reflector to a triangular-pentagonal matrix

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var K = 2;
var M = 3;
var N = 3;

var T = new Complex128Array( K * K );
var A = new Complex128Array( K * N );
var B = new Complex128Array( M * N );
var WORK = new Complex128Array( K * Math.max( K, N - K ) );

zlarfb_gett( 'column-major', 'identity', M, N, K, T, K, A, K, B, M, WORK, K );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **ident**: `'identity'` (`V1` is the identity) or `'not-identity'` (`V1` is unit lower-triangular, stored in `A`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **T**: input matrix.
-   **LDT**: leading dimension of `T`.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **B**: input matrix.
-   **LDB**: leading dimension of `B`.
-   **WORK**: output matrix.
-   **LDWORK**: leading dimension of `WORK`.

#### zlarfb_gett.ndarray( ident, M, N, K, T, strideT1, strideT2, offsetT, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, WORK, strideWORK1, strideWORK2, offsetWORK )

Applies a complex Householder block reflector to a triangular-pentagonal matrix, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var K = 2;
var M = 3;
var N = 3;

var T = new Complex128Array( K * K );
var A = new Complex128Array( K * N );
var B = new Complex128Array( M * N );
var WORK = new Complex128Array( K * Math.max( K, N - K ) );

zlarfb_gett.ndarray( 'identity', M, N, K, T, 1, K, 0, A, 1, K, 0, B, 1, M, 0, WORK, 1, K, 0 );
```

The function has the following additional parameters:

-   **ident**: specifies the operation type.
-   **M**: number of rows.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **T**: input matrix.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input matrix.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.
-   **WORK**: output matrix.
-   **strideWORK1**: stride of dimension 1 of `WORK`.
-   **strideWORK2**: stride of dimension 2 of `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On input, the upper-trapezoidal `K`-by-`N` block is stored in `A`; when `ident = 'not-identity'` the columns of `V1` (unit lower-triangular) occupy the strict lower triangle of `A(1:K,1:K)`. On output, `A` is overwritten by `H*A`.
-   The reflector `V2` is stored in the first `K` columns of `B`; on output, `B` contains the rectangular product `H*B`.
-   `WORK` must have dimension at least `K`-by-`max(K, N-K)`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlarfb_gett = require( '@stdlib/lapack/base/zlarfb_gett' );

var K = 2;
var M = 3;
var N = 4;

var T = new Complex128Array( K * K );
var A = new Complex128Array( K * N );
var B = new Complex128Array( M * N );
var WORK = new Complex128Array( K * Math.max( K, N - K ) );

zlarfb_gett.ndarray( 'identity', M, N, K, T, 1, K, 0, A, 1, K, 0, B, 1, M, 0, WORK, 1, K, 0 );
console.log( A );
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
