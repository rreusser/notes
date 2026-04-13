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

# dlarfb_gett

> Applies a real Householder block reflector to a triangular-pentagonal matrix

<section class="usage">

## Usage

```javascript
var dlarfb_gett = require( '@stdlib/lapack/base/dlarfb_gett' );
```

#### dlarfb_gett( order, ident, M, N, K, T, LDT, A, LDA, B, LDB, WORK, LDWORK )

Applies a real Householder block reflector to a triangular-pentagonal matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 1.0, 0.5 ] );
var B = new Float64Array( [ 0.25, 0.5, 1.0, 3.0, 2.0, 4.0 ] );
var T = new Float64Array( [ 1.4 ] );
var WORK = new Float64Array( 3 );

dlarfb_gett( 'column-major', 'identity', 2, 3, 1, T, 1, A, 1, B, 2, WORK, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **ident**: specifies whether `V1` is the identity (`'identity'`) or a unit lower-triangular matrix stored in `A` (`'not-identity'`).
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

#### dlarfb_gett.ndarray( ident, M, N, K, T, strideT1, strideT2, offsetT, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, WORK, strideWORK1, strideWORK2, offsetWORK )

Applies a real Householder block reflector to a triangular-pentagonal matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 1.0, 0.5 ] );
var B = new Float64Array( [ 0.25, 0.5, 1.0, 3.0, 2.0, 4.0 ] );
var T = new Float64Array( [ 1.4 ] );
var WORK = new Float64Array( 3 );

dlarfb_gett.ndarray( 'identity', 2, 3, 1, T, 1, 1, 0, A, 1, 1, 0, B, 1, 2, 0, WORK, 1, 1, 0 );
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

-   `dlarfb_gett` applies a block Householder reflector `H` from the left to a `(K+M)`-by-`N` "triangular-pentagonal" matrix composed of an upper trapezoidal `K`-by-`N` block `A` and a rectangular `M`-by-`N` block `B`.
-   The elementary reflectors are held in the strictly lower triangle of the leading `K`-by-`K` block of `A` (`V1`, with implicit unit diagonal) and in the leading `M`-by-`K` block of `B` (`V2`). The upper triangular `K`-by-`K` matrix `T` defines the block reflector.
-   When `ident` is `'identity'`, `V1` is taken to be the identity and is not read from `A`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlarfb_gett = require( '@stdlib/lapack/base/dlarfb_gett' );

var A = new Float64Array( [ 2.0, 1.0, 0.5 ] );
var B = new Float64Array( [ 0.25, 0.5, 1.0, 3.0, 2.0, 4.0 ] );
var T = new Float64Array( [ 1.4 ] );
var WORK = new Float64Array( 3 );

dlarfb_gett( 'column-major', 'identity', 2, 3, 1, T, 1, A, 1, B, 2, WORK, 1 );
console.log( A );
console.log( B );
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
