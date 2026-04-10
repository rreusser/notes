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

# dtprfb

> Applies a real triangular-pentagonal block reflector to a matrix

<section class="usage">

## Usage

```javascript
var dtprfb = require( '@stdlib/lapack/base/dtprfb' );
```

#### dtprfb( order, side, trans, direct, storev, M, N, K, l, V, LDV, T, LDT, A, LDA, B, LDB, WORK, LDWORK )

Applies a real triangular-pentagonal block reflector to a matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var V = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 0.3, 1.0, 0.0, 1.0 ] );
var T = new Float64Array( [ 1.2, 0.0, -0.3, 0.8 ] );
var A = new Float64Array( [ 1.0, 4.0, 2.0, 5.0, 3.0, 6.0 ] );
var B = new Float64Array( [ 7.0, 10.0, 13.0, 16.0, 8.0, 11.0, 14.0, 17.0, 9.0, 12.0, 15.0, 18.0 ] );
var WORK = new Float64Array( 6 );

dtprfb( 'column-major', 'left', 'no-transpose', 'forward', 'columnwise', 4, 3, 2, 1, V, 4, T, 2, A, 2, B, 4, WORK, 2 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **side**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **direct**: specifies the operation type.
-   **storev**: specifies the operation type.
-   **M**: number of rows.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **l**: l.
-   **V**: input matrix.
-   **LDV**: leading dimension of `V`.
-   **T**: input matrix.
-   **LDT**: leading dimension of `T`.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **B**: input matrix.
-   **LDB**: leading dimension of `B`.
-   **WORK**: output matrix.
-   **LDWORK**: leading dimension of `WORK`.

#### dtprfb.ndarray( side, trans, direct, storev, M, N, K, l, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, WORK, strideWORK1, strideWORK2, offsetWORK )

Applies a real triangular-pentagonal block reflector to a matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var V = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 0.3, 1.0, 0.0, 1.0 ] );
var T = new Float64Array( [ 1.2, 0.0, -0.3, 0.8 ] );
var A = new Float64Array( [ 1.0, 4.0, 2.0, 5.0, 3.0, 6.0 ] );
var B = new Float64Array( [ 7.0, 10.0, 13.0, 16.0, 8.0, 11.0, 14.0, 17.0, 9.0, 12.0, 15.0, 18.0 ] );
var WORK = new Float64Array( 6 );

dtprfb.ndarray( 'left', 'no-transpose', 'forward', 'columnwise', 4, 3, 2, 1, V, 1, 4, 0, T, 1, 2, 0, A, 1, 2, 0, B, 1, 4, 0, WORK, 1, 2, 0 );
```

The function has the following additional parameters:

-   **side**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **direct**: specifies the operation type.
-   **storev**: specifies the operation type.
-   **M**: number of rows.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **l**: l.
-   **V**: input matrix.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.
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

-   The routine implements all eight combinations of `side`, `direct`, and `storev`.
-   When `l === 0`, `V` has no trapezoidal block and `dtprfb` behaves like `dlarfb`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dtprfb = require( '@stdlib/lapack/base/dtprfb' );

var V = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 0.3, 1.0, 0.0, 1.0 ] );
var T = new Float64Array( [ 1.2, 0.0, -0.3, 0.8 ] );
var A = new Float64Array( [ 1.0, 4.0, 2.0, 5.0, 3.0, 6.0 ] );
var B = new Float64Array( [ 7.0, 10.0, 13.0, 16.0, 8.0, 11.0, 14.0, 17.0, 9.0, 12.0, 15.0, 18.0 ] );
var WORK = new Float64Array( 6 );

dtprfb( 'column-major', 'left', 'no-transpose', 'forward', 'columnwise', 4, 3, 2, 1, V, 4, T, 2, A, 2, B, 4, WORK, 2 );
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
