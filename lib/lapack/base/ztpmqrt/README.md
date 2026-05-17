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

# ztpmqrt

> Applies a complex unitary matrix `Q` (or its conjugate-transpose) obtained from a triangular-pentagonal compact-WY block reflector — the output of `ztpqrt` — to a stacked matrix `C` formed by two blocks `A` and `B`.

<section class="usage">

## Usage

```javascript
var ztpmqrt = require( '@stdlib/lapack/base/ztpmqrt' );
```

#### ztpmqrt( order, side, trans, M, N, K, l, nb, V, LDV, T, LDT, A, LDA, B, LDB, WORK, strideWORK )

Applies `Q`, `Q^H`, `Q*C`, or `C*Q^H` (according to `side` and `trans`) in place on the stacked matrix `[A; B]` (left) or `[A B]` (right), where `Q` is the unitary factor produced by `ztpqrt`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var V = new Complex128Array( 12 );
var T = new Complex128Array( 6 );
var A = new Complex128Array( 9 );
var B = new Complex128Array( 12 );
var WORK = new Complex128Array( 6 );

ztpmqrt( 'column-major', 'left', 'no-transpose', 4, 3, 3, 2, 2, V, 4, T, 2, A, 3, B, 4, WORK, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **side**: `'left'` to apply `Q` from the left, `'right'` from the right.
-   **trans**: `'no-transpose'` to apply `Q`, `'conjugate-transpose'` to apply `Q^H`. Plain `'transpose'` is rejected because `Q` is unitary.
-   **M**: number of rows of `B`.
-   **N**: number of columns of `B`.
-   **K**: number of elementary reflectors.
-   **l**: order of the trapezoidal block of `V` (`0 <= L <= K`). `L = 0` reduces `V` to a rectangular block; `L = K` reduces `V` to a triangular block.
-   **nb**: block size used to construct `T` (must equal the value used in `ztpqrt`).
-   **V**: pentagonal reflector matrix produced by `ztpqrt`.
-   **LDV**: leading dimension of `V`.
-   **T**: block triangular factor produced by `ztpqrt`.
-   **LDT**: leading dimension of `T` (must be `>= nb`).
-   **A**: upper (left) or left (right) block of `C`, modified in-place.
-   **LDA**: leading dimension of `A`.
-   **B**: lower (left) or right (right) block of `C`, modified in-place.
-   **LDB**: leading dimension of `B` (`>= max(1,M)`).
-   **WORK**: workspace buffer (at least `N*nb` complex elements for `side='left'` or `M*nb` for `side='right'`).
-   **strideWORK**: element stride for `WORK` (in complex elements).

#### ztpmqrt.ndarray( side, trans, M, N, K, l, nb, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, WORK, strideWORK, offsetWORK )

Same operation as `ztpmqrt`, using alternative indexing semantics with explicit strides and offsets (in complex elements).

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var V = new Complex128Array( 12 );
var T = new Complex128Array( 6 );
var A = new Complex128Array( 9 );
var B = new Complex128Array( 12 );
var WORK = new Complex128Array( 6 );

ztpmqrt.ndarray( 'left', 'no-transpose', 4, 3, 3, 2, 2, V, 1, 4, 0, T, 1, 2, 0, A, 1, 3, 0, B, 1, 4, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **strideV1**: stride of dimension 1 of `V` (in complex elements).
-   **strideV2**: stride of dimension 2 of `V` (in complex elements).
-   **offsetV**: starting index for `V` (in complex elements).
-   **strideT1**: stride of dimension 1 of `T` (in complex elements).
-   **strideT2**: stride of dimension 2 of `T` (in complex elements).
-   **offsetT**: starting index for `T` (in complex elements).
-   **strideA1**: stride of dimension 1 of `A` (in complex elements).
-   **strideA2**: stride of dimension 2 of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **strideB1**: stride of dimension 1 of `B` (in complex elements).
-   **strideB2**: stride of dimension 2 of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The pentagonal `V` is composed of an `(M-L)`-by-`K` (left) or `(N-L)`-by-`K` (right) rectangular block stacked on top of an `L`-by-`K` upper trapezoidal block (the first `L` rows of a `K`-by-`K` upper triangular matrix).
-   The `WORK` buffer should provide at least `N*nb` complex elements (`side='left'`) or `M*nb` complex elements (`side='right'`). If the buffer is too small, an internal scratch array is allocated automatically.
-   `V` and `T` must come from a prior call to `ztpqrt` with the same block size `nb` and the same value of `L`.
-   Plain `'transpose'` (`Q^T`) is **not** a valid `trans` value: `Q` is unitary and only the no-transpose and conjugate-transpose operators are physically meaningful.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var ztpmqrt = require( '@stdlib/lapack/base/ztpmqrt' );

var V = new Complex128Array( 12 );
var T = new Complex128Array( 6 );
var A = new Complex128Array( 9 );
var B = new Complex128Array( 12 );
var WORK = new Complex128Array( 6 );

ztpmqrt.ndarray( 'left', 'no-transpose', 4, 3, 3, 2, 2, V, 1, 4, 0, T, 1, 2, 0, A, 1, 3, 0, B, 1, 4, 0, WORK, 1, 0 );
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
