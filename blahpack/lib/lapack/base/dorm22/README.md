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

# dorm22

> Multiplies a general matrix by a banded orthogonal matrix

<section class="usage">

## Usage

```javascript
var dorm22 = require( '@stdlib/lapack/base/dorm22' );
```

#### dorm22( order, side, trans, M, N, n1, n2, Q, LDQ, C, LDC, WORK, strideWORK, lwork )

Multiplies a general matrix by a banded orthogonal matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var Q = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var WORK = new Float64Array( 4 );
dorm22( 'column-major', 'left', 'no-transpose', 2, 2, 1, 1, Q, 2, C, 2, WORK, 1, 4 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **side**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **M**: number of rows.
-   **N**: number of columns.
-   **n1**: n1.
-   **n2**: n2.
-   **Q**: input matrix.
-   **LDQ**: leading dimension of `Q`.
-   **C**: input matrix.
-   **LDC**: leading dimension of `C`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **lwork**: lwork.

#### dorm22.ndarray( side, trans, M, N, n1, n2, Q, strideQ1, strideQ2, offsetQ, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork )

Multiplies a general matrix by a banded orthogonal matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var Q = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var WORK = new Float64Array( 4 );
dorm22.ndarray( 'left', 'no-transpose', 2, 2, 1, 1, Q, 1, 2, 0, C, 1, 2, 0, WORK, 1, 0, 4 );
```

The function has the following additional parameters:

-   **side**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **M**: number of rows.
-   **N**: number of columns.
-   **n1**: n1.
-   **n2**: n2.
-   **Q**: input matrix.
-   **strideQ1**: stride of dimension 1 of `Q`.
-   **strideQ2**: stride of dimension 2 of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **C**: input matrix.
-   **strideC1**: stride of dimension 1 of `C`.
-   **strideC2**: stride of dimension 2 of `C`.
-   **offsetC**: starting index for `C`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The orthogonal matrix `Q` is assumed to have a 2x2 block structure: an upper-left rectangular block, a lower-triangular block, an upper-triangular block, and a lower-right rectangular block. It is typically produced by the multi-shift QR iteration (`dlaqr2`/`dlaqr3`).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dorm22 = require( '@stdlib/lapack/base/dorm22' );

var Q = new Float64Array( [ 0.8, 0.6, -0.6, 0.8 ] );
var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var WORK = new Float64Array( 4 );

var info = dorm22( 'column-major', 'left', 'no-transpose', 2, 2, 1, 1, Q, 2, C, 2, WORK, 1, 4 );
console.log( info );
console.log( C );
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
