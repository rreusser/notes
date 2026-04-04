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

# dlanhs

> Return the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix.

<section class="usage">

## Usage

```javascript
var dlanhs = require( '@stdlib/lapack/base/dlanhs' );
```

#### dlanhs( order, norm, N, A, LDA, WORK, strideWORK )

Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 1.0, 4.0, 0.0, 2.0, 5.0, 7.0, 3.0, 6.0, 8.0 ] );
var WORK = new Float64Array( 3 );

var result = dlanhs( 'column-major', 'one-norm', 3, A, 3, WORK, 1 );
// returns 17.0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **norm**: specifies the norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **N**: order of the matrix `A`.
-   **A**: input upper Hessenberg matrix stored as a [`Float64Array`][mdn-float64array].
-   **LDA**: leading dimension of `A`.
-   **WORK**: workspace array as a [`Float64Array`][mdn-float64array] (length >= N for `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.

#### dlanhs.ndarray( norm, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK )

Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 1.0, 4.0, 0.0, 2.0, 5.0, 7.0, 3.0, 6.0, 8.0 ] );
var WORK = new Float64Array( 3 );

var result = dlanhs.ndarray( 'one-norm', 3, A, 1, 3, 0, WORK, 1, 0 );
// returns 17.0
```

The function has the following parameters:

-   **norm**: specifies the norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **N**: order of the matrix.
-   **A**: input upper Hessenberg matrix as a [`Float64Array`][mdn-float64array].
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **WORK**: workspace array as a [`Float64Array`][mdn-float64array] (length >= N for `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   An upper Hessenberg matrix has zeros below the first subdiagonal. That is, `A[i][j] = 0` for `i > j + 1`.
-   The `WORK` array is only referenced when `norm` is `'inf-norm'`.
-   The norm types are:
    -   `'max'`: max absolute value of any element.
    -   `'one-norm'`: maximum column sum of absolute values.
    -   `'inf-norm'`: maximum row sum of absolute values.
    -   `'frobenius'`: square root of the sum of squares of all elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlanhs = require( '@stdlib/lapack/base/dlanhs' );

// 3x3 upper Hessenberg matrix (column-major):
var A = new Float64Array( [ 1.0, 4.0, 0.0, 2.0, 5.0, 7.0, 3.0, 6.0, 8.0 ] );
var WORK = new Float64Array( 3 );

var maxNorm = dlanhs( 'column-major', 'max', 3, A, 3, WORK, 1 );
// returns 8.0

var oneNorm = dlanhs( 'column-major', 'one-norm', 3, A, 3, WORK, 1 );
// returns 17.0

var infNorm = dlanhs( 'column-major', 'inf-norm', 3, A, 3, WORK, 1 );
// returns 15.0
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
