# dgetf2

> Computes an LU factorization of a general M-by-N matrix using partial pivoting with row interchanges (unblocked algorithm).

<section class="usage">

## Usage

```javascript
var dgetf2 = require( '@stdlib/lapack/base/dgetf2' );
```

#### dgetf2( order, M, N, A, LDA, IPIV, strideIPIV )

Computes an LU factorization of a general M-by-N matrix `A` using partial pivoting with row interchanges.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 2x2 matrix A (column-major): [1 3; 2 4]
var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var IPIV = new Int32Array( 2 );

var info = dgetf2( 'column-major', 2, 2, A, 2, IPIV, 1 );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout ('row-major' or 'column-major').
-   **M**: number of rows of matrix `A`.
-   **N**: number of columns of matrix `A`.
-   **A**: input matrix stored as a [`Float64Array`][mdn-float64array].
-   **LDA**: leading dimension of `A`.
-   **IPIV**: pivot index output array stored as an [`Int32Array`][mdn-int32array], length `min(M,N)`.
-   **strideIPIV**: stride length for `IPIV`.

#### dgetf2.ndarray( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV )

Computes an LU factorization using an alternative interface with stride and offset parameters.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var IPIV = new Int32Array( 2 );

var info = dgetf2.ndarray( 2, 2, A, 1, 2, 0, IPIV, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix as a [`Float64Array`][mdn-float64array].
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **IPIV**: pivot index output array as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A = P*L*U` where P is a permutation matrix, L is lower triangular with unit diagonal elements (lower trapezoidal if M > N), and U is upper triangular (upper trapezoidal if M < N).
-   `IPIV` stores 0-based pivot indices: row `i` was interchanged with row `IPIV[i]`.
-   Returns 0 if successful. If the return value is `k > 0`, then `U[k-1,k-1]` is exactly zero, indicating that the matrix is singular.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetf2 = require( '@stdlib/lapack/base/dgetf2' );

// 3x3 matrix A (column-major):
// [2 1 1]
// [4 3 3]
// [8 7 9]
var A = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
var IPIV = new Int32Array( 3 );

var info = dgetf2( 'column-major', 3, 3, A, 3, IPIV, 1 );
// info => 0
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
