# dgetrf

> Compute an LU factorization of a general M-by-N matrix `A` using partial pivoting with row interchanges.

<section class="usage">

## Usage

```javascript
var dgetrf = require( '@stdlib/lapack/base/dgetrf' );
```

#### dgetrf( order, M, N, A, LDA, IPIV, strideIPIV )

Computes an LU factorization of a general M-by-N matrix `A` using partial pivoting with row interchanges. The factorization has the form `A = P * L * U` where `P` is a permutation matrix, `L` is lower triangular with unit diagonal elements, and `U` is upper triangular.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var IPIV = new Int32Array( 2 );

var info = dgetrf( 'row-major', 2, 2, A, 2, IPIV, 1 );
// returns 0
```

The function has the following parameters:

-   **order**: storage layout, either `'row-major'` or `'column-major'`.
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **A**: input matrix stored as a [`Float64Array`][mdn-float64array].
-   **LDA**: leading dimension of `A`.
-   **IPIV**: pivot index output array stored as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride length for `IPIV`.

The function returns an `info` status code. A value of `0` indicates success. A value of `k > 0` indicates that `U(k-1,k-1)` is exactly zero, so the factorization has been completed but the factor `U` is exactly singular.

#### dgetrf.ndarray( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV )

Computes an LU factorization of a general M-by-N matrix `A` using partial pivoting with row interchanges and alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var IPIV = new Int32Array( 2 );

var info = dgetrf.ndarray( 2, 2, A, 1, 2, 0, IPIV, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **A**: input matrix stored as a [`Float64Array`][mdn-float64array].
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **IPIV**: pivot index output array stored as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `IPIV` stores 0-based pivot indices: row `i` was interchanged with row `IPIV[i]`.
-   The blocked algorithm uses a block size of 64. For matrices with `min(M,N) <= 64`, the unblocked algorithm ([`dgetrf2`][@stdlib/lapack/base/dgetrf2]) is used directly.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetrf = require( '@stdlib/lapack/base/dgetrf' );

// 3x3 column-major matrix:
var A = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
var IPIV = new Int32Array( 3 );

var info = dgetrf( 'column-major', 3, 3, A, 3, IPIV, 1 );
console.log( 'info: ' + info );
console.log( 'A (factored): ' + A );
console.log( 'IPIV: ' + IPIV );
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
