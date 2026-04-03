# zgetf2

> Computes an LU factorization of a general M-by-N complex matrix using partial pivoting with row interchanges (unblocked algorithm).

<section class="usage">

## Usage

```javascript
var zgetf2 = require( '@stdlib/lapack/base/zgetf2' );
```

#### zgetf2.ndarray( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV )

Computes an LU factorization of a general M-by-N complex matrix `A` using partial pivoting with row interchanges.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 2.0, 1.0, 4.0, 2.0, 1.0, 0.5, 3.0, 1.0 ] );
var IPIV = new Int32Array( 2 );

var info = zgetf2.ndarray( 2, 2, A, 1, 2, 0, IPIV, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **M**: number of rows of matrix A.
-   **N**: number of columns of matrix A.
-   **A**: input/output complex matrix stored as a `Complex128Array` (column-major).
-   **strideA1**: stride of the first dimension of `A` (complex elements).
-   **strideA2**: stride of the second dimension of `A` (complex elements).
-   **offsetA**: index offset for `A` (complex elements).
-   **IPIV**: pivot index output array (`Int32Array`), length `min(M,N)`. Stores 0-based pivot indices.
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: index offset for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A = P*L*U` where P is a permutation matrix, L is lower triangular with unit diagonal elements (lower trapezoidal if m > n), and U is upper triangular (upper trapezoidal if m < n).
-   Returns an integer `info`: 0 if successful, or k (1-based) if `U(k-1,k-1)` is exactly zero, indicating a singular factor.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetf2 = require( '@stdlib/lapack/base/zgetf2' );

var A = new Complex128Array([
    2.0, 1.0, 4.0, 2.0, 8.0, 3.0,
    1.0, 0.5, 3.0, 1.0, 7.0, 2.0,
    1.0, 0.1, 3.0, 0.5, 9.0, 1.0
]);
var IPIV = new Int32Array( 3 );

var info = zgetf2.ndarray( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );

var view = reinterpret( A, 0 );
console.log( view );
// => Float64Array [ 8.0, 3.0, ... ]
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
