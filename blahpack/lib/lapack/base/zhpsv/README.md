# zhpsv

> Computes the solution to a complex system of linear equations A * X = B where A is Hermitian in packed storage.

<section class="usage">

## Usage

```javascript
var zhpsv = require( '@stdlib/lapack/base/zhpsv' );
```

#### zhpsv( uplo, N, nrhs, AP, IPIV, B, LDB )

Computes the solution to a complex system of linear equations `A * X = B` where A is an N-by-N Hermitian matrix stored in packed format and B is an N-by-NRHS matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 Hermitian matrix (upper packed):
var AP = new Complex128Array( [ 4, 0, 1, 2, 5, 0, 2, -1, 3, 1, 6, 0 ] );
var B = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
var IPIV = new Int32Array( 3 );

var info = zhpsv( 'upper', 3, 1, AP, IPIV, B, 3 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of A is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns in B.
-   **AP**: packed Hermitian matrix as a [`Complex128Array`][@stdlib/array/complex128], length `N*(N+1)/2`.
-   **IPIV**: pivot index output array as an [`Int32Array`][mdn-int32array], length `N`.
-   **B**: input/output N-by-NRHS matrix as a [`Complex128Array`][@stdlib/array/complex128].
-   **LDB**: leading dimension of B.

#### zhpsv.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Computes the solution with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4, 0, 1, 2, 5, 0, 2, -1, 3, 1, 6, 0 ] );
var B = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
var IPIV = new Int32Array( 3 );

var info = zhpsv.ndarray( 'upper', 3, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The diagonal pivoting method is used to factor A as `A = U*D*U^H` (if uplo = `'upper'`) or `A = L*D*L^H` (if uplo = `'lower'`), where U (or L) is a product of permutation and unit upper (lower) triangular matrices, D is Hermitian and block diagonal with 1-by-1 and 2-by-2 diagonal blocks. The factored form of A is then used to solve the system `A*X = B`.
-   Returns `0` on success, or `k > 0` (1-based) if `D(k,k)` is exactly zero, indicating the matrix is singular and the solution could not be computed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpsv = require( '@stdlib/lapack/base/zhpsv' );

var AP = new Complex128Array( [ 4, 0, 1, 2, 5, 0, 2, -1, 3, 1, 6, 0 ] );
var B = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
var IPIV = new Int32Array( 3 );

var info = zhpsv( 'upper', 3, 1, AP, IPIV, B, 3 );

console.log( 'info: %d', info );
console.log( 'x (interleaved re/im):', reinterpret( B, 0 ) );
console.log( 'IPIV:', IPIV );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib/tree/main/lib/node_modules/%40stdlib/array/complex128

[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array

</section>

<!-- /.links -->
