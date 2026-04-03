# zsptrf

> Computes the Bunch-Kaufman factorization of a complex symmetric matrix in packed storage.

<section class="usage">

## Usage

```javascript
var zsptrf = require( '@stdlib/lapack/base/zsptrf' );
```

#### zsptrf( uplo, N, AP, IPIV )

Computes the Bunch-Kaufman factorization of a complex symmetric matrix `A` stored in packed format using the Bunch-Kaufman diagonal pivoting method.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 complex symmetric matrix (lower packed):
var AP = new Complex128Array( [ 4.0, 1.0, 2.0, -1.0, 1.0, 2.0, 5.0, 0.5, 3.0, -1.0, 6.0, 1.0 ] );
var IPIV = new Int32Array( 3 );

var info = zsptrf( 'lower', 3, AP, IPIV );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: packed complex symmetric matrix as a [`Complex128Array`][@stdlib/array/complex128] of length `N*(N+1)/2`.
-   **IPIV**: pivot index output array as an [`Int32Array`][mdn-int32array] of length `N`.

#### zsptrf.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV )

Computes the Bunch-Kaufman factorization with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4.0, 1.0, 2.0, -1.0, 1.0, 2.0, 5.0, 0.5, 3.0, -1.0, 6.0, 1.0 ] );
var IPIV = new Int32Array( 3 );

var info = zsptrf.ndarray( 'lower', 3, AP, 1, 0, IPIV, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: index offset for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A = U*D*U^T` (if uplo = `'upper'`) or `A = L*D*L^T` (if uplo = `'lower'`), where `U` (or `L`) is a product of permutation and unit upper (lower) triangular matrices, and `D` is symmetric and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
-   This routine is for **symmetric** (not Hermitian) matrices. The transpose is used (not the conjugate transpose). The diagonal elements are fully complex.
-   `IPIV` stores 0-based pivot indices. If `IPIV[k] >= 0`, a 1x1 pivot was used. If `IPIV[k] < 0` (for a 2x2 pivot), then `IPIV[k] = IPIV[k+/-1] = ~p` where `p` is the 0-based row/column that was interchanged.
-   The function returns an `info` value: `0` indicates success; a positive value `k` indicates that `D(k,k)` is exactly zero (1-based indexing).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsptrf = require( '@stdlib/lapack/base/zsptrf' );

// 3x3 complex symmetric matrix (lower packed):
var AP = new Complex128Array( [ 4.0, 1.0, 2.0, -1.0, 1.0, 2.0, 5.0, 0.5, 3.0, -1.0, 6.0, 1.0 ] );
var IPIV = new Int32Array( 3 );

var info = zsptrf( 'lower', 3, AP, IPIV );

console.log( 'info:', info );
console.log( 'AP (factored):', reinterpret( AP, 0 ) );
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

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array

</section>

<!-- /.links -->
