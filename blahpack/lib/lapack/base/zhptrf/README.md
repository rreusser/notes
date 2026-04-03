# zhptrf

> Computes the Bunch-Kaufman factorization of a complex Hermitian matrix in packed storage.

<section class="usage">

## Usage

```javascript
var zhptrf = require( '@stdlib/lapack/base/zhptrf' );
```

#### zhptrf( uplo, N, AP, IPIV )

Computes the Bunch-Kaufman factorization of a complex Hermitian matrix `A` stored in packed format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 Hermitian positive definite matrix (lower packed):
var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
var IPIV = new Int32Array( 3 );

var info = zhptrf( 'lower', 3, AP, IPIV );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: packed Hermitian matrix as a [`Complex128Array`][@stdlib/array/complex128], length `N*(N+1)/2`.
-   **IPIV**: pivot index output array as an [`Int32Array`][mdn-int32array], length `N`.

#### zhptrf.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV )

Computes the Bunch-Kaufman factorization with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
var IPIV = new Int32Array( 3 );

var info = zhptrf.ndarray( 'lower', 3, AP, 1, 0, IPIV, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A = U*D*U^H` (if uplo = `'upper'`) or `A = L*D*L^H` (if uplo = `'lower'`), where U (or L) is a product of permutation and unit upper (lower) triangular matrices, and D is Hermitian and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
-   Diagonal elements of a Hermitian matrix are real. The routine enforces this by zeroing imaginary parts on the diagonal.
-   IPIV stores 0-based pivot indices. If `IPIV[k] >= 0`, a 1x1 pivot was used. If `IPIV[k] < 0`, a 2x2 pivot was used and `IPIV[k] = ~p` where `p` is the 0-based interchange index.
-   Returns `0` on success, or `k > 0` (1-based) if `D(k,k)` is exactly zero, indicating the matrix is singular.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhptrf = require( '@stdlib/lapack/base/zhptrf' );

// 3x3 Hermitian matrix (lower packed):
// A = [ 4       1-2i    3+i  ]
//     [ 1+2i    5       2-i  ]
//     [ 3-i     2+i     7    ]
var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
var IPIV = new Int32Array( 3 );

var info = zhptrf( 'lower', 3, AP, IPIV );
console.log( 'info:', info );
console.log( 'AP:', reinterpret( AP, 0 ) );
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
