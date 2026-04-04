# zsptri

> Computes the inverse of a complex symmetric matrix in packed storage using the factorization `A = U * D * U^T` or `A = L * D * L^T` computed by [`zsptrf`][@stdlib/lapack/base/zsptrf].

<section class="usage">

## Usage

```javascript
var zsptri = require( '@stdlib/lapack/base/zsptri' );
```

#### zsptri( uplo, N, AP, IPIV, WORK )

Computes the inverse of a complex symmetric matrix in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4.0, 2.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var WORK = new Complex128Array( 1 );

var info = zsptri( 'upper', 1, AP, IPIV, WORK );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **AP**: packed symmetric matrix as a [`Complex128Array`][@stdlib/array/complex128] of length `N*(N+1)/2` (overwritten with inverse).
-   **IPIV**: pivot indices from `zsptrf` as an [`Int32Array`][mdn-int32array].
-   **WORK**: workspace [`Complex128Array`][@stdlib/array/complex128] of length `N`.

#### zsptri.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK )

Computes the inverse with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4.0, 2.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var WORK = new Complex128Array( 1 );

var info = zsptri.ndarray( 'upper', 1, AP, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
// returns 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The matrix `A` must first be factored by [`zsptrf`][@stdlib/lapack/base/zsptrf] before calling `zsptri`.
-   `AP` is a packed representation of a complex symmetric (NOT Hermitian) matrix. For symmetric matrices, `A[i,j] = A[j,i]` (no conjugation).
-   IPIV is 0-based. Negative values encode 2x2 pivot blocks via bitwise NOT.
-   Returns `info`: 0 on success, `k > 0` if `D(k,k)` is zero (1-based).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsptrf = require( '@stdlib/lapack/base/zsptrf' );
var zsptri = require( '@stdlib/lapack/base/zsptri' );

// 2x2 complex symmetric matrix (upper packed): A = [ 2+i, 1+2i; 1+2i, 3+i ]
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, 1.0 ] );
var IPIV = new Int32Array( 2 );
var WORK = new Complex128Array( 2 );

// Factor and invert
zsptrf.ndarray( 'upper', 2, AP, 1, 0, IPIV, 1, 0 );
var info = zsptri.ndarray( 'upper', 2, AP, 1, 0, IPIV, 1, 0, WORK, 1, 0 );

var view = reinterpret( AP, 0 );
console.log( 'inv(A) packed:', Array.from( view ) );
```

</section>

<!-- /.examples -->

<section class="related">

</section>

<!-- /.related -->

<section class="links">

[@stdlib/lapack/base/zsptrf]: https://github.com/stdlib-js/stdlib
[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array

</section>

<!-- /.links -->
