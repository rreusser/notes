# zhbgv

> Computes all eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem A\_x = lambda\_B\_x.

<section class="usage">

## Usage

```javascript
var zhbgv = require( '@stdlib/lapack/base/zhbgv' );
```

#### zhbgv.ndarray( jobz, uplo, N, ka, kb, AB, strideAB1, strideAB2, offsetAB, BB, strideBB1, strideBB2, offsetBB, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Computes all eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );

// 3x3 diagonal matrices (KA=KB=0), column-major:
var AB = new Complex128Array( 3 );
var BB = new Complex128Array( 3 );
var v = reinterpret( AB, 0 );
v[ 0 ] = 5.0; v[ 2 ] = 6.0; v[ 4 ] = 7.0;
v = reinterpret( BB, 0 );
v[ 0 ] = 2.0; v[ 2 ] = 3.0; v[ 4 ] = 4.0;
var W = new Float64Array( 3 );
var Z = new Complex128Array( 9 );
var WORK = new Complex128Array( 3 );
var RWORK = new Float64Array( 9 );

var info = zhbgv.ndarray( 'compute-vectors', 'upper', 3, 0, 0, AB, 1, 1, 0, BB, 1, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// W => [ 1.75, 2.0, 2.5 ]
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` to compute eigenvalues only, `'compute-vectors'` to also compute eigenvectors.
-   **uplo**: `'upper'` or `'lower'` triangles of A and B are stored.
-   **N**: order of matrices A and B.
-   **ka**: number of super- (or sub-) diagonals of A.
-   **kb**: number of super- (or sub-) diagonals of B.
-   **AB**: [`Complex128Array`][@stdlib/array/complex128] band matrix A in band storage, dimension (ka+1, N).
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **BB**: [`Complex128Array`][@stdlib/array/complex128] band matrix B in band storage, dimension (kb+1, N).
-   **strideBB1**: stride of the first dimension of `BB` (in complex elements).
-   **strideBB2**: stride of the second dimension of `BB` (in complex elements).
-   **offsetBB**: starting index for `BB` (in complex elements).
-   **w**: [`Float64Array`][mdn-float64array] output array for eigenvalues (length N).
-   **strideW**: stride for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: [`Complex128Array`][@stdlib/array/complex128] output matrix for eigenvectors (N-by-N), referenced only if jobz = `'compute-vectors'`.
-   **strideZ1**: stride of the first dimension of `Z` (in complex elements).
-   **strideZ2**: stride of the second dimension of `Z` (in complex elements).
-   **offsetZ**: starting index for `Z` (in complex elements).
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] workspace array (length >= N).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: [`Float64Array`][mdn-float64array] real workspace array (length >= 3\*N).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   A and B are assumed to be Hermitian and banded. B must also be positive definite.
-   On exit, AB is destroyed. BB contains the split Cholesky factor from `zpbstf`.
-   If `info > 0` and `info <= N`, dsteqr/dsterf did not converge. If `info > N`, B is not positive definite (`info - N` is the return value from `zpbstf`).
-   The eigenvalues are returned in ascending order in `w`.
-   If jobz = `'compute-vectors'`, the eigenvectors are normalized so that Z^H\_B\_Z = I.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhbgv = require( '@stdlib/lapack/base/zhbgv' );

// 3x3 diagonal Hermitian matrices (KA=KB=0):
var AB = new Complex128Array( 3 );
var BB = new Complex128Array( 3 );
var v = reinterpret( AB, 0 );
v[ 0 ] = 5.0; v[ 2 ] = 6.0; v[ 4 ] = 7.0;
v = reinterpret( BB, 0 );
v[ 0 ] = 2.0; v[ 2 ] = 3.0; v[ 4 ] = 4.0;
var W = new Float64Array( 3 );
var Z = new Complex128Array( 9 );
var WORK = new Complex128Array( 3 );
var RWORK = new Float64Array( 9 );

var info = zhbgv.ndarray( 'compute-vectors', 'upper', 3, 0, 0, AB, 1, 1, 0, BB, 1, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info );
console.log( 'Eigenvalues:', W );
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
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray
[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
