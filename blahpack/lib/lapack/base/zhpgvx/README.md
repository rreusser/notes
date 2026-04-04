# zhpgvx

> Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.

<section class="usage">

## Usage

```javascript
var zhpgvx = require( '@stdlib/lapack/base/zhpgvx' );
```

#### zhpgvx( order, itype, jobz, range, uplo, N, AP, BP, vl, vu, il, iu, abstol, out, w, Z, LDZ, WORK, RWORK, IWORK, IFAIL )

Computes selected eigenvalues and, optionally, eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// A = [4 1-i; 1+i 5] in upper packed storage (complex interleaved):
var AP = new Complex128Array( [ 4, 0, 1, -1, 5, 0 ] );

// B = [2 0; 0 3] in upper packed storage:
var BP = new Complex128Array( [ 2, 0, 0, 0, 3, 0 ] );

var W = new Float64Array( 2 );
var Z = new Complex128Array( 4 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 20 );
var IWORK = new Int32Array( 15 );
var IFAIL = new Int32Array( 2 );
var out = { 'M': 0 };

var info = zhpgvx( 'column-major', 1, 'compute-vectors', 'all', 'upper', 2, AP, BP, 0, 0, 0, 0, 0, out, W, Z, 2, WORK, RWORK, IWORK, IFAIL );
// info => 0
// out.M => 2
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **itype**: problem type (1, 2, or 3).
-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues and eigenvectors).
-   **range**: `'all'`, `'value'`, or `'index'`.
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of A and B is stored.
-   **N**: order of matrices A and B.
-   **AP**: packed Hermitian matrix A (length `N*(N+1)/2`); on exit, overwritten.
-   **BP**: packed Hermitian positive definite matrix B (length `N*(N+1)/2`); on exit, Cholesky factor.
-   **vl**: lower bound of eigenvalue interval (range=`'value'`).
-   **vu**: upper bound of eigenvalue interval (range=`'value'`).
-   **il**: index of smallest eigenvalue to compute (1-based, range=`'index'`).
-   **iu**: index of largest eigenvalue to compute (1-based, range=`'index'`).
-   **abstol**: absolute tolerance for eigenvalues.
-   **out**: output object; `out.M` will be set to the number of eigenvalues found.
-   **w**: output array for eigenvalues (length `N`), in ascending order.
-   **Z**: output eigenvector matrix (complex); referenced only if jobz is `'compute-vectors'`.
-   **LDZ**: leading dimension of `Z` (in complex elements).
-   **WORK**: complex workspace array (length >= `2*N`).
-   **RWORK**: real workspace array (length >= `7*N`).
-   **IWORK**: integer workspace (length >= `5*N`).
-   **IFAIL**: output array for indices of non-converged eigenvectors (length `N`).

#### zhpgvx.ndarray( itype, jobz, range, uplo, N, AP, strideAP, offsetAP, BP, strideBP, offsetBP, vl, vu, il, iu, abstol, out, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and, optionally, eigenvectors with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4, 0, 1, -1, 5, 0 ] );
var BP = new Complex128Array( [ 2, 0, 0, 0, 3, 0 ] );
var W = new Float64Array( 2 );
var Z = new Complex128Array( 4 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 20 );
var IWORK = new Int32Array( 15 );
var IFAIL = new Int32Array( 2 );
var out = { 'M': 0 };

var info = zhpgvx.ndarray( 1, 'compute-vectors', 'all', 'upper', 2, AP, 1, 0, BP, 1, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 2, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// info => 0
// out.M => 2
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (complex elements).
-   **offsetAP**: starting index for `AP` (complex elements).
-   **strideBP**: stride length for `BP` (complex elements).
-   **offsetBP**: starting index for `BP` (complex elements).
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **strideZ1**: stride of the first dimension of `Z` (complex elements).
-   **strideZ2**: stride of the second dimension of `Z` (complex elements).
-   **offsetZ**: starting index for `Z` (complex elements).
-   **strideWORK**: stride length for `WORK` (complex elements).
-   **offsetWORK**: starting index for `WORK` (complex elements).
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **strideIFAIL**: stride length for `IFAIL`.
-   **offsetIFAIL**: starting index for `IFAIL`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zhpgvx` solves `A*x = lambda*B*x` (itype=1), `A*B*x = lambda*x` (itype=2), or `B*A*x = lambda*x` (itype=3).
-   Both A and B must be Hermitian. B must also be positive definite.
-   On exit, AP is destroyed and BP contains the Cholesky factor of B.
-   The number of eigenvalues found is returned in `out.M`.
-   Eigenvalues are always real for Hermitian-definite problems.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zhpgvx = require( '@stdlib/lapack/base/zhpgvx' );

// A = [4 1-i; 1+i 5] in upper packed storage:
var AP = new Complex128Array( [ 4, 0, 1, -1, 5, 0 ] );

// B = [2 0; 0 3] in upper packed storage:
var BP = new Complex128Array( [ 2, 0, 0, 0, 3, 0 ] );

var W = new Float64Array( 2 );
var Z = new Complex128Array( 4 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 20 );
var IWORK = new Int32Array( 15 );
var IFAIL = new Int32Array( 2 );
var out = { 'M': 0 };

var info = zhpgvx( 'column-major', 1, 'compute-vectors', 'all', 'upper', 2, AP, BP, 0, 0, 0, 0, 0, out, W, Z, 2, WORK, RWORK, IWORK, IFAIL );

console.log( 'info:', info );
console.log( 'M (eigenvalues found):', out.M );
console.log( 'Eigenvalues:', W );
console.log( 'Eigenvectors:', Z );
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
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array

</section>

<!-- /.links -->
