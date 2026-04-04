# zhbgvx

> Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.

<section class="usage">

## Usage

```javascript
var zhbgvx = require( '@stdlib/lapack/base/zhbgvx' );
```

#### zhbgvx.ndarray( jobz, range, uplo, N, ka, kb, AB, strideAB1, strideAB2, offsetAB, BB, strideBB1, strideBB2, offsetBB, Q, strideQ1, strideQ2, offsetQ, vl, vu, il, iu, abstol, M, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );

// 5x5 Hermitian band matrix A (KA=2), upper storage:
var AB = new Complex128Array( [ 0, 0, 0, 0, 10, 0, 0, 0, 1, 0.5, 8, 0, 0.5, 0.1, 2, -0.3, 6, 0, 0.3, -0.2, 1.5, 0.2, 9, 0, 0.4, 0.15, 1, -0.4, 7, 0 ] );
// 5x5 Hermitian positive definite band matrix B (KB=1), upper storage:
var BB = new Complex128Array( [ 0, 0, 4, 0, 0.2, 0.1, 5, 0, 0.3, -0.1, 3, 0, 0.1, 0.05, 6, 0, 0.2, -0.1, 4, 0 ] );
var Q = new Complex128Array( 25 );
var W = new Float64Array( 5 );
var Z = new Complex128Array( 25 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 5 );
var out = { M: 0 };

zhbgvx.ndarray( 'compute-vectors', 'all', 'upper', 5, 2, 1, AB, 1, 3, 0, BB, 1, 2, 0, Q, 1, 5, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 5, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// out.M => 5
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` or `'compute-vectors'`.
-   **range**: `'all'`, `'value'`, or `'index'`.
-   **uplo**: `'upper'` or `'lower'`.
-   **N**: order of matrices A and B.
-   **ka**: number of super- (or sub-) diagonals of A.
-   **kb**: number of super- (or sub-) diagonals of B.
-   **AB**: [`Complex128Array`][@stdlib/array/complex128] band matrix A in band storage.
-   **strideAB1**: stride of the first dimension of `AB` (complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (complex elements).
-   **offsetAB**: starting index for `AB` (complex elements).
-   **BB**: [`Complex128Array`][@stdlib/array/complex128] band matrix B in band storage.
-   **strideBB1**: stride of the first dimension of `BB` (complex elements).
-   **strideBB2**: stride of the second dimension of `BB` (complex elements).
-   **offsetBB**: starting index for `BB` (complex elements).
-   **Q**: [`Complex128Array`][@stdlib/array/complex128] output transformation matrix (N-by-N).
-   **strideQ1**: stride of the first dimension of `Q` (complex elements).
-   **strideQ2**: stride of the second dimension of `Q` (complex elements).
-   **offsetQ**: starting index for `Q` (complex elements).
-   **vl**: lower bound of eigenvalue interval (range='value').
-   **vu**: upper bound of eigenvalue interval (range='value').
-   **il**: index of smallest eigenvalue to compute (1-based, range='index').
-   **iu**: index of largest eigenvalue to compute (1-based, range='index').
-   **abstol**: absolute tolerance for eigenvalues.
-   **out**: output object; `out.M` will be set to the number of eigenvalues found.
-   **w**: [`Float64Array`][@stdlib/array/float64] output array for eigenvalues (length N).
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: [`Complex128Array`][@stdlib/array/complex128] output eigenvector matrix (N-by-M).
-   **strideZ1**: stride of the first dimension of `Z` (complex elements).
-   **strideZ2**: stride of the second dimension of `Z` (complex elements).
-   **offsetZ**: starting index for `Z` (complex elements).
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] complex workspace (length >= N).
-   **strideWORK**: stride length for `WORK` (complex elements).
-   **offsetWORK**: starting index for `WORK` (complex elements).
-   **RWORK**: [`Float64Array`][@stdlib/array/float64] real workspace (length >= 7\*N).
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.
-   **IWORK**: [`Int32Array`][@stdlib/array/int32] integer workspace (length >= 5\*N).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **IFAIL**: [`Int32Array`][@stdlib/array/int32] output failure indices (length N).
-   **strideIFAIL**: stride length for `IFAIL`.
-   **offsetIFAIL**: starting index for `IFAIL`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Workspace arrays must be pre-allocated: `WORK` (Complex128Array, length >= N), `RWORK` (Float64Array, length >= 7\*N), `IWORK` (Int32Array, length >= 5\*N), `IFAIL` (Int32Array, length N).
-   The number of eigenvalues found is returned via `out.M`.
-   Eigenvalues are returned in ascending order in `W` (Float64Array).
-   If `jobz` is `'compute-vectors'`, eigenvectors are stored column-wise in `Z` (Complex128Array).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zhbgvx = require( '@stdlib/lapack/base/zhbgvx' );

var AB = new Complex128Array( [ 0, 0, 0, 0, 10, 0, 0, 0, 1, 0.5, 8, 0, 0.5, 0.1, 2, -0.3, 6, 0, 0.3, -0.2, 1.5, 0.2, 9, 0, 0.4, 0.15, 1, -0.4, 7, 0 ] );
var BB = new Complex128Array( [ 0, 0, 4, 0, 0.2, 0.1, 5, 0, 0.3, -0.1, 3, 0, 0.1, 0.05, 6, 0, 0.2, -0.1, 4, 0 ] );
var Q = new Complex128Array( 25 );
var W = new Float64Array( 5 );
var Z = new Complex128Array( 25 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 5 );
var out = { M: 0 };

zhbgvx.ndarray( 'compute-vectors', 'all', 'upper', 5, 2, 1, AB, 1, 3, 0, BB, 1, 2, 0, Q, 1, 5, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 5, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
console.log( 'Eigenvalues found:', out.M );
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

[@stdlib/array/float64]: https://github.com/stdlib-js/array-float64
[@stdlib/array/int32]: https://github.com/stdlib-js/array-int32
[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
