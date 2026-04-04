# zhbevx

> Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.

<section class="usage">

## Usage

```javascript
var zhbevx = require( '@stdlib/lapack/base/zhbevx' );
```

#### zhbevx.ndarray( jobz, range, uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, Q, strideQ1, strideQ2, offsetQ, vl, vu, il, iu, abstol, out, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );

// 4x4 Hermitian tridiagonal (KD=1), lower band storage:
var AB = new Complex128Array( [ 4, 0, 1, -1, 5, 0, 2, 1, 6, 0, 3, -1, 7, 0, 0, 0 ] );
var Q = new Complex128Array( 16 );
var W = new Float64Array( 4 );
var Z = new Complex128Array( 16 );
var WORK = new Complex128Array( 20 );
var RWORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 4 );
var out = { M: 0 };

var info = zhbevx.ndarray( 'compute-vectors', 'all', 'lower', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// returns 0

// out.M => 4
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues and eigenvectors).
-   **range**: `'all'` (all eigenvalues), `'value'` (eigenvalues in interval [vl, vu)), or `'index'` (eigenvalues il through iu).
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of the band matrix is stored.
-   **N**: order of the matrix.
-   **kd**: number of super- (upper) or sub- (lower) diagonals.
-   **AB**: complex band matrix in band storage (Complex128Array, LDAB-by-N).
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **Q**: output unitary transformation matrix (Complex128Array, N-by-N).
-   **strideQ1**: stride of the first dimension of `Q` (in complex elements).
-   **strideQ2**: stride of the second dimension of `Q` (in complex elements).
-   **offsetQ**: starting index for `Q` (in complex elements).
-   **vl**: lower bound of eigenvalue interval (used when range='value').
-   **vu**: upper bound of eigenvalue interval (used when range='value').
-   **il**: index of smallest eigenvalue to compute, 1-based (used when range='index').
-   **iu**: index of largest eigenvalue to compute, 1-based (used when range='index').
-   **abstol**: absolute tolerance for eigenvalues.
-   **out**: output object; `out.M` is set to the number of eigenvalues found.
-   **w**: output array for eigenvalues (Float64Array, length N), in ascending order.
-   **strideW**: stride for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: output eigenvector matrix (Complex128Array, N-by-M).
-   **strideZ1**: stride of first dimension of `Z` (in complex elements).
-   **strideZ2**: stride of second dimension of `Z` (in complex elements).
-   **offsetZ**: starting index for `Z` (in complex elements).
-   **WORK**: complex workspace (Complex128Array, length >= N).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace (Float64Array, length >= 7\*N).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.
-   **IWORK**: integer workspace (Int32Array, length >= 5\*N).
-   **strideIWORK**: stride for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **IFAIL**: output array for non-converged eigenvector indices (Int32Array, length N).
-   **strideIFAIL**: stride for `IFAIL`.
-   **offsetIFAIL**: starting index for `IFAIL`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine reduces the complex Hermitian band matrix to real tridiagonal form via `zhbtrd`, then computes eigenvalues (and optionally eigenvectors) using bisection (`dstebz`) and inverse iteration (`zstein`).
-   When `range` is `'all'` and `abstol <= 0`, a fast path using `dsterf`/`zsteqr` is used.
-   The `out.M` field indicates how many eigenvalues were found.
-   Eigenvalues are real (stored in Float64Array `w`), while eigenvectors are complex (stored in Complex128Array `Z`).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zhbevx = require( '@stdlib/lapack/base/zhbevx' );

var AB = new Complex128Array( [ 4, 0, 1, -1, 5, 0, 2, 1, 6, 0, 3, -1, 7, 0, 0, 0 ] );
var Q = new Complex128Array( 16 );
var W = new Float64Array( 4 );
var Z = new Complex128Array( 16 );
var WORK = new Complex128Array( 20 );
var RWORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 4 );
var out = { M: 0 };

var info = zhbevx.ndarray( 'compute-vectors', 'all', 'lower', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
console.log( 'info:', info );
console.log( 'M:', out.M );
console.log( 'W:', W );
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
