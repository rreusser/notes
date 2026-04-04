# dsbevx

> Computes selected eigenvalues and optionally eigenvectors of a real symmetric band matrix.

<section class="usage">

## Usage

```javascript
var dsbevx = require( '@stdlib/lapack/base/dsbevx' );
```

#### dsbevx.ndarray( jobz, range, uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, Q, strideQ1, strideQ2, offsetQ, vl, vu, il, iu, abstol, out, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and optionally eigenvectors of a real symmetric band matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 4x4 tridiagonal (KD=1), lower band storage:
var AB = new Float64Array( [ 4, 1, 5, 2, 6, 3, 7, 0 ] );
var Q = new Float64Array( 16 );
var W = new Float64Array( 4 );
var Z = new Float64Array( 16 );
var WORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 4 );
var out = { M: 0 };

var info = dsbevx.ndarray( 'compute-vectors', 'all', 'lower', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// returns 0

// out.M => 4
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues and eigenvectors).
-   **range**: `'all'` (all eigenvalues), `'value'` (eigenvalues in interval [vl, vu)), or `'index'` (eigenvalues il through iu).
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of the band matrix is stored.
-   **N**: order of the matrix.
-   **kd**: number of super- (upper) or sub- (lower) diagonals.
-   **AB**: band matrix in band storage (LDAB-by-N).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **Q**: output orthogonal transformation matrix (N-by-N).
-   **strideQ1**: stride of the first dimension of `Q`.
-   **strideQ2**: stride of the second dimension of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **vl**: lower bound of eigenvalue interval (used when range='value').
-   **vu**: upper bound of eigenvalue interval (used when range='value').
-   **il**: index of smallest eigenvalue to compute, 1-based (used when range='index').
-   **iu**: index of largest eigenvalue to compute, 1-based (used when range='index').
-   **abstol**: absolute tolerance for eigenvalues.
-   **out**: output object; `out.M` is set to the number of eigenvalues found.
-   **w**: output array for eigenvalues (length N).
-   **strideW**: stride for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: output eigenvector matrix (N-by-M).
-   **strideZ1**: stride of first dimension of `Z`.
-   **strideZ2**: stride of second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: workspace (length >= 7\*N).
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace (length >= 5\*N).
-   **strideIWORK**: stride for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **IFAIL**: output array for non-converged eigenvector indices (length N).
-   **strideIFAIL**: stride for `IFAIL`.
-   **offsetIFAIL**: starting index for `IFAIL`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine reduces the band matrix to tridiagonal form, then computes eigenvalues (and optionally eigenvectors) using bisection and inverse iteration.
-   When `range` is `'all'` and `abstol <= 0`, a fast path using `dsterf`/`dsteqr` is used.
-   The `out.M` field indicates how many eigenvalues were found.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsbevx = require( '@stdlib/lapack/base/dsbevx' );

var AB = new Float64Array( [ 4, 1, 5, 2, 6, 3, 7, 0 ] );
var Q = new Float64Array( 16 );
var W = new Float64Array( 4 );
var Z = new Float64Array( 16 );
var WORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 4 );
var out = { M: 0 };

var info = dsbevx.ndarray( 'compute-vectors', 'all', 'lower', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
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
