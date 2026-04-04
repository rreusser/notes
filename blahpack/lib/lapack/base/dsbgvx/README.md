# dsbgvx

> Computes selected eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem.

<section class="usage">

## Usage

```javascript
var dsbgvx = require( '@stdlib/lapack/base/dsbgvx' );
```

#### dsbgvx.ndarray( jobz, range, uplo, N, ka, kb, AB, strideAB1, strideAB2, offsetAB, BB, strideBB1, strideBB2, offsetBB, Q, strideQ1, strideQ2, offsetQ, vl, vu, il, iu, abstol, out, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and optionally eigenvectors of a real generalized symmetric-definite banded eigenproblem A\_x = lambda\_B\_x.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 5x5 symmetric band matrix A (KA=2), upper band storage:
var AB = new Float64Array( [ 0, 0, 10, 0, 1, 8, 0.5, 2, 6, 0.3, 1.5, 9, 0.4, 1, 7 ] );
// 5x5 positive definite band matrix B (KB=1), upper storage:
var BB = new Float64Array( [ 0, 4, 0.2, 5, 0.3, 3, 0.1, 6, 0.2, 4 ] );
var Q = new Float64Array( 25 );
var W = new Float64Array( 5 );
var Z = new Float64Array( 25 );
var WORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 5 );
var out = { M: 0 };

var info = dsbgvx.ndarray( 'compute-vectors', 'all', 'upper', 5, 2, 1, AB, 1, 3, 0, BB, 1, 2, 0, Q, 1, 5, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 5, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// returns 0

// out.M => 5
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues and eigenvectors).
-   **range**: `'all'` (all eigenvalues), `'value'` (eigenvalues in interval (vl, vu]), or `'index'` (eigenvalues il through iu).
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of the band matrices is stored.
-   **N**: order of the matrices A and B.
-   **ka**: number of super- (upper) or sub- (lower) diagonals of A.
-   **kb**: number of super- (upper) or sub- (lower) diagonals of B.
-   **AB**: band matrix A in band storage ((ka+1)-by-N).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **BB**: band matrix B in band storage ((kb+1)-by-N).
-   **strideBB1**: stride of the first dimension of `BB`.
-   **strideBB2**: stride of the second dimension of `BB`.
-   **offsetBB**: starting index for `BB`.
-   **Q**: output transformation matrix (N-by-N).
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

-   The routine first factorizes B via `dpbstf`, then transforms to standard form via `dsbgst`, reduces to tridiagonal form via `dsbtrd`, and computes eigenvalues using bisection and inverse iteration.
-   When `range` is `'all'` and `abstol <= 0`, a fast path using `dsterf`/`dsteqr` is used.
-   The `out.M` field indicates how many eigenvalues were found.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsbgvx = require( '@stdlib/lapack/base/dsbgvx' );

var AB = new Float64Array( [ 0, 0, 10, 0, 1, 8, 0.5, 2, 6, 0.3, 1.5, 9, 0.4, 1, 7 ] );
var BB = new Float64Array( [ 0, 4, 0.2, 5, 0.3, 3, 0.1, 6, 0.2, 4 ] );
var Q = new Float64Array( 25 );
var W = new Float64Array( 5 );
var Z = new Float64Array( 25 );
var WORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 5 );
var out = { M: 0 };

var info = dsbgvx.ndarray( 'compute-vectors', 'all', 'upper', 5, 2, 1, AB, 1, 3, 0, BB, 1, 2, 0, Q, 1, 5, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 5, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
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
