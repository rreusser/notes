# dspevx

> Computes selected eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.

<section class="usage">

## Usage

```javascript
var dspevx = require( '@stdlib/lapack/base/dspevx' );
```

#### dspevx.ndarray( jobz, range, uplo, N, AP, strideAP, offsetAP, vl, vu, il, iu, abstol, out, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var w = new Float64Array( 2 );
var Z = new Float64Array( 4 );
var WORK = new Float64Array( 16 );
var IWORK = new Int32Array( 10 );
var IFAIL = new Int32Array( 2 );
var out = { 'M': 0 };

dspevx.ndarray( 'compute-vectors', 'all', 'upper', 2, AP, 1, 0, 0, 0, 0, 0, 0, out, w, 1, 0, Z, 1, 2, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// out.M => 2
// w => [ ~-0.236, ~4.236 ]
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues + eigenvectors).
-   **range**: `'all'`, `'value'`, or `'index'`.
-   **uplo**: `'upper'` or `'lower'`.
-   **N**: order of the matrix A.
-   **AP**: packed symmetric matrix (length N\*(N+1)/2).
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **vl**: lower bound of eigenvalue interval (RANGE='value').
-   **vu**: upper bound of eigenvalue interval (RANGE='value').
-   **il**: index of smallest eigenvalue to compute (1-based, RANGE='index').
-   **iu**: index of largest eigenvalue to compute (1-based, RANGE='index').
-   **abstol**: absolute tolerance for eigenvalues.
-   **out**: output object; `out.M` will be set to number of eigenvalues found.
-   **w**: output array for eigenvalues (length N).
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: output eigenvector matrix (N x M).
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: workspace array (length >= 8\*N).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace array (length >= 5\*N).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **IFAIL**: output integer array for non-converged eigenvector indices (length N).
-   **strideIFAIL**: stride length for `IFAIL`.
-   **offsetIFAIL**: starting index for `IFAIL`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dspevx` is the packed-storage analog of `dsyevx`. It operates on symmetric matrices stored in packed form (upper or lower triangle packed column-wise into a linear array), rather than full two-dimensional storage.
-   The workspace WORK needs at least 8\*N elements and IWORK needs at least 5\*N elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dspevx = require( '@stdlib/lapack/base/dspevx' );

// 3x3 symmetric [[2,1,0],[1,3,1],[0,1,4]] upper packed: [2, 1, 3, 0, 1, 4]
var AP = new Float64Array( [ 2.0, 1.0, 3.0, 0.0, 1.0, 4.0 ] );
var w = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 24 );
var IWORK = new Int32Array( 15 );
var IFAIL = new Int32Array( 3 );
var out = { 'M': 0 };

dspevx.ndarray( 'compute-vectors', 'all', 'upper', 3, AP, 1, 0, 0, 0, 0, 0, 0, out, w, 1, 0, Z, 1, 3, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );

console.log( 'Number of eigenvalues found:', out.M );
console.log( 'Eigenvalues:', w );
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
