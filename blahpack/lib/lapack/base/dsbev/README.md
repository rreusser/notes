# dsbev

> Computes all eigenvalues and optionally eigenvectors of a real symmetric band matrix.

<section class="usage">

## Usage

```javascript
var dsbev = require( '@stdlib/lapack/base/dsbev' );
```

#### dsbev( order, jobz, uplo, N, kd, AB, LDAB, w, strideW, Z, LDZ, WORK, strideWORK )

Computes all eigenvalues and, optionally, eigenvectors of a real symmetric band matrix stored in LAPACK-style band storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 4x4 tridiagonal (KD=1), lower band storage, column-major:
var AB = new Float64Array( [ 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0, 0.0 ] );
var W = new Float64Array( 4 );
var Z = new Float64Array( 16 );
var WORK = new Float64Array( 10 );

var info = dsbev( 'column-major', 'compute-vectors', 'lower', 4, 1, AB, 2, W, 1, Z, 4, WORK, 1 );
// info => 0
// W contains eigenvalues in ascending order
// Z contains orthonormal eigenvectors
```

#### dsbev.ndarray( jobz, uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK )

Computes all eigenvalues and, optionally, eigenvectors of a real symmetric band matrix using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 4x4 tridiagonal (KD=1), upper band storage:
var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
var W = new Float64Array( 4 );
var Z = new Float64Array( 16 );
var WORK = new Float64Array( 10 );

var info = dsbev.ndarray( 'compute-vectors', 'upper', 4, 1, AB, 1, 2, 0, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` to compute eigenvalues only, or `'compute-vectors'` to also compute eigenvectors.
-   **uplo**: `'upper'` if the upper triangle of `A` is stored in `AB`, or `'lower'` if the lower triangle is stored.
-   **N**: order of the symmetric band matrix `A` (N >= 0).
-   **kd**: number of superdiagonals (if `uplo` is `'upper'`) or subdiagonals (if `uplo` is `'lower'`) of `A` (kd >= 0).
-   **AB**: band matrix in LAPACK band storage format, dimension `(kd+1, N)`.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **w**: output array for eigenvalues in ascending order (length `N`).
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: output matrix for eigenvectors (dimension `N`-by-`N`), referenced only when `jobz` is `'compute-vectors'`.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: workspace array (length >= max(1, 3\*N-2)).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `AB` is overwritten during the computation. The band matrix is reduced to tridiagonal form by `dsbtrd`.
-   When `jobz` is `'compute-vectors'`, the eigenvectors are stored column-wise in `Z`. The `i`-th column of `Z` holds the eigenvector associated with `W[i]`.
-   The function returns an integer `info`: 0 for success, or a positive value if the tridiagonal eigensolver did not converge.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dsbev = require( '@stdlib/lapack/base/dsbev' );

// 4x4 symmetric tridiagonal matrix (KD=1), lower band:
//   4  1  0  0
//   1  5  2  0
//   0  2  6  3
//   0  0  3  7
var AB = new Float64Array( [ 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0, 0.0 ] );
var W = new Float64Array( 4 );
var Z = new Float64Array( 16 );
var WORK = new Float64Array( 10 );

var info = dsbev( 'column-major', 'compute-vectors', 'lower', 4, 1, AB, 2, W, 1, Z, 4, WORK, 1 );
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
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
