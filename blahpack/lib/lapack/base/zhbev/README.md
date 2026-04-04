# zhbev

> Computes all eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.

<section class="usage">

## Usage

```javascript
var zhbev = require( '@stdlib/lapack/base/zhbev' );
```

#### zhbev( order, jobz, uplo, N, kd, AB, LDAB, w, strideW, Z, LDZ, WORK, strideWORK, RWORK, strideRWORK )

Computes all eigenvalues and, optionally, eigenvectors of a complex Hermitian band matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 4x4 Hermitian tridiagonal (KD=1), upper band storage, column-major:
var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0, 3.0, 1.0, 7.0, 0.0 ] );
var W = new Float64Array( 4 );
var Z = new Complex128Array( 16 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 10 );

zhbev( 'column-major', 'compute-vectors', 'upper', 4, 1, AB, 2, W, 1, Z, 4, WORK, 1, RWORK, 1 );
// W now contains eigenvalues in ascending order
```

#### zhbev.ndarray( jobz, uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Computes all eigenvalues and optionally eigenvectors of a complex Hermitian band matrix using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0, 3.0, 1.0, 7.0, 0.0 ] );
var W = new Float64Array( 4 );
var Z = new Complex128Array( 16 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 10 );

zhbev.ndarray( 'compute-vectors', 'upper', 4, 1, AB, 1, 2, 0, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, RWORK, 1, 0 );
// W now contains eigenvalues in ascending order
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` to compute eigenvalues only, or `'compute-vectors'` to compute eigenvalues and eigenvectors.
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of the band matrix is stored.
-   **N**: order of the matrix A.
-   **kd**: number of super- (upper) or sub-diagonals (lower).
-   **AB**: [`Complex128Array`][@stdlib/array/complex128] band matrix in band storage.
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **w**: [`Float64Array`][mdn-float64array] output array for eigenvalues (length N), returned in ascending order.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: [`Complex128Array`][@stdlib/array/complex128] output matrix for eigenvectors (N-by-N). Only referenced when `jobz` is `'compute-vectors'`.
-   **strideZ1**: stride of the first dimension of `Z` (in complex elements).
-   **strideZ2**: stride of the second dimension of `Z` (in complex elements).
-   **offsetZ**: starting index for `Z` (in complex elements).
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] complex workspace (length >= N).
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: [`Float64Array`][mdn-float64array] real workspace (length >= max(1, 3*N-2)).
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Eigenvalues of a Hermitian matrix are always real, so `w` is a `Float64Array`.
-   The matrix AB is overwritten during computation.
-   WORK is a complex workspace, while RWORK is a real workspace used for the tridiagonal eigenvalue solver.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbev = require( '@stdlib/lapack/base/zhbev' );

// 4x4 Hermitian tridiagonal, upper band, column-major:
var AB = new Complex128Array( [
    0.0, 0.0, 4.0, 0.0,   // col 1
    1.0, 1.0, 5.0, 0.0,   // col 2
    2.0, -1.0, 6.0, 0.0,  // col 3
    3.0, 1.0, 7.0, 0.0    // col 4
] );
var W = new Float64Array( 4 );
var Z = new Complex128Array( 16 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 10 );

var info = zhbev( 'column-major', 'compute-vectors', 'upper', 4, 1, AB, 2, W, 1, Z, 4, WORK, 1, RWORK, 1 );
console.log( 'info:', info );
console.log( 'eigenvalues:', W );
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
[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
