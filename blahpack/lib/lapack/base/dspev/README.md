# dspev

> Computes all eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.

<section class="usage">

## Usage

```javascript
var dspev = require( '@stdlib/lapack/base/dspev' );
```

#### dspev( order, jobz, uplo, N, AP, w, Z, LDZ, WORK )

Computes all eigenvalues and optionally eigenvectors of a real symmetric matrix A stored in packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var w = new Float64Array( 2 );
var Z = new Float64Array( 4 );
var WORK = new Float64Array( 6 );

var info = dspev( 'column-major', 'compute', 'upper', 2, AP, w, Z, 2, WORK );
// returns 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **jobz**: `'none'` to compute eigenvalues only, or `'compute'` to also compute eigenvectors.
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of A is stored in AP.
-   **N**: order of the symmetric matrix A (N >= 0).
-   **AP**: packed symmetric matrix as a [`Float64Array`][mdn-float64array] of length N*(N+1)/2.
-   **w**: output [`Float64Array`][mdn-float64array] of length N for eigenvalues in ascending order.
-   **Z**: output [`Float64Array`][mdn-float64array] of size N*N for eigenvectors (referenced only when jobz = `'compute'`).
-   **LDZ**: leading dimension of Z (must be >= max(1,N) when jobz = `'compute'`).
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length >= 3*N.

#### dspev.ndarray( jobz, uplo, N, AP, strideAP, offsetAP, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK )

Computes all eigenvalues and optionally eigenvectors using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var w = new Float64Array( 2 );
var Z = new Float64Array( 4 );
var WORK = new Float64Array( 6 );

var info = dspev.ndarray( 'compute', 'upper', 2, AP, 1, 0, w, 1, 0, Z, 1, 2, 0, WORK, 1, 0 );
// returns 0
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The packed storage format stores the upper or lower triangle of the symmetric matrix column-wise in a one-dimensional array of length N*(N+1)/2.
-   On exit, AP is overwritten with the tridiagonal reduction data from `dsptrd`.
-   The eigenvalues are returned in ascending order in `w`.
-   If jobz = `'compute'`, the eigenvector matrix Z satisfies `A * Z = Z * diag(w)` and `Z^T * Z = I`.
-   WORK must have length at least 3*N.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dspev = require( '@stdlib/lapack/base/dspev' );

// 3x3 symmetric [[5,1,2],[1,4,1],[2,1,6]] in lower packed storage:
var AP = new Float64Array( [ 5.0, 1.0, 2.0, 4.0, 1.0, 6.0 ] );
var w = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dspev.ndarray( 'compute', 'lower', 3, AP, 1, 0, w, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
// info => 0
// w => [ ~3.31, ~3.64, ~8.05 ] (eigenvalues in ascending order)
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

</section>

<!-- /.links -->
