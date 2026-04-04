# dsbgv

> Computes all eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem A\*x = lambda\*B\*x.

<section class="usage">

## Usage

```javascript
var dsbgv = require( '@stdlib/lapack/base/dsbgv' );
```

#### dsbgv( order, jobz, uplo, N, ka, kb, AB, LDAB, BB, LDBB, w, strideW, Z, LDZ, WORK, strideWORK )

Computes all eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 diagonal matrices (KA=KB=0), column-major:
var AB = new Float64Array( [ 5.0, 6.0, 7.0 ] );
var BB = new Float64Array( [ 2.0, 3.0, 4.0 ] );
var W = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dsbgv( 'column-major', 'compute-vectors', 'upper', 3, 0, 0, AB, 1, BB, 1, W, 1, Z, 3, WORK, 1 );
// info => 0
// W => [ 1.75, 2.0, 2.5 ]
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **jobz**: `'no-vectors'` to compute eigenvalues only, `'compute-vectors'` to also compute eigenvectors.
-   **uplo**: `'upper'` or `'lower'` triangles of A and B are stored.
-   **N**: order of matrices A and B.
-   **ka**: number of super- (or sub-) diagonals of A.
-   **kb**: number of super- (or sub-) diagonals of B.
-   **AB**: band matrix A in band storage, dimension (LDAB, N).
-   **LDAB**: leading dimension of `AB` (>= ka+1).
-   **BB**: band matrix B in band storage, dimension (LDBB, N).
-   **LDBB**: leading dimension of `BB` (>= kb+1).
-   **w**: output array for eigenvalues (length N).
-   **strideW**: stride for `w`.
-   **Z**: output matrix for eigenvectors (LDZ, N), referenced only if jobz = `'compute-vectors'`.
-   **LDZ**: leading dimension of `Z`.
-   **WORK**: workspace array (length >= 3\*N).
-   **strideWORK**: stride for `WORK`.

#### dsbgv.ndarray( jobz, uplo, N, ka, kb, AB, strideAB1, strideAB2, offsetAB, BB, strideBB1, strideBB2, offsetBB, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK )

Computes all eigenvalues and optionally eigenvectors using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Float64Array( [ 5.0, 6.0, 7.0 ] );
var BB = new Float64Array( [ 2.0, 3.0, 4.0 ] );
var W = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dsbgv.ndarray( 'compute-vectors', 'upper', 3, 0, 0, AB, 1, 1, 0, BB, 1, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
// info => 0
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   A and B are assumed to be symmetric and banded. B must also be positive definite.
-   On exit, AB is destroyed. BB contains the split Cholesky factor from `dpbstf`.
-   If `info > 0` and `info <= N`, dsteqr/dsterf did not converge. If `info > N`, B is not positive definite (`info - N` is the return value from `dpbstf`).
-   The eigenvalues are returned in ascending order in `w`.
-   If jobz = `'compute-vectors'`, the eigenvectors are normalized so that Z^T\*B\*Z = I.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dsbgv = require( '@stdlib/lapack/base/dsbgv' );

// 3x3 diagonal matrices (KA=KB=0), column-major:
var AB = new Float64Array( [ 5.0, 6.0, 7.0 ] );
var BB = new Float64Array( [ 2.0, 3.0, 4.0 ] );
var W = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dsbgv( 'column-major', 'compute-vectors', 'upper', 3, 0, 0, AB, 1, BB, 1, W, 1, Z, 3, WORK, 1 );
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

</section>

<!-- /.links -->
