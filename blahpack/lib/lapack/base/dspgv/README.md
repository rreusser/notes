# dspgv

> Computes all eigenvalues and optionally eigenvectors of a real symmetric-definite generalized eigenproblem in packed storage.

<section class="usage">

## Usage

```javascript
var dspgv = require( '@stdlib/lapack/base/dspgv' );
```

#### dspgv( order, itype, jobz, uplo, N, AP, BP, w, Z, LDZ, WORK )

Computes all eigenvalues and, optionally, eigenvectors of a real generalized symmetric-definite eigenproblem in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// A = [4 2 1; 2 5 3; 1 3 6] in upper packed storage:
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );

// B = [4 2 0; 2 5 1; 0 1 3] in upper packed storage:
var BP = new Float64Array( [ 4.0, 2.0, 5.0, 0.0, 1.0, 3.0 ] );

var W = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dspgv( 'column-major', 1, 'compute-vectors', 'upper', 3, AP, BP, W, Z, 3, WORK );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **itype**: problem type (1, 2, or 3).
-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues and eigenvectors).
-   **uplo**: `'upper'` or `'lower'`, specifying which triangle of A and B is stored.
-   **N**: order of matrices A and B.
-   **AP**: packed symmetric matrix A (length `N*(N+1)/2`); on exit, overwritten.
-   **BP**: packed symmetric positive definite matrix B (length `N*(N+1)/2`); on exit, Cholesky factor.
-   **w**: output array for eigenvalues (length `N`), in ascending order.
-   **Z**: output eigenvector matrix (`N x N`); referenced only if jobz is `'compute-vectors'`.
-   **LDZ**: leading dimension of `Z`.
-   **WORK**: workspace array (length >= `3*N`).

#### dspgv.ndarray( itype, jobz, uplo, N, AP, strideAP, offsetAP, BP, strideBP, offsetBP, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK )

Computes all eigenvalues and, optionally, eigenvectors with alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var BP = new Float64Array( [ 4.0, 2.0, 5.0, 0.0, 1.0, 3.0 ] );
var W = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dspgv.ndarray( 1, 'compute-vectors', 'upper', 3, AP, 1, 0, BP, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideBP**: stride length for `BP`.
-   **offsetBP**: starting index for `BP`.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dspgv` solves `A*x = lambda*B*x` (itype=1), `A*B*x = lambda*x` (itype=2), or `B*A*x = lambda*x` (itype=3).
-   Both A and B must be symmetric. B must also be positive definite.
-   On exit, AP is destroyed and BP contains the Cholesky factor of B.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dspgv = require( '@stdlib/lapack/base/dspgv' );

// A = [4 2 1; 2 5 3; 1 3 6] in upper packed storage:
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );

// B = [4 2 0; 2 5 1; 0 1 3] in upper packed storage:
var BP = new Float64Array( [ 4.0, 2.0, 5.0, 0.0, 1.0, 3.0 ] );

var W = new Float64Array( 3 );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 9 );

var info = dspgv( 'column-major', 1, 'compute-vectors', 'upper', 3, AP, BP, W, Z, 3, WORK );

console.log( 'info:', info );
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

</section>

<!-- /.links -->
