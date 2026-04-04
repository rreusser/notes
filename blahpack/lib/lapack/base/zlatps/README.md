# zlatps

> Solves a complex triangular system with scaling to prevent overflow, where the matrix is in packed storage.

<section class="usage">

## Usage

```javascript
var zlatps = require( '@stdlib/lapack/base/zlatps' );
```

#### zlatps.ndarray( uplo, trans, diag, normin, N, AP, strideAP, offsetAP, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM )

Solves one of the triangular systems `A*x = s*b`, `A^T*x = s*b`, or `A^H*x = s*b` with scaling to prevent overflow, where `A` is an `N`-by-`N` upper or lower triangular matrix stored in packed form.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.5, 3.0, 0.0 ] );
var x = new Complex128Array( [ 4.0, 2.0, 6.0, 0.0 ] );
var scale = new Float64Array( 1 );
var cnorm = new Float64Array( 2 );

var info = zlatps.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', 2, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper (`'upper'`) or lower (`'lower'`) triangular.
-   **trans**: specifies the operation: `'no-transpose'`, `'transpose'`, or `'conjugate-transpose'`.
-   **diag**: specifies whether the matrix is unit (`'unit'`) or non-unit (`'non-unit'`) triangular.
-   **normin**: `'yes'` if column norms are provided in CNORM, `'no'` to compute them.
-   **N**: order of the matrix.
-   **AP**: packed triangular matrix as a `Complex128Array`.
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **x**: right-hand side / solution vector as a `Complex128Array`.
-   **strideX**: stride length for `x` (in complex elements).
-   **offsetX**: starting index for `x` (in complex elements).
-   **scale**: `Float64Array` of length 1; on exit, the scaling factor `s`.
-   **CNORM**: `Float64Array` of length `N`; column norms.
-   **strideCNORM**: stride length for `CNORM`.
-   **offsetCNORM**: starting index for `CNORM`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The matrix `A` is stored in packed form in column-major order.
-   The scaling factor `s` is chosen to prevent overflow in the solution.
-   If `A` is singular, `s` is set to zero and a non-trivial solution to `A*x = 0` is returned.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlatps = require( '@stdlib/lapack/base/zlatps' );

var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.5, 3.0, 0.0 ] );
var x = new Complex128Array( [ 4.0, 2.0, 6.0, 0.0 ] );
var scale = new Float64Array( 1 );
var cnorm = new Float64Array( 2 );

var info = zlatps.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', 2, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
console.log( 'info:', info );
console.log( 'scale:', scale[ 0 ] );
```

</section>

<!-- /.examples -->

<section class="related">

</section>

<!-- /.related -->

<section class="links">

</section>

<!-- /.links -->
