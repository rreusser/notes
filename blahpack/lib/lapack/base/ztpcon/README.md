# ztpcon

> Estimates the reciprocal condition number of a complex triangular matrix in packed storage.

<section class="usage">

## Usage

```javascript
var ztpcon = require( '@stdlib/lapack/base/ztpcon' );
```

#### ztpcon( norm, uplo, diag, N, AP, RCOND, WORK, RWORK )

Estimates the reciprocal condition number of a complex triangular matrix in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 upper triangular, non-unit, packed column-major:
var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 1.0, 3.0, 0.0, 0.5, 0.0, 1.0, -1.0, 2.0, 1.0 ] );
var RCOND = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztpcon( 'one-norm', 'upper', 'non-unit', 3, AP, RCOND, WORK, RWORK );
// info => 0
// RCOND[ 0 ] => ~0.335
```

The function has the following parameters:

-   **norm**: norm type (`'one-norm'` or `'inf-norm'`).
-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix has a unit diagonal (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **AP**: packed triangular matrix as a [`Complex128Array`][@stdlib/array/complex128].
-   **RCOND**: output [`Float64Array`][mdn-float64array] of length 1 to receive the reciprocal condition number.
-   **WORK**: workspace [`Complex128Array`][@stdlib/array/complex128] of length `2*N`.
-   **RWORK**: workspace [`Float64Array`][mdn-float64array] of length `N`.

#### ztpcon.ndarray( norm, uplo, diag, N, AP, strideAP, offsetAP, RCOND, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Estimates the reciprocal condition number with explicit stride and offset control.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 1.0, 3.0, 0.0, 0.5, 0.0, 1.0, -1.0, 2.0, 1.0 ] );
var RCOND = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztpcon.ndarray( 'one-norm', 'upper', 'non-unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// RCOND[ 0 ] => ~0.335
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine computes `norm(A)` via [`zlantp`][@stdlib/lapack/base/zlantp] and estimates `norm(inv(A))` using reverse communication with [`zlacn2`][@stdlib/lapack/base/zlacn2] and triangular solves via [`zlatps`][@stdlib/lapack/base/zlatps].
-   The reciprocal condition number is returned in `RCOND[0]` as `1 / (norm(A) * norm(inv(A)))`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ztpcon = require( '@stdlib/lapack/base/ztpcon' );

// 3x3 upper triangular matrix in packed storage:
var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 1.0, 3.0, 0.0, 0.5, 0.0, 1.0, -1.0, 2.0, 1.0 ] );
var RCOND = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztpcon( 'one-norm', 'upper', 'non-unit', 3, AP, RCOND, WORK, RWORK );
console.log( 'info:', info );
console.log( 'rcond:', RCOND[ 0 ] );
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

[@stdlib/lapack/base/zlantp]: https://github.com/stdlib-js/lapack-base-zlantp

[@stdlib/lapack/base/zlacn2]: https://github.com/stdlib-js/lapack-base-zlacn2

[@stdlib/lapack/base/zlatps]: https://github.com/stdlib-js/lapack-base-zlatps

</section>

<!-- /.links -->
