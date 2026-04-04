# ztbcon

> Estimates the reciprocal condition number of a complex triangular band matrix.

<section class="usage">

## Usage

```javascript
var ztbcon = require( '@stdlib/lapack/base/ztbcon' );
```

#### ztbcon.ndarray( norm, uplo, diag, N, kd, AB, strideAB1, strideAB2, offsetAB, RCOND, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Estimates the reciprocal of the condition number of a complex triangular band matrix A, in either the 1-norm or the infinity-norm.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 identity band matrix (KD=0):
var AB = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
var RCOND = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztbcon.ndarray( 'one-norm', 'upper', 'non-unit', 3, 0, AB, 1, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// RCOND[ 0 ] => 1.0
```

The function has the following parameters:

-   **norm**: norm type: `'one-norm'` or `'inf-norm'`.
-   **uplo**: specifies whether the matrix is upper or lower triangular: `'upper'` or `'lower'`.
-   **diag**: specifies whether the matrix has unit diagonal: `'unit'` or `'non-unit'`.
-   **N**: order of the matrix.
-   **kd**: number of super-diagonals (if upper) or sub-diagonals (if lower).
-   **AB**: triangular band matrix in band storage as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideAB1**: stride of the first dimension of `AB` (complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (complex elements).
-   **offsetAB**: starting index for `AB` (complex elements).
-   **RCOND**: output [`Float64Array`][mdn-float64array] of length 1 receiving the reciprocal condition number.
-   **WORK**: workspace [`Complex128Array`][@stdlib/array/complex128] of length `2*N`.
-   **strideWORK**: stride for `WORK` (complex elements).
-   **offsetWORK**: starting index for `WORK` (complex elements).
-   **RWORK**: workspace [`Float64Array`][mdn-float64array] of length `N`.
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The reciprocal condition number is computed as `RCOND = 1 / ( norm(A) * norm(inv(A)) )`.
-   Uses reverse communication with `zlacn2` to estimate `norm(inv(A))`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ztbcon = require( '@stdlib/lapack/base/ztbcon' );

// 3x3 identity band matrix (KD=0):
var AB = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
var RCOND = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztbcon.ndarray( 'one-norm', 'upper', 'non-unit', 3, 0, AB, 1, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info, 'rcond:', RCOND[ 0 ] );
// => info: 0 rcond: 1
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
