# dtpcon

> Estimates the reciprocal of the condition number of a real triangular matrix in packed storage, in either the 1-norm or the infinity-norm.

<section class="usage">

## Usage

```javascript
var dtpcon = require( '@stdlib/lapack/base/dtpcon' );
```

#### dtpcon( norm, uplo, diag, N, AP, rcond, WORK, IWORK )

Estimates the reciprocal condition number of a real triangular matrix stored in packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 upper triangular identity in packed storage:
var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtpcon( 'one-norm', 'upper', 'non-unit', 3, AP, rcond, WORK, IWORK );
// info => 0
// rcond[ 0 ] => 1.0
```

The function has the following parameters:

-   **norm**: norm type (`'one-norm'` or `'inf-norm'`).
-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix has unit diagonal (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **AP**: packed triangular matrix of length `N*(N+1)/2`.
-   **rcond**: output [`Float64Array`][mdn-float64array] of length 1 receiving the reciprocal condition number.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length at least `3*N`.
-   **IWORK**: workspace [`Int32Array`][mdn-int32array] of length at least `N`.

#### dtpcon.ndarray( norm, uplo, diag, N, AP, strideAP, offsetAP, rcond, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Estimates the reciprocal condition number with alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtpcon.ndarray( 'one-norm', 'upper', 'non-unit', 3, AP, 1, 0, rcond, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
// rcond[ 0 ] => 1.0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The reciprocal condition number is computed as `rcond = 1 / ( norm(A) * norm(inv(A)) )`. A small value indicates a nearly singular matrix.
-   The packed storage format stores the upper or lower triangle column-by-column in a one-dimensional array of length `N*(N+1)/2`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtpcon = require( '@stdlib/lapack/base/dtpcon' );

// Upper triangular 3x3 identity matrix in packed storage:
var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtpcon( 'one-norm', 'upper', 'non-unit', 3, AP, rcond, WORK, IWORK );
console.log( 'info:', info );
console.log( 'rcond:', rcond[ 0 ] );
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
