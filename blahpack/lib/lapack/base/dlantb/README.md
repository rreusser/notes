# dlantb

> Returns the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real triangular band matrix.

<section class="usage">

## Usage

```javascript
var dlantb = require( '@stdlib/lapack/base/dlantb' );
```

#### dlantb.ndarray( norm, uplo, diag, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK )

Returns the norm of a real triangular band matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Upper triangular 4x4 band matrix with K=1 (bandwidth 1):
// Full matrix:
//   [ 2  -3   0   0 ]
//   [ 0   4   1   0 ]
//   [ 0   0  -5   6 ]
//   [ 0   0   0   7 ]
//
// Band storage (column-major, LDAB=2):
var AB = new Float64Array( [ 0, 2, -3, 4, 1, -5, 6, 7 ] );
var WORK = new Float64Array( 4 );

var result = dlantb.ndarray( 'one-norm', 'upper', 'non-unit', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
// returns 13.0
```

The function has the following parameters:

-   **norm**: norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the matrix is upper or lower triangular: `'upper'` or `'lower'`.
-   **diag**: specifies whether the diagonal is unit: `'unit'` or `'non-unit'`.
-   **N**: order of the matrix.
-   **K**: number of super-diagonals (upper) or sub-diagonals (lower).
-   **AB**: band matrix in band storage format.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **WORK**: workspace array (length >= N for `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Band storage format stores the diagonals of the matrix in rows of a 2D array with (K+1) rows and N columns. For an upper triangular band matrix, the diagonal is in the last row (row K), and for a lower triangular band matrix, the diagonal is in the first row (row 0).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlantb = require( '@stdlib/lapack/base/dlantb' );

// Lower triangular 4x4 band matrix with K=1:
var AB = new Float64Array( [ 2, -3, 4, 1, -5, 6, 7, 0 ] );
var WORK = new Float64Array( 4 );

var result = dlantb.ndarray( 'frobenius', 'lower', 'non-unit', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
console.log( result );
// => ~11.832
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
