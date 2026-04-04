# dlansb

> Returns the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real symmetric band matrix.

<section class="usage">

## Usage

```javascript
var dlansb = require( '@stdlib/lapack/base/dlansb' );
```

#### dlansb.ndarray( norm, uplo, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK )

Returns the norm of a real symmetric band matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Upper symmetric 4x4 band matrix with K=1 (bandwidth 1):
// Full symmetric matrix:
//   [ 2  -3   0   0 ]
//   [-3   4   1   0 ]
//   [ 0   1  -5   6 ]
//   [ 0   0   6   7 ]
//
// Band storage (column-major, LDAB=2):
var AB = new Float64Array( [ 0, 2, -3, 4, 1, -5, 6, 7 ] );
var WORK = new Float64Array( 4 );

var result = dlansb.ndarray( 'one-norm', 'upper', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
// returns 13.0
```

The function has the following parameters:

-   **norm**: norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the upper or lower triangular part of the symmetric band matrix is stored: `'upper'` or `'lower'`.
-   **N**: order of the matrix.
-   **K**: number of super-diagonals (upper) or sub-diagonals (lower).
-   **AB**: band matrix in band storage format.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **WORK**: workspace array (length >= N for `'one-norm'` or `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Band storage format stores the diagonals of the matrix in rows of a 2D array with (K+1) rows and N columns. For upper storage, the diagonal is in the last row (row K), and for lower storage, the diagonal is in the first row (row 0).
-   For symmetric matrices, the one-norm and infinity-norm are equal.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlansb = require( '@stdlib/lapack/base/dlansb' );

// Lower symmetric 4x4 band matrix with K=1:
var AB = new Float64Array( [ 2, -3, 4, 1, -5, 6, 7, 0 ] );
var WORK = new Float64Array( 4 );

var result = dlansb.ndarray( 'frobenius', 'lower', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
console.log( result );
// => ~13.638
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
