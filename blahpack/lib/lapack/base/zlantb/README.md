# zlantb

> Returns the norm of a complex triangular band matrix.

<section class="usage">

## Usage

```javascript
var zlantb = require( '@stdlib/lapack/base/zlantb' );
```

#### zlantb.ndarray( norm, uplo, diag, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK )

Returns the norm of a complex triangular band matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 upper triangular band matrix with K=1 superdiagonal:
// Full matrix:
//   [ (1+1i)  (2+2i)    0   ]
//   [    0    (3+3i)  (4+4i) ]
//   [    0       0    (5+5i) ]
// Band storage (LDAB=2):
//   Row 0:  0      (2+2i)  (4+4i)
//   Row 1: (1+1i)  (3+3i)  (5+5i)
var AB = new Complex128Array( [ 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5 ] );
var WORK = new Float64Array( 3 );

var v = zlantb.ndarray( 'one-norm', 'upper', 'non-unit', 3, 1, AB, 1, 2, 0, WORK, 1, 0 );
// returns ~12.73
```

The function has the following parameters:

-   **norm**: norm type (`'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`).
-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix has a unit diagonal (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **K**: number of super-diagonals (if upper) or sub-diagonals (if lower).
-   **AB**: band matrix in band storage as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **WORK**: workspace array (length >= N for `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Band storage format: for upper triangular, the diagonal is at band row K (0-indexed); for lower triangular, the diagonal is at band row 0.
-   WORK is only referenced when `norm` is `'inf-norm'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantb = require( '@stdlib/lapack/base/zlantb' );

// 3x3 upper triangular band matrix with K=1 superdiagonal
var AB = new Complex128Array( [ 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5 ] );
var WORK = new Float64Array( 3 );

var v = zlantb.ndarray( 'frobenius', 'upper', 'non-unit', 3, 1, AB, 1, 2, 0, WORK, 1, 0 );
console.log( v );
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
