# zlansb

> Returns the norm of a complex symmetric band matrix.

<section class="usage">

## Usage

```javascript
var zlansb = require( '@stdlib/lapack/base/zlansb' );
```

#### zlansb.ndarray( norm, uplo, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK )

Returns the norm of a complex symmetric band matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// Upper complex symmetric 4x4 band matrix with K=1 (bandwidth 1):
// Full symmetric matrix (A(i,j) = A(j,i), NO conjugation):
//   [ (2+1i)    (-3+1i)   0         0       ]
//   [ (-3+1i)   (4-2i)    (1+2i)    0       ]
//   [ 0         (1+2i)    (-5+1i)   (6-3i)  ]
//   [ 0         0         (6-3i)    (7+2i)  ]
//
// Band storage (column-major, LDAB=2):
var AB = new Complex128Array( [ 0, 0, 2, 1, -3, 1, 4, -2, 1, 2, -5, 1, 6, -3, 7, 2 ] );
var WORK = new Float64Array( 4 );

var result = zlansb.ndarray( 'one-norm', 'upper', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
// returns ~14.043
```

The function has the following parameters:

-   **norm**: norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the upper or lower triangular part of the symmetric band matrix is stored: `'upper'` or `'lower'`.
-   **N**: order of the matrix.
-   **K**: number of super-diagonals (upper) or sub-diagonals (lower).
-   **AB**: band matrix in band storage format as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **WORK**: workspace array (length >= N for `'one-norm'` or `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Band storage format stores the diagonals of the matrix in rows of a 2D array with (K+1) rows and N columns. For upper storage, the diagonal is in the last row (row K), and for lower storage, the diagonal is in the first row (row 0).
-   For symmetric matrices, the one-norm and infinity-norm are equal.
-   All elements (including the diagonal) are fully complex. This differs from the Hermitian variant (`zlanhb`) where the diagonal is real.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansb = require( '@stdlib/lapack/base/zlansb' );

// Lower complex symmetric 4x4 band matrix with K=1:
var AB = new Complex128Array( [ 2, 1, -3, 1, 4, -2, 1, 2, -5, 1, 6, -3, 7, 2, 0, 0 ] );
var WORK = new Float64Array( 4 );

var result = zlansb.ndarray( 'frobenius', 'lower', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
console.log( result );
// => ~14.967
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
