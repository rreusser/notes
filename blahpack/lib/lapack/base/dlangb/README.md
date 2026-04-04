# dlangb

> Returns the norm of a real general band matrix.

<section class="usage">

## Usage

```javascript
var dlangb = require( '@stdlib/lapack/base/dlangb' );
```

#### dlangb.ndarray( norm, N, KL, KU, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK )

Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of any element of a real general band matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 diagonal matrix (KL=0, KU=0, LDAB=1):
var AB = new Float64Array( [ 3.0, -1.0, 2.0 ] );
var WORK = new Float64Array( 3 );

var v = dlangb.ndarray( 'max', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
// returns 3.0
```

The function has the following parameters:

-   **norm**: norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **N**: order of the matrix.
-   **KL**: number of sub-diagonals.
-   **KU**: number of super-diagonals.
-   **AB**: input band matrix as a [`Float64Array`][mdn-float64array].
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **WORK**: workspace array as a [`Float64Array`][mdn-float64array] (length >= N for `'inf-norm'`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

The band matrix `AB` is stored in LAPACK band format: element `A(i,j)` is stored at `AB(KU+i-j, j)` (0-indexed). The array has `KL+KU+1` band rows and `N` columns.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlangb()` corresponds to the [LAPACK][lapack] routine [`dlangb`][lapack-dlangb].
-   The `WORK` array is only referenced when `norm` is `'inf-norm'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlangb = require( '@stdlib/lapack/base/dlangb' );

// 3x3 tridiagonal matrix (KL=1, KU=1, LDAB=3):
// A = [ 1   2   0 ]
//     [ 3   4   5 ]
//     [ 0   6   7 ]
var AB = new Float64Array([
    0, 1, 3,
    2, 4, 6,
    5, 7, 0
]);
var WORK = new Float64Array( 3 );

var v = dlangb.ndarray( 'one-norm', 3, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
console.log( v );
// => 12.0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[lapack]: https://www.netlib.org/lapack/explore-html/

[lapack-dlangb]: https://www.netlib.org/lapack/explore-html/d8/d0f/dlangb_8f.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

</section>

<!-- /.links -->
