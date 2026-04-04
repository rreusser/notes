# zgbequ

> Computes row and column scalings to equilibrate a complex general band matrix.

<section class="usage">

## Usage

```javascript
var zgbequ = require( '@stdlib/lapack/base/zgbequ' );
```

#### zgbequ.ndarray( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, r, strideR, offsetR, c, strideC, offsetC )

Computes row and column scalings intended to equilibrate an M-by-N complex band matrix A stored in LAPACK band storage and reduce its condition number. Uses `CABS1(z) = |Re(z)| + |Im(z)|` for element magnitudes.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 tridiagonal complex band matrix (KL=1, KU=1, LDAB=3):
var AB = new Complex128Array([
    0, 0, 2, 1, 3, 0,
    1, 2, 4, 1, 2, 3,
    1, 1, 5, 0, 0, 0
]);
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var out = zgbequ.ndarray( 3, 3, 1, 1, AB, 1, 3, 0, r, 1, 0, c, 1, 0 );
// out.info => 0
// out.amax => 5.0
```

The function has the following parameters:

-   **M**: number of rows of A.
-   **N**: number of columns of A.
-   **kl**: number of subdiagonals within the band of A.
-   **ku**: number of superdiagonals within the band of A.
-   **AB**: input complex band matrix stored in LAPACK band format (`Complex128Array`).
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **r**: output row scale factors (`Float64Array`, length M).
-   **strideR**: stride for `r`.
-   **offsetR**: starting index for `r`.
-   **c**: output column scale factors (`Float64Array`, length N).
-   **strideC**: stride for `c`.
-   **offsetC**: starting index for `c`.

Returns an object with `info`, `rowcnd`, `colcnd`, and `amax`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The band matrix A is stored in rows 1 to KL+KU+1 such that `AB(ku+1+i-j, j) = A(i,j)`.
-   Element magnitudes use `CABS1(z) = |Re(z)| + |Im(z)|`, not the complex modulus.
-   If `info = 0`, the equilibration was successful. If `info = i` (1-based) where `i <= M`, the i-th row of A is exactly zero. If `info = M + j`, the j-th column is exactly zero after row scaling.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbequ = require( '@stdlib/lapack/base/zgbequ' );

// Diagonal complex band matrix (KL=0, KU=0):
var AB = new Complex128Array([ 3, 4, 1, 0, 0, 2 ]);
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var out = zgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
console.log( 'info:', out.info );
console.log( 'amax:', out.amax );
console.log( 'r:', r );
console.log( 'c:', c );
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
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
