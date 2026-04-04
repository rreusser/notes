# zlaqgb

> Equilibrate a complex general band matrix using row and column scaling factors.

<section class="usage">

## Usage

```javascript
var zlaqgb = require( '@stdlib/lapack/base/zlaqgb' );
```

#### zlaqgb.ndarray( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, r, strideR, offsetR, c, strideC, offsetC, rowcnd, colcnd, amax )

Equilibrates a complex general M-by-N band matrix A with KL subdiagonals and KU superdiagonals using the row and column scaling factors in the vectors R and C.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 band matrix with kl=1, ku=0, ldab=2:
var AB = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0, 4.0, 2.0 ] );
var r = new Float64Array( [ 0.5, 2.0 ] );
var c = new Float64Array( [ 0.5, 2.0 ] );

var equed = zlaqgb.ndarray( 2, 2, 1, 0, AB, 1, 2, 0, r, 1, 0, c, 1, 0, 0.01, 0.01, 5.0 );
// returns 'both'
```

The function has the following parameters:

-   **M**: number of rows of the matrix A.
-   **N**: number of columns of the matrix A.
-   **kl**: number of subdiagonals within the band of A.
-   **ku**: number of superdiagonals within the band of A.
-   **AB**: input complex band matrix in band storage as a `Complex128Array`.
-   **strideAB1**: stride of the first dimension of `AB` (complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (complex elements).
-   **offsetAB**: starting index for `AB` (complex elements).
-   **r**: row scale factors as a `Float64Array` of length M.
-   **strideR**: stride length for `r`.
-   **offsetR**: starting index for `r`.
-   **c**: column scale factors as a `Float64Array` of length N.
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.
-   **rowcnd**: ratio of the smallest R(i) to the largest R(i).
-   **colcnd**: ratio of the smallest C(i) to the largest C(i).
-   **amax**: absolute value of the largest matrix entry.

The function returns a string indicating the equilibration performed: `'none'`, `'row'`, `'column'`, or `'both'`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine decides whether to scale based on threshold comparisons of `rowcnd`, `colcnd`, and `amax` against internal constants derived from machine precision.
-   Band storage format: element A(i,j) is stored at AB(ku+i-j, j) (0-based).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlaqgb = require( '@stdlib/lapack/base/zlaqgb' );

// 2x2 band matrix with kl=1, ku=0, ldab=2:
var AB = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0, 4.0, 2.0 ] );
var r = new Float64Array( [ 0.5, 2.0 ] );
var c = new Float64Array( [ 1.0, 1.0 ] );

var equed = zlaqgb.ndarray( 2, 2, 1, 0, AB, 1, 2, 0, r, 1, 0, c, 1, 0, 0.01, 1.0, 5.0 );
console.log( 'equed:', equed );
// => equed: row

var ABv = reinterpret( AB, 0 );
console.log( 'AB (scaled):', Array.from( ABv ) );
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

</section>

<!-- /.links -->
