# dlaqgb

> Equilibrate a general band matrix using row and column scaling factors.

<section class="usage">

## Usage

```javascript
var dlaqgb = require( '@stdlib/lapack/base/dlaqgb' );
```

#### dlaqgb.ndarray( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, r, strideR, offsetR, c, strideC, offsetC, rowcnd, colcnd, amax )

Equilibrates a general M-by-N band matrix A with KL sub-diagonals and KU super-diagonals using the row and column scaling factors in the vectors R and C.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 4x5 band matrix with KL=1, KU=2 in band storage (LDAB=4):
var AB = new Float64Array([
    0.0, 0.0, 2.0, 1.5,   // column 1
    0.0, 3.0, 1.0, 0.5,   // column 2
    0.8, 2.5, 4.0, 1.2,   // column 3
    0.6, 3.5, 2.0, 0.0,   // column 4
    1.0, 0.7, 0.0, 0.0    // column 5
]);

var r = new Float64Array( [ 0.5, 1.0, 0.8, 0.25 ] );
var c = new Float64Array( [ 0.6, 1.0, 0.7, 0.9, 0.4 ] );

var equed = dlaqgb.ndarray( 4, 5, 1, 2, AB, 1, 4, 0, r, 1, 0, c, 1, 0, 0.01, 0.01, 4.0 );
// equed => 'both'
```

The function has the following parameters:

-   **M**: number of rows of the matrix A.
-   **N**: number of columns of the matrix A.
-   **kl**: number of sub-diagonals within the band of A.
-   **ku**: number of super-diagonals within the band of A.
-   **AB**: input/output band matrix in band storage as a [`Float64Array`][mdn-float64array].
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **r**: row scale factors as a [`Float64Array`][mdn-float64array], length M.
-   **strideR**: stride length for `r`.
-   **offsetR**: starting index for `r`.
-   **c**: column scale factors as a [`Float64Array`][mdn-float64array], length N.
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.
-   **rowcnd**: ratio of the smallest R(i) to the largest R(i).
-   **colcnd**: ratio of the smallest C(i) to the largest C(i).
-   **amax**: absolute value of the largest matrix entry.

The function returns a string indicating the form of equilibration applied:

-   `'none'`: no equilibration.
-   `'row'`: row equilibration (A premultiplied by diag(R)).
-   `'column'`: column equilibration (A postmultiplied by diag(C)).
-   `'both'`: both row and column equilibration (A replaced by diag(R) \* A \* diag(C)).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The band matrix A is stored in LAPACK band storage format, where element A(i,j) is stored at `AB[ku+i-j, j]` (0-based).
-   The decision to equilibrate is based on internal thresholds: row scaling is skipped when `rowcnd >= 0.1` and `SMALL <= amax <= LARGE`, and column scaling is skipped when `colcnd >= 0.1`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlaqgb = require( '@stdlib/lapack/base/dlaqgb' );

var AB = new Float64Array([
    0.0, 0.0, 2.0, 1.5,
    0.0, 3.0, 1.0, 0.5,
    0.8, 2.5, 4.0, 1.2,
    0.6, 3.5, 2.0, 0.0,
    1.0, 0.7, 0.0, 0.0
]);

var r = new Float64Array( [ 0.5, 1.0, 0.8, 0.25 ] );
var c = new Float64Array( [ 0.6, 1.0, 0.7, 0.9, 0.4 ] );

var equed = dlaqgb.ndarray( 4, 5, 1, 2, AB, 1, 4, 0, r, 1, 0, c, 1, 0, 0.01, 0.01, 4.0 );
console.log( 'equed:', equed );
// => 'both'
console.log( 'AB:', AB );
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
