# dgbequ

> Computes row and column scalings to equilibrate a real general band matrix.

<section class="usage">

## Usage

```javascript
var dgbequ = require( '@stdlib/lapack/base/dgbequ' );
```

#### dgbequ.ndarray( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, r, strideR, offsetR, c, strideC, offsetC )

Computes row and column scalings intended to equilibrate an M-by-N real general band matrix A and reduce its condition number.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 diagonal matrix: diag(3, 1, 2)
var AB = new Float64Array( [ 3, 1, 2 ] );
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var out = dgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
// out.info => 0
// out.amax => 3.0
```

The function has the following parameters:

-   **M**: number of rows of the matrix A.
-   **N**: number of columns of the matrix A.
-   **kl**: number of subdiagonals within the band of A.
-   **ku**: number of superdiagonals within the band of A.
-   **AB**: band matrix in LAPACK band storage (KL+KU+1 by N).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **r**: output row scale factors, length M.
-   **strideR**: stride length for `r`.
-   **offsetR**: starting index for `r`.
-   **c**: output column scale factors, length N.
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.

The function returns an object with the following properties:

-   **info**: status code. `0` indicates success. A positive value `i` where `i <= M` indicates the `i`-th row is exactly zero; `i > M` indicates the `(i-M)`-th column is exactly zero (after row scaling).
-   **rowcnd**: ratio of the smallest to the largest row scale factor.
-   **colcnd**: ratio of the smallest to the largest column scale factor.
-   **amax**: absolute value of the largest matrix element.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   R returns the row scale factors and C the column scale factors, chosen to try to make the largest element in each row and column of the matrix B with elements `B(i,j)=R(i)*A(i,j)*C(j)` have absolute value 1.
-   If `rowcnd >= 0.1` and `amax` is neither too large nor too small, row scaling is not needed.
-   If `colcnd >= 0.1`, column scaling is not needed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dgbequ = require( '@stdlib/lapack/base/dgbequ' );

// Diagonal real band matrix (KL=0, KU=0):
var AB = new Float64Array( [ 3, 1, 2 ] );
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var out = dgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );

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
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
