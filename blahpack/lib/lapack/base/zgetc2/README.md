# zgetc2

> Computes an LU factorization with complete pivoting of a general N-by-N complex matrix.

<section class="usage">

## Usage

```javascript
var zgetc2 = require( '@stdlib/lapack/base/zgetc2' );
```

#### zgetc2.ndarray( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV )

Computes an LU factorization with complete pivoting of a general N-by-N complex matrix.

The factorization has the form `A = P * L * U * Q`, where P and Q are permutation matrices, L is lower triangular with unit diagonal elements, and U is upper triangular.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 1.0, 2.0, 5.0, 6.0, 3.0, 4.0, 7.0, 8.0 ] );
var IPIV = new Int32Array( 2 );
var JPIV = new Int32Array( 2 );

var info = zgetc2.ndarray( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **N**: order of the matrix.
-   **A**: N-by-N complex matrix stored as a `Complex128Array` (overwritten with L and U on exit).
-   **strideA1**: stride of the first dimension of `A` (in complex elements).
-   **strideA2**: stride of the second dimension of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **IPIV**: row pivot indices as an `Int32Array` (length N), 0-based.
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **JPIV**: column pivot indices as an `Int32Array` (length N), 0-based.
-   **strideJPIV**: stride for `JPIV`.
-   **offsetJPIV**: starting index for `JPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On exit, `A` contains the LU factors. `IPIV[i]` and `JPIV[i]` are the row and column pivots applied at step `i`.
-   A return value of `info > 0` indicates that `U(info,info)` is likely to produce overflow when solving `Ax = b`, so U is perturbed to avoid overflow.
-   `IPIV` and `JPIV` use 0-based indexing.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zgetc2 = require( '@stdlib/lapack/base/zgetc2' );

// 2x2 complex matrix in column-major order:
var A = new Complex128Array( [ 1.0, 2.0, 5.0, 6.0, 3.0, 4.0, 7.0, 8.0 ] );
var IPIV = new Int32Array( 2 );
var JPIV = new Int32Array( 2 );

var info = zgetc2.ndarray( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
console.log( 'info:', info );
console.log( 'IPIV:', IPIV );
console.log( 'JPIV:', JPIV );
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
