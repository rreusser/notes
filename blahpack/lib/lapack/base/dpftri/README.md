# dpftri

> Computes the inverse of a real symmetric positive definite matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var dpftri = require( '@stdlib/lapack/base/dpftri' );
```

#### dpftri.ndarray( transr, uplo, N, A, strideA, offsetA )

Computes the inverse of a real symmetric positive definite matrix stored in Rectangular Full Packed (RFP) format, using the Cholesky factorization computed by `dpftrf`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 SPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
// A = [10, 3, 1; 3, 8, 2; 1, 2, 6]
var A = new Float64Array( [ 10.0, 3.0, 1.0, 6.0, 8.0, 2.0 ] );

// First factorize with dpftrf, then invert with dpftri:
var info = dpftri.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the storage format of the RFP matrix (`'no-transpose'` or `'transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **A**: input/output [`Float64Array`][mdn-float64array] in RFP format of length `N*(N+1)/2`.
-   **strideA**: stride length for `A`.
-   **offsetA**: starting index for `A`.

The function returns an `integer` status code: `0` indicates success, and a value `k > 0` indicates that the `(k,k)` element of the Cholesky factor is zero.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The input matrix `A` must contain the Cholesky factorization as computed by `dpftrf`. On exit, `A` is overwritten with the inverse.
-   The routine operates on matrices in Rectangular Full Packed (RFP) format, which stores a symmetric/triangular matrix using `N*(N+1)/2` elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpftrf = require( '@stdlib/lapack/base/dpftrf' );
var dpftri = require( '@stdlib/lapack/base/dpftri' );

// 3x3 SPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 10.0, 3.0, 1.0, 6.0, 8.0, 2.0 ] );

// Factorize...
var info = dpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
console.log( 'dpftrf info:', info );

// Invert...
info = dpftri.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
console.log( 'dpftri info:', info );
console.log( 'Inverse (RFP):', A );
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
