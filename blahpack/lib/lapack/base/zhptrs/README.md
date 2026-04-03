# zhptrs

> Solves a system of linear equations with a complex Hermitian matrix in packed storage using the factorization computed by zhptrf.

<section class="usage">

## Usage

```javascript
var zhptrs = require( '@stdlib/lapack/base/zhptrs' );
```

#### zhptrs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Solves a system of linear equations with a complex Hermitian matrix in packed storage using the factorization computed by zhptrf.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhptrs = require( '@stdlib/lapack/base/zhptrs' );

// Factored 2x2 Hermitian packed matrix (upper triangle) and pivot indices:
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );

var info = zhptrs.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
// info => 0
// B is overwritten with the solution
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **nrhs**: nrhs.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: output matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The matrix `A` must already be factored by `zhptrf` into `U*D*U**H` or `L*D*L**H` form. The `AP` and `IPIV` arrays are the output of that factorization.
-   The solution matrix `X` overwrites `B` on output.
-   `IPIV` uses 0-based indexing with bitwise NOT convention for 2x2 pivots.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhptrs = require( '@stdlib/lapack/base/zhptrs' );

// Factored 2x2 Hermitian packed matrix (upper triangle):
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );

var info = zhptrs.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
console.log( 'info:', info );
// => 0
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
