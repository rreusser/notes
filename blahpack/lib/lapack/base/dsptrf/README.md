# dsptrf

> Computes the Bunch-Kaufman factorization of a real symmetric matrix in packed storage.

<section class="usage">

## Usage

```javascript
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
```

#### dsptrf( uplo, N, AP, IPIV )

Computes the Bunch-Kaufman factorization of a real symmetric matrix `A` stored in packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 symmetric positive definite matrix (lower packed):
// [4 2 1]
// [2 5 3]
// [1 3 6]
var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );

var info = dsptrf( 'lower', 3, AP, IPIV );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed ('upper' or 'lower').
-   **N**: order of the matrix `A`.
-   **AP**: packed symmetric matrix stored as a [`Float64Array`][mdn-float64array], length `N*(N+1)/2`.
-   **IPIV**: pivot index output array stored as an [`Int32Array`][mdn-int32array], length `N`.

#### dsptrf.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV )

Computes the Bunch-Kaufman factorization using an alternative interface with stride and offset parameters.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );

var info = dsptrf.ndarray( 'lower', 3, AP, 1, 0, IPIV, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed ('upper' or 'lower').
-   **N**: order of the matrix `A`.
-   **AP**: packed symmetric matrix as a [`Float64Array`][mdn-float64array].
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **IPIV**: pivot index output array as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A = U*D*U^T` (if uplo = 'upper') or `A = L*D*L^T` (if uplo = 'lower'), where U (or L) is a product of permutation and unit upper (lower) triangular matrices, and D is symmetric and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
-   `IPIV` stores 0-based pivot indices. If `IPIV[k] >= 0`, a 1x1 pivot was used and rows/columns k and `IPIV[k]` were interchanged. If `IPIV[k] < 0`, a 2x2 pivot was used and `IPIV[k] = ~p` where `p` is the 0-based row/column interchanged.
-   Returns 0 if successful. If the return value is `k > 0`, then `D(k,k)` is exactly zero, indicating that the matrix is singular.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );

// 3x3 symmetric positive definite matrix (lower packed):
// [4 2 1]
// [2 5 3]
// [1 3 6]
var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );

var info = dsptrf( 'lower', 3, AP, IPIV );
// info => 0
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
