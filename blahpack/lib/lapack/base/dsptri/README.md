# dsptri

> Computes the inverse of a real symmetric matrix in packed storage using the factorization computed by dsptrf.

<section class="usage">

## Usage

```javascript
var dsptri = require( '@stdlib/lapack/base/dsptri' );
```

#### dsptri( uplo, N, AP, IPIV )

Computes the inverse of a real symmetric matrix stored in packed format, using the factorization `A = U * D * U^T` or `A = L * D * L^T` computed by [`dsptrf`][@stdlib/lapack/base/dsptrf].

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );

// 3x3 symmetric positive definite matrix (upper packed):
// A = [ 4  2  1; 2  5  3; 1  3  6 ]
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );

// Factor...
dsptrf( 'upper', 3, AP, IPIV );

// Invert...
var info = dsptri( 'upper', 3, AP, IPIV );
// info => 0
```

#### dsptri.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK )

Computes the inverse of a real symmetric matrix in packed storage using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );
var WORK = new Float64Array( 3 );

dsptrf( 'upper', 3, AP, IPIV );
var info = dsptri.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: packed symmetric matrix containing the factorization from [`dsptrf`][@stdlib/lapack/base/dsptrf] as a [`Float64Array`][mdn-float64array].
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **IPIV**: pivot index array from [`dsptrf`][@stdlib/lapack/base/dsptrf] as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **WORK**: workspace array as a [`Float64Array`][mdn-float64array] of length at least `N`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine requires the output of [`dsptrf`][@stdlib/lapack/base/dsptrf] as input. The packed matrix `AP` is overwritten with the inverse on exit.
-   `IPIV` uses 0-based indices with bitwise NOT (`~p`) encoding for 2x2 pivots, matching the convention of the [`dsptrf`][@stdlib/lapack/base/dsptrf] implementation.
-   The function returns `0` on success. A return value `k > 0` indicates that `D(k,k)` is exactly zero and the matrix is singular.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
var dsptri = require( '@stdlib/lapack/base/dsptri' );

// 3x3 symmetric positive definite (lower packed):
var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );

dsptrf( 'lower', 3, AP, IPIV );
var info = dsptri( 'lower', 3, AP, IPIV );

console.log( 'info:', info );
console.log( 'inv(A) (lower packed):', AP );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/lapack/base/dsptrf]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/lapack/base/dsptrf

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
