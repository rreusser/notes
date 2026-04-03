# zspcon

> Estimates the reciprocal of the condition number of a complex symmetric matrix in packed storage.

<section class="usage">

## Usage

```javascript
var zspcon = require( '@stdlib/lapack/base/zspcon' );
```

#### zspcon.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK )

Estimates the reciprocal of the condition number (in the 1-norm) of a complex symmetric matrix `A` in packed storage, using the factorization `A = U*D*U^T` or `A = L*D*L^T` computed by `zsptrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 2x2 identity in upper packed format: A(1,1)=1, A(1,2)=0, A(2,2)=1
var AP = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var rcond = new Float64Array( 1 );
var WORK = new Complex128Array( 4 );

var info = zspcon.ndarray( 'upper', 2, AP, 1, 0, IPIV, 1, 0, 1.0, rcond, WORK, 1, 0 );
// info => 0
// rcond[ 0 ] => 1.0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: factored packed matrix from `zsptrf` as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideAP**: stride for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **IPIV**: pivot indices from `zsptrf` as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **anorm**: the 1-norm of the original matrix `A`.
-   **rcond**: output [`Float64Array`][mdn-float64array] where `rcond[0]` receives the reciprocal condition number.
-   **WORK**: workspace [`Complex128Array`][@stdlib/array/complex128] of length at least `2*N`.
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses reverse communication with `zlacn2` to estimate `norm(inv(A))`, then computes `rcond = 1 / (anorm * norm(inv(A)))`.
-   If `N` is `0`, the reciprocal condition number is set to `1.0`.
-   If `anorm` is `0` or negative, the reciprocal condition number is set to `0.0`.
-   If the factored matrix contains a zero diagonal element (from a 1x1 pivot), `rcond` is set to `0.0` (singular matrix).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zspcon = require( '@stdlib/lapack/base/zspcon' );

// 3x3 identity in upper packed format
var AP = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
var IPIV = new Int32Array( [ 0, 1, 2 ] );
var rcond = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );

var info = zspcon.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, 1.0, rcond, WORK, 1, 0 );
console.log( 'info:', info );
console.log( 'rcond:', rcond[ 0 ] );
// => info: 0
// => rcond: 1.0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
