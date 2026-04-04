# dsytri

> Computes the inverse of a real symmetric matrix using the factorization computed by dsytrf.

<section class="usage">

## Usage

```javascript
var dsytri = require( '@stdlib/lapack/base/dsytri' );
```

#### dsytri( order, uplo, N, A, LDA, IPIV, strideIPIV, WORK )

Computes the inverse of a real symmetric matrix `A` using the factorization `A = U * D * U^T` or `A = L * D * L^T` computed by [`dsytrf`][@stdlib/lapack/base/dsytrf].

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 2x2 factored symmetric matrix (column-major), with D and L factors:
var A = new Float64Array( [ 2.0, 0.5, 0.0, 1.5 ] );
var IPIV = new Int32Array( [ 1, 1 ] );
var WORK = new Float64Array( 2 );

var info = dsytri( 'column-major', 'lower', 2, A, 2, IPIV, 1, WORK );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout ('row-major' or 'column-major').
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **A**: input matrix stored as a [`Float64Array`][mdn-float64array], containing the factorization from `dsytrf`.
-   **LDA**: leading dimension of `A`.
-   **IPIV**: pivot index array from `dsytrf` stored as an [`Int32Array`][mdn-int32array], length `N`.
-   **strideIPIV**: stride length for `IPIV`.
-   **WORK**: workspace array stored as a [`Float64Array`][mdn-float64array], length `N`.

#### dsytri.ndarray( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK )

Computes the inverse using an alternative interface with stride and offset parameters.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Float64Array( [ 2.0, 0.5, 0.0, 1.5 ] );
var IPIV = new Int32Array( [ 1, 1 ] );
var WORK = new Float64Array( 2 );

var info = dsytri.ndarray( 'lower', 2, A, 1, 2, 0, IPIV, 1, 0, WORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **A**: input matrix as a [`Float64Array`][mdn-float64array].
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **IPIV**: pivot index array as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **WORK**: workspace array as a [`Float64Array`][mdn-float64array].
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On entry, `A` must contain the block diagonal matrix `D` and the multipliers used to obtain the factor `U` (or `L`) as computed by `dsytrf`, stored in full format. On exit, if `info === 0`, `A` contains the upper (or lower) triangle of the inverse.
-   `IPIV` stores 0-based pivot indices from `dsytrf`/`dsytf2`. If `IPIV[k] >= 0`, a 1x1 pivot was used. If `IPIV[k] < 0`, then `IPIV[k] = ~p` where `p` is the 0-based row/column that was interchanged.
-   Returns 0 if successful. If the return value is `k > 0`, then `D[k-1,k-1]` is exactly zero, indicating that the matrix is singular and the inverse could not be computed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytri = require( '@stdlib/lapack/base/dsytri' );

// 3x3 factored symmetric matrix (column-major), already factored by dsytf2:
var A = new Float64Array( [ 4.0, 0.5, 0.25, 0.0, 2.0, 0.5, 0.0, 0.0, 1.0 ] );
var IPIV = new Int32Array( [ 0, 1, 2 ] );
var WORK = new Float64Array( 3 );

var info = dsytri.ndarray( 'upper', 3, A, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
console.log( 'info:', info );
console.log( 'A (inverse):', A );
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
