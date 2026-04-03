# zpstrf

> Computes the Cholesky factorization with complete pivoting of a complex Hermitian positive semi-definite matrix using a blocked algorithm.

<section class="usage">

## Usage

```javascript
var zpstrf = require( '@stdlib/lapack/base/zpstrf' );
```

#### zpstrf( order, uplo, N, A, LDA, PIV, RANK, tol, WORK )

Computes the Cholesky factorization with complete pivoting of a complex Hermitian positive semi-definite matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 Hermitian positive semi-definite matrix A (column-major, upper):
var A = new Complex128Array( [ 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 1.0, 8.0, 0.0, 0.0, 0.0, 3.0, -2.0, 1.0, 1.0, 6.0, 0.0 ] );
var PIV = new Int32Array( 3 );
var RANK = new Int32Array( 1 );
var WORK = new Float64Array( 6 );

var info = zpstrf( 'column-major', 'upper', 3, A, 3, PIV, RANK, -1.0, WORK );
// returns 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies whether upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of matrix `A`.
-   **A**: input/output [`Complex128Array`][mdn-typed-array] Hermitian matrix.
-   **LDA**: leading dimension of `A`.
-   **PIV**: output [`Int32Array`][mdn-int32array] permutation array (0-based).
-   **RANK**: 1-element [`Int32Array`][mdn-int32array]; on exit, `RANK[0]` = computed rank.
-   **tol**: user-defined tolerance; if negative, a default based on machine epsilon is used.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length `2*N`.

#### zpstrf.ndarray( uplo, N, A, strideA1, strideA2, offsetA, PIV, stridePIV, offsetPIV, RANK, tol, WORK )

Computes the Cholesky factorization with complete pivoting using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 1.0, 8.0, 0.0, 0.0, 0.0, 3.0, -2.0, 1.0, 1.0, 6.0, 0.0 ] );
var PIV = new Int32Array( 3 );
var RANK = new Int32Array( 1 );
var WORK = new Float64Array( 6 );

var info = zpstrf.ndarray( 'upper', 3, A, 1, 3, 0, PIV, 1, 0, RANK, -1.0, WORK );
// returns 0
```

The function has the following additional parameters:

-   **strideA1**: stride of the first dimension of `A` (in complex elements).
-   **strideA2**: stride of the second dimension of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **stridePIV**: stride length for `PIV`.
-   **offsetPIV**: starting index for `PIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `P^T * A * P = U^H * U` (upper) or `P^T * A * P = L * L^H` (lower).
-   `PIV` is 0-based (row `i` was interchanged with row `PIV[i]`).
-   Returns `0` if the factorization completed (full rank), `1` if the matrix is rank-deficient.
-   The computed rank is stored in `RANK[0]`.
-   This is the blocked version of the algorithm. For small matrices, it delegates to the unblocked `zpstf2`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpstrf = require( '@stdlib/lapack/base/zpstrf' );

var A = new Complex128Array( [ 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 1.0, 8.0, 0.0, 0.0, 0.0, 3.0, -2.0, 1.0, 1.0, 6.0, 0.0 ] );
var PIV = new Int32Array( 3 );
var RANK = new Int32Array( 1 );
var WORK = new Float64Array( 6 );

var info = zpstrf( 'column-major', 'upper', 3, A, 3, PIV, RANK, -1.0, WORK );
console.log( 'info:', info );
console.log( 'A (factored):', reinterpret( A, 0 ) );
console.log( 'PIV:', PIV );
console.log( 'RANK:', RANK[ 0 ] );
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
