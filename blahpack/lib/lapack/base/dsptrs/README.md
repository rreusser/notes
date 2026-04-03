# dsptrs

> Solves a system of linear equations with a real symmetric matrix in packed storage using the factorization computed by dsptrf.

<section class="usage">

## Usage

```javascript
var dsptrs = require( '@stdlib/lapack/base/dsptrs' );
```

#### dsptrs( uplo, N, nrhs, AP, IPIV, B, LDB )

Solves a system of linear equations `A * X = B` with a real symmetric matrix `A` in packed storage, using the factorization `A = U*D*U^T` or `A = L*D*L^T` computed by `dsptrf`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// Pre-factored 2x2 diagonal matrix (lower packed):
var AP = new Float64Array( [ 2.0, 0.0, 3.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Float64Array( [ 4.0, 9.0 ] );

var info = dsptrs( 'lower', 2, 1, AP, IPIV, B, 2 );
// info => 0
// B => <Float64Array>[ 2.0, 3.0 ]
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of A is packed (`'upper'` or `'lower'`). Must match the factorization.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side vectors.
-   **AP**: factored packed matrix from `dsptrf`, length `N*(N+1)/2` ([`Float64Array`][mdn-float64array]).
-   **IPIV**: pivot indices from `dsptrf`, length `N` ([`Int32Array`][mdn-int32array]).
-   **B**: right-hand side matrix, overwritten with the solution on exit ([`Float64Array`][mdn-float64array]).
-   **LDB**: leading dimension of `B` (>= max(1,N)).

#### dsptrs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Solves a system of linear equations with a real symmetric matrix in packed storage using the factorization computed by `dsptrf`, with explicit stride and offset control.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Float64Array( [ 2.0, 0.0, 3.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Float64Array( [ 4.0, 9.0 ] );

var info = dsptrs.ndarray( 'lower', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
// info => 0
// B => <Float64Array>[ 2.0, 3.0 ]
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of A is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side vectors.
-   **AP**: factored packed matrix from `dsptrf`.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **IPIV**: pivot indices from `dsptrf`.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: input/output right-hand side / solution matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dsptrs` requires a prior call to `dsptrf` to compute the Bunch-Kaufman factorization.
-   The IPIV array uses 0-based conventions: non-negative values indicate 1x1 pivots with row interchange, negative values (`~kp`, bitwise NOT) indicate 2x2 pivots.
-   The routine supports both upper and lower packed triangular storage formats.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
var dsptrs = require( '@stdlib/lapack/base/dsptrs' );

// 3x3 SPD matrix in lower packed storage:
// A = [ 4  2  1 ]
//     [ 2  5  3 ]
//     [ 1  3  6 ]
var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );

// Factor: A = L * D * L^T
dsptrf( 'lower', 3, AP, IPIV );

// Solve A * x = b where b = A * [1,1,1]^T = [7,10,10]^T
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
dsptrs( 'lower', 3, 1, AP, IPIV, B, 3 );
// B is now approximately [ 1.0, 1.0, 1.0 ]
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
