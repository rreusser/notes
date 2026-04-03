# zsptrs

> Solves a system of linear equations with a complex symmetric matrix in packed storage using the factorization computed by zsptrf.

<section class="usage">

## Usage

```javascript
var zsptrs = require( '@stdlib/lapack/base/zsptrs' );
```

#### zsptrs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Solves the system of linear equations `A * X = B` with a complex symmetric matrix `A` in packed storage, using the factorization `A = U * D * U**T` or `A = L * D * L**T` computed by `zsptrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 5.0, -1.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );

zsptrs.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand sides (columns of `B`).
-   **AP**: factored packed symmetric matrix from `zsptrf` as a `Complex128Array`.
-   **strideAP**: stride for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **IPIV**: pivot indices from `zsptrf` as an `Int32Array` (0-based).
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix as a `Complex128Array`, overwritten with the solution.
-   **strideB1**: first dimension stride of `B` (in complex elements).
-   **strideB2**: second dimension stride of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `IPIV` uses 0-based indexing with bitwise-NOT encoding for 2x2 pivots (negative values indicate 2x2 pivot blocks).
-   Unlike `zhptrs` (Hermitian), `zsptrs` uses transpose (not conjugate-transpose) and does not conjugate off-diagonal elements.
-   The diagonal elements of a symmetric matrix are fully complex (unlike Hermitian where they are real).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsptrs = require( '@stdlib/lapack/base/zsptrs' );

var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 5.0, -1.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );

var info = zsptrs.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );

var Bv = reinterpret( B, 0 );
// Bv contains the solution X
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

</section>

<!-- /.links -->
