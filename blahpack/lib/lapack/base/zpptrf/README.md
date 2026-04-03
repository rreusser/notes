# zpptrf

> Compute the Cholesky factorization of a complex Hermitian positive definite matrix stored in packed format.

<section class="usage">

## Usage

```javascript
var zpptrf = require( '@stdlib/lapack/base/zpptrf' );
```

#### zpptrf.ndarray( uplo, N, AP, stride, offset )

Computes the Cholesky factorization of a complex Hermitian positive definite matrix stored in packed format.

The factorization has the form `A = U^H * U` if `uplo = 'upper'`, or `A = L * L^H` if `uplo = 'lower'`, where `U` is upper triangular and `L` is lower triangular.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 2x2 HPD matrix in upper packed format: [4, (1+2i), 10]
var AP = new Complex128Array( [ 4, 0, 1, 2, 10, 0 ] );

var info = zpptrf.ndarray( 'upper', 2, AP, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle of the Hermitian matrix is stored.
-   **N**: order of the matrix `A`.
-   **AP**: packed triangular matrix `A` as a [`Complex128Array`][@stdlib/array/complex128].
-   **stride**: stride length for `AP` (in complex elements).
-   **offset**: starting index for `AP` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The function returns an integer `info` value. If `info = 0`, the factorization was successful. If `info = k > 0`, the leading minor of order `k` is not positive definite and the factorization could not be completed.
-   The packed array `AP` is modified in place. On exit, it contains the triangular factor `U` or `L`.
-   Diagonal elements of the input Hermitian matrix must be real. The output diagonal elements are also real (imaginary parts are zeroed).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpptrf = require( '@stdlib/lapack/base/zpptrf' );

// 3x3 HPD matrix in upper packed format:
var AP = new Complex128Array( [ 10, 0, 2, 1, 8, 0, 3, -2, 1, 1, 6, 0 ] );
var info = zpptrf.ndarray( 'upper', 3, AP, 1, 0 );

console.log( 'info:', info );
console.log( 'U (packed):', reinterpret( AP, 0 ) );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
