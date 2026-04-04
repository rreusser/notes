# zpptri

> Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.

<section class="usage">

## Usage

```javascript
var zpptri = require( '@stdlib/lapack/base/zpptri' );
```

#### zpptri( uplo, N, AP )

Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization computed by `zpptrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 2x2 upper Cholesky factor (already factored):
var AP = new Complex128Array( [ 3.0, 0.0, 1.0, 0.0, 2.0, 0.0 ] );

var info = zpptri( 'upper', 2, AP );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle of `A` is stored.
-   **N**: order of the matrix `A`.
-   **AP**: packed Hermitian matrix as a [`Complex128Array`][@stdlib/array/complex128], containing the Cholesky factor from `zpptrf`.

#### zpptri.ndarray( uplo, N, AP, stride, offset )

Computes the inverse with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 2x2 upper Cholesky factor (already factored):
var AP = new Complex128Array( [ 3.0, 0.0, 1.0, 0.0, 2.0, 0.0 ] );

var info = zpptri.ndarray( 'upper', 2, AP, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **stride**: stride length for `AP` (in complex elements).
-   **offset**: starting index for `AP` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On entry, `AP` must contain the triangular factor U or L from the Cholesky factorization `A = U^H * U` or `A = L * L^H`, as computed by `zpptrf`, in packed format.
-   On exit, `AP` is overwritten by the upper or lower triangle of the inverse of `A`, in packed format.
-   The return value is `0` on success, or `k > 0` if the `k`-th diagonal element of the Cholesky factor is zero (the matrix is singular).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpptri = require( '@stdlib/lapack/base/zpptri' );

// 2x2 upper Cholesky factor (real diagonal, already factored):
var AP = new Complex128Array( [ 3.0, 0.0, 1.0, 0.0, 2.0, 0.0 ] );

var info = zpptri( 'upper', 2, AP );
console.log( 'info:', info );
console.log( 'inverse (packed):', reinterpret( AP, 0 ) );
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

</section>

<!-- /.links -->
