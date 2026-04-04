# zhpgst

> Reduce a complex Hermitian-definite generalized eigenproblem to standard form, using packed storage.

<section class="usage">

## Usage

```javascript
var zhpgst = require( '@stdlib/lapack/base/zhpgst' );
```

#### zhpgst( itype, uplo, N, AP, BP )

Reduces a complex Hermitian-definite generalized eigenproblem to standard form, using packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var AP = new Complex128Array( [ 12, 0, 3, 2, 9, 0, 1, -1, 2, 3, 7, 0 ] );
var BP = new Complex128Array( [ 3, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0 ] );

var info = zhpgst( 1, 'upper', 3, AP, BP );
// info => 0
```

#### zhpgst.ndarray( itype, uplo, N, AP, strideAP, offsetAP, BP, strideBP, offsetBP )

Reduces a complex Hermitian-definite generalized eigenproblem to standard form, using packed storage, with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var AP = new Complex128Array( [ 12, 0, 3, 2, 9, 0, 1, -1, 2, 3, 7, 0 ] );
var BP = new Complex128Array( [ 3, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0 ] );

var info = zhpgst.ndarray( 1, 'upper', 3, AP, 1, 0, BP, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **itype**: problem type (1, 2, or 3).
-   **uplo**: specifies whether upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of matrices A and B.
-   **AP**: Hermitian matrix A in packed storage ([`Complex128Array`][mdn-complex128array]).
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **BP**: triangular factor from Cholesky factorization of B in packed storage ([`Complex128Array`][mdn-complex128array]).
-   **strideBP**: stride length for `BP` (in complex elements).
-   **offsetBP**: starting index for `BP` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   If `itype = 1`, the problem is `A*x = lambda*B*x`, and A is overwritten by `inv(U**H)*A*inv(U)` or `inv(L)*A*inv(L**H)`.
-   If `itype = 2` or `3`, the problem is `A*B*x = lambda*x` or `B*A*x = lambda*x`, and A is overwritten by `U*A*U**H` or `L**H*A*L`.
-   `B` must have been previously factorized as `U**H*U` or `L*L**H` by `zpptrf`.
-   `AP` and `BP` are [`Complex128Array`][mdn-complex128array] instances of length `N*(N+1)/2`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpptrf = require( '@stdlib/lapack/base/zpptrf' );
var zhpgst = require( '@stdlib/lapack/base/zhpgst' );

// Hermitian positive-definite B (2x2), upper packed:
var BP = new Complex128Array( [ 4, 0, 1, 2, 10, 0 ] );
zpptrf( 'upper', 2, BP );

// Hermitian A (2x2), upper packed:
var AP = new Complex128Array( [ 5, 0, 1, -1, 3, 0 ] );

var info = zhpgst( 1, 'upper', 2, AP, BP );
// info => 0
// AP now contains the reduced matrix
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-complex128array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Complex128Array

</section>

<!-- /.links -->
