# dspgst

> Reduce a real symmetric-definite generalized eigenproblem to standard form, using packed storage.

<section class="usage">

## Usage

```javascript
var dspgst = require( '@stdlib/lapack/base/dspgst' );
```

#### dspgst( itype, uplo, N, AP, BP )

Reduces a real symmetric-definite generalized eigenproblem to standard form, using packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var BP = new Float64Array( [ 2.0, 0.0, 2.0, 0.0, 0.0, 1.5 ] ); // already Cholesky-factored

var info = dspgst( 1, 'upper', 3, AP, BP );
// info => 0
```

The function has the following parameters:

-   **itype**: problem type (1, 2, or 3).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of matrices A and B.
-   **AP**: symmetric matrix A in packed storage ([`Float64Array`][mdn-float64array]).
-   **BP**: triangular factor from Cholesky factorization of B in packed storage ([`Float64Array`][mdn-float64array]).

#### dspgst.ndarray( itype, uplo, N, AP, strideAP, offsetAP, BP, strideBP, offsetBP )

Reduces a real symmetric-definite generalized eigenproblem to standard form, using packed storage and alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 0.0, 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var BP = new Float64Array( [ 0.0, 2.0, 0.0, 2.0, 0.0, 0.0, 1.5 ] );

var info = dspgst.ndarray( 1, 'upper', 3, AP, 1, 1, BP, 1, 1 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideBP**: stride length for `BP`.
-   **offsetBP**: starting index for `BP`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `AP` and `BP` are stored in packed format with `N*(N+1)/2` elements.
-   `BP` must contain the triangular factor from a Cholesky factorization of B, as computed by `dpptrf`.
-   For `itype = 1`: A is overwritten by `inv(U^T)*A*inv(U)` (upper) or `inv(L)*A*inv(L^T)` (lower).
-   For `itype = 2` or `3`: A is overwritten by `U*A*U^T` (upper) or `L^T*A*L` (lower).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpptrf = require( '@stdlib/lapack/base/dpptrf' );
var dspgst = require( '@stdlib/lapack/base/dspgst' );

// 3x3 SPD matrix B in upper packed format:
var BP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 1.0, 3.0 ] );
dpptrf( 'upper', 3, BP );

// 3x3 symmetric matrix A in upper packed format:
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );

var info = dspgst( 1, 'upper', 3, AP, BP );
console.log( 'info:', info );
console.log( 'AP:', AP );
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

</section>

<!-- /.links -->
