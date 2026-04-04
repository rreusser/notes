# ztftri

> Computes the inverse of a complex triangular matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var ztftri = require( '@stdlib/lapack/base/ztftri' );
```

#### ztftri.ndarray( transr, uplo, diag, N, a, stride, offset )

Computes the inverse of a complex triangular matrix stored in RFP format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 3x3 lower triangular matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var a = new Complex128Array( [ 2, 1, 0.5, -0.3, 1, 0.5, 4, 0, 3, -1, 0.8, 0.2 ] );
var info = ztftri.ndarray( 'no-transpose', 'lower', 'non-unit', 3, a, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **a**: input/output [`Complex128Array`][@stdlib/array/complex128] in RFP format of length `N*(N+1)/2`.
-   **stride**: stride length for `a`.
-   **offset**: starting index for `a`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The RFP (Rectangular Full Packed) format stores a triangular matrix in a rectangular array, enabling efficient use of Level 3 BLAS operations.
-   On successful exit (`info = 0`), the inverse of the triangular matrix overwrites the input in RFP format.
-   If `info = k > 0`, the element `A(k,k)` is exactly zero and the matrix is singular; its inverse cannot be computed.
-   Uses a Level 3 BLAS algorithm by delegating to `ztrtri` and `ztrmm`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var ztftri = require( '@stdlib/lapack/base/ztftri' );

// 3x3 lower triangular matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var a = new Complex128Array( [ 2, 1, 0.5, -0.3, 1, 0.5, 4, 0, 3, -1, 0.8, 0.2 ] );
var info = ztftri.ndarray( 'no-transpose', 'lower', 'non-unit', 3, a, 1, 0 );
console.log( 'info:', info );
// => info: 0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/array/complex128

</section>

<!-- /.links -->
