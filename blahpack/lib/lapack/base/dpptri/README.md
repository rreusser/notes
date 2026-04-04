# dpptri

> Compute the inverse of a real symmetric positive definite matrix in packed storage using the Cholesky factorization.

<section class="usage">

## Usage

```javascript
var dpptri = require( '@stdlib/lapack/base/dpptri' );
```

#### dpptri.ndarray( uplo, N, AP, stride, offset )

Computes the inverse of a real symmetric positive definite matrix `A` using the Cholesky factorization `A = U^T * U` or `A = L * L^T` computed by `dpptrf`. The matrix is stored in packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Upper packed Cholesky factor of a 2x2 SPD matrix (pre-factored):
var AP = new Float64Array( [ 2.0, 0.5, 1.3228756555322954 ] );

var info = dpptri.ndarray( 'upper', 2, AP, 1, 0 );
// info => 0
// AP now contains the upper triangle of the inverse in packed format
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle is stored.
-   **N**: order of the matrix `A`.
-   **AP**: packed triangular matrix of length `N*(N+1)/2`.
-   **stride**: stride length for `AP`.
-   **offset**: starting index for `AP`.

The function returns an integer `info` status code: `0` indicates success; `k > 0` indicates the `k`-th diagonal element of the triangular factor is zero and the matrix is singular.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `AP` must contain the Cholesky factor computed by [`dpptrf`][@stdlib/lapack/base/dpptrf] before calling `dpptri`.
-   On exit, `AP` is overwritten with the upper or lower triangle of the inverse of `A`.
-   The routine supports non-unit strides and offsets for operating on sub-arrays.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpptrf = require( '@stdlib/lapack/base/dpptrf' );
var dpptri = require( '@stdlib/lapack/base/dpptri' );

// 3x3 SPD matrix in upper packed format:
// A = [25 5 -5; 5 10 2; -5 2 6]
var AP = new Float64Array( [ 25.0, 5.0, 10.0, -5.0, 2.0, 6.0 ] );

// Factor:
dpptrf.ndarray( 'upper', 3, AP, 1, 0 );

// Invert:
var info = dpptri.ndarray( 'upper', 3, AP, 1, 0 );
// info => 0
// AP now contains the upper triangle of inv(A)
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/lapack/base/dpptrf]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/lapack/base/dpptrf

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
