# dggrqf

> Computes a generalized RQ factorization of an M-by-N matrix A and a P-by-N matrix B.

Given an M-by-N matrix A and a P-by-N matrix B, `dggrqf` computes an RQ factorization of A and then uses the orthogonal factor Q to compute a QR factorization of B:

```text
A = R*Q,        B = Z*T*Q,
```

where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal matrix, and R and T are upper trapezoidal/triangular matrices.

<section class="usage">

## Usage

```javascript
var dggrqf = require( '@stdlib/lapack/base/dggrqf' );
```

#### dggrqf( M, p, N, A, LDA, TAUA, strideTAUA, B, LDB, TAUB, strideTAUB )

Computes a generalized RQ factorization of an M-by-N matrix A and a P-by-N matrix B.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 matrix A (column-major):
var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );

// 3x3 matrix B (column-major):
var B = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 1.0, 2.0, 2.0, 3.0, 1.0 ] );
var TAUA = new Float64Array( 3 );
var TAUB = new Float64Array( 3 );

var info = dggrqf( 3, 3, 3, A, 3, TAUA, 1, B, 3, TAUB, 1 );
// info => 0
```

The function has the following parameters:

-   **M**: number of rows of matrix `A`.
-   **p**: number of rows of matrix `B`.
-   **N**: number of columns of matrices `A` and `B`.
-   **A**: [`Float64Array`][mdn-float64array] containing the M-by-N matrix `A`. On exit, the upper triangle (or trapezoid) contains the factor `R`; the elements below the diagonal, with `TAUA`, represent the orthogonal matrix `Q` as a product of elementary reflectors.
-   **LDA**: leading dimension of `A`.
-   **TAUA**: output [`Float64Array`][mdn-float64array] of length `min(M,N)` containing the scalar factors of the elementary reflectors for `Q`.
-   **strideTAUA**: stride for `TAUA`.
-   **B**: [`Float64Array`][mdn-float64array] containing the P-by-N matrix `B`. On exit, the upper triangle contains the factor `T`; the elements below the diagonal, with `TAUB`, represent the orthogonal matrix `Z` as a product of elementary reflectors.
-   **LDB**: leading dimension of `B`.
-   **TAUB**: output [`Float64Array`][mdn-float64array] of length `min(P,N)` containing the scalar factors of the elementary reflectors for `Z`.
-   **strideTAUB**: stride for `TAUB`.

#### dggrqf.ndarray( M, p, N, A, strideA1, strideA2, offsetA, TAUA, strideTAUA, offsetTAUA, B, strideB1, strideB2, offsetB, TAUB, strideTAUB, offsetTAUB )

Computes a generalized RQ factorization using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 matrix A (column-major):
var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );

// 3x3 matrix B (column-major):
var B = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 1.0, 2.0, 2.0, 3.0, 1.0 ] );
var TAUA = new Float64Array( 3 );
var TAUB = new Float64Array( 3 );

var info = dggrqf.ndarray( 3, 3, 3, A, 1, 3, 0, TAUA, 1, 0, B, 1, 3, 0, TAUB, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **strideTAUA**: stride for `TAUA`.
-   **offsetTAUA**: starting index for `TAUA`.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **strideTAUB**: stride for `TAUB`.
-   **offsetTAUB**: starting index for `TAUB`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Both `A` and `B` are overwritten on exit with the factored results.
-   The routine internally allocates workspace. No external workspace array is required.
-   The function returns `0` on successful exit.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dggrqf = require( '@stdlib/lapack/base/dggrqf' );

// Define a 3x3 matrix A in column-major order:
var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );

// Define a 3x3 matrix B in column-major order:
var B = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 1.0, 2.0, 2.0, 3.0, 1.0 ] );
var TAUA = new Float64Array( 3 );
var TAUB = new Float64Array( 3 );

var info = dggrqf( 3, 3, 3, A, 3, TAUA, 1, B, 3, TAUB, 1 );
console.log( 'info:', info );
// => 0

console.log( 'A (R + reflectors):', A );
console.log( 'TAUA:', TAUA );
console.log( 'B (T + reflectors):', B );
console.log( 'TAUB:', TAUB );
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
