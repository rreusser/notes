# zpbstf

> Computes a split Cholesky factorization of a complex Hermitian positive definite band matrix.

<section class="usage">

## Usage

```javascript
var zpbstf = require( '@stdlib/lapack/base/zpbstf' );
```

#### zpbstf.ndarray( uplo, N, kd, AB, strideAB1, strideAB2, offsetAB )

Computes a split Cholesky factorization of a complex Hermitian positive definite band matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 4x4 HPD tridiagonal (KD=1), upper band storage (LDAB=2):
var ab = new Complex128Array([
    0.0, 0.0, 4.0, 0.0,
    -1.0, 0.5, 4.0, 0.0,
    -1.0, 0.5, 4.0, 0.0,
    -1.0, 0.5, 4.0, 0.0
]);

var info = zpbstf.ndarray( 'upper', 4, 1, ab, 1, 2, 0 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle is stored.
-   **N**: order of the matrix A.
-   **kd**: number of super/sub-diagonals.
-   **AB**: input/output band matrix in band storage as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A = S**H * S` where S is a band matrix of the same bandwidth as A with structure `S = [ U; M L ]`, where U is upper triangular of order `m = floor((n+kd)/2)` and L is lower triangular of order `n-m`.
-   This routine is designed to be used in conjunction with `zhbgst`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpbstf = require( '@stdlib/lapack/base/zpbstf' );

// 4x4 HPD band matrix with KD=1, upper storage (LDAB=2):
var ab = new Complex128Array([
    0.0, 0.0, 4.0, 0.0,
    -1.0, 0.5, 4.0, 0.0,
    -1.0, 0.5, 4.0, 0.0,
    -1.0, 0.5, 4.0, 0.0
]);

var info = zpbstf.ndarray( 'upper', 4, 1, ab, 1, 2, 0 );
console.log( 'info:', info );
console.log( 'S (band storage):', reinterpret( ab, 0 ) );
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
