# dpbstf

> Computes a split Cholesky factorization of a real symmetric positive definite band matrix.

<section class="usage">

## Usage

```javascript
var dpbstf = require( '@stdlib/lapack/base/dpbstf' );
```

#### dpbstf.ndarray( uplo, N, kd, AB, strideAB1, strideAB2, offsetAB )

Computes a split Cholesky factorization of a real symmetric positive definite band matrix `A`, stored in band format. The factorization has the form `A = S**T * S` where `S` is a band matrix of the same bandwidth.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 4x4 SPD tridiagonal matrix (KD=1), upper band storage (LDAB=2):
var ab = new Float64Array( [ 0.0, 2.0, -1.0, 2.0, -1.0, 2.0, -1.0, 2.0 ] );

var info = dpbstf.ndarray( 'upper', 4, 1, ab, 1, 2, 0 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle of the band matrix is stored.
-   **N**: order of the matrix `A` (number of rows and columns).
-   **kd**: number of super-diagonals (if `uplo` is `'upper'`) or sub-diagonals (if `uplo` is `'lower'`).
-   **AB**: input/output band matrix in band storage format as a [`Float64Array`][mdn-float64array].
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.

The function returns an integer `info` status code: `0` indicates success; a positive value `j` indicates that the factorization could not be completed because element `a(j,j)` was not positive.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dpbstf` is designed to be used as a preprocessing step for `dsbgst` (generalized symmetric band eigenproblem).
-   The routine modifies `AB` in place, overwriting it with the split Cholesky factor `S`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpbstf = require( '@stdlib/lapack/base/dpbstf' );

// 5x5 SPD tridiagonal matrix with upper band storage (LDAB=2):
var ab = new Float64Array( [
    0.0, 2.0,
    -1.0, 2.0,
    -1.0, 2.0,
    -1.0, 2.0,
    -1.0, 2.0
] );

var info = dpbstf.ndarray( 'upper', 5, 1, ab, 1, 2, 0 );
console.log( 'info:', info );
// => 0
console.log( 'S (band storage):', ab );
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
