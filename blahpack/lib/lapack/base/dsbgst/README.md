# dsbgst

> Reduces a real symmetric-definite banded generalized eigenproblem to standard form.

<section class="usage">

## Usage

```javascript
var dsbgst = require( '@stdlib/lapack/base/dsbgst' );
```

#### dsbgst.ndarray( vect, uplo, N, ka, kb, AB, strideAB1, strideAB2, offsetAB, BB, strideBB1, strideBB2, offsetBB, X, strideX1, strideX2, offsetX, WORK, strideWORK, offsetWORK )

Reduces a real symmetric-definite banded generalized eigenproblem `A*x = lambda*B*x` to standard form `C*y = lambda*y`, such that C has the same bandwidth as A.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Diagonal matrix A (ka=0), diagonal matrix B (kb=0), N=3, upper:
var AB = new Float64Array( [ 5.0, 6.0, 7.0 ] );
var BB = new Float64Array( [ 2.0, 3.0, 4.0 ] );
var X = new Float64Array( 1 );
var WORK = new Float64Array( 6 );

var info = dsbgst.ndarray( 'none', 'upper', 3, 0, 0, AB, 1, 1, 0, BB, 1, 1, 0, X, 1, 1, 0, WORK, 1, 0 );
// info => 0
// AB => [ 1.25, 0.667, 0.4375 ]
```

The function has the following parameters:

-   **vect**: `'none'` to not form the transformation matrix X, or `'update'` to form X.
-   **uplo**: `'upper'` if upper triangle of A is stored, or `'lower'` if lower triangle is stored.
-   **N**: order of the matrices A and B.
-   **ka**: number of super/subdiagonals of A.
-   **kb**: number of super/subdiagonals of B (ka >= kb >= 0).
-   **AB**: band matrix A in band storage (ka+1 by N).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **BB**: split Cholesky factor of B from DPBSTF (kb+1 by N).
-   **strideBB1**: stride of the first dimension of `BB`.
-   **strideBB2**: stride of the second dimension of `BB`.
-   **offsetBB**: starting index for `BB`.
-   **X**: if vect='update', the N-by-N transformation matrix on exit.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **WORK**: workspace of dimension at least 2\*N.
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   B must have been previously factorized as S\*\*T\*S by `dpbstf` (split Cholesky factorization). A is overwritten by C = X\*\*T\*A\*X, where X = S\*\*(-1)\*Q and Q is orthogonal.
-   Band storage: for upper triangle, `AB(ka+1+i-j, j) = A(i,j)` for `max(1,j-ka) <= i <= j`. For lower triangle, `AB(1+i-j, j) = A(i,j)` for `j <= i <= min(n,j+ka)`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dsbgst = require( '@stdlib/lapack/base/dsbgst' );

var AB = new Float64Array( [ 0, 5, 1, 6, 0.5, 7 ] );
var BB = new Float64Array( [ 2, 3, 4 ] );
var X = new Float64Array( 1 );
var WORK = new Float64Array( 6 );

var info = dsbgst.ndarray( 'none', 'upper', 3, 1, 0, AB, 1, 2, 0, BB, 1, 1, 0, X, 1, 1, 0, WORK, 1, 0 );
console.log( 'info:', info );
console.log( 'AB:', AB );
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
