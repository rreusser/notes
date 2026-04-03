# dsptrd

> Reduce a real symmetric matrix in packed storage to tridiagonal form.

<section class="usage">

## Usage

```javascript
var dsptrd = require( '@stdlib/lapack/base/dsptrd' );
```

#### dsptrd( uplo, N, AP, d, e, TAU )

Reduces a real symmetric matrix `A` stored in packed form to symmetric tridiagonal form `T` by an orthogonal similarity transformation: `Q^T * A * Q = T`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 symmetric matrix [[1,2],[2,3]] in upper packed storage:
var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var d = new Float64Array( 2 );
var e = new Float64Array( 1 );
var TAU = new Float64Array( 1 );

dsptrd( 'upper', 2, AP, d, e, TAU );
// d => [ diagonal elements of T ]
// e => [ off-diagonal elements of T ]
```

#### dsptrd.ndarray( uplo, N, AP, strideAP, offsetAP, d, strideD, offsetD, e, strideE, offsetE, TAU, strideTAU, offsetTAU )

Reduces a real symmetric matrix in packed storage to tridiagonal form using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var d = new Float64Array( 2 );
var e = new Float64Array( 1 );
var TAU = new Float64Array( 1 );

dsptrd.ndarray( 'upper', 2, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle of `A` is stored.
-   **N**: order of the matrix `A`.
-   **AP**: packed symmetric matrix of size `N*(N+1)/2`.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **d**: output array for diagonal elements of `T` (length `N`).
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: output array for off-diagonal elements of `T` (length `N-1`).
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **TAU**: output array for scalar factors of the reflectors (length `N-1`).
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On exit, `AP` is overwritten with the tridiagonal matrix and the Householder reflectors.
-   The diagonal and off-diagonal of `T` are stored in `d` and `e`.
-   The reflector scalars are stored in `TAU`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dsptrd = require( '@stdlib/lapack/base/dsptrd' );

// 3x3 symmetric matrix in upper packed storage:
//   [ 2  3  1 ]
//   [ 3  5  4 ]
//   [ 1  4  8 ]
// Upper packed (column-major): 2, 3, 5, 1, 4, 8
var AP = new Float64Array( [ 2.0, 3.0, 5.0, 1.0, 4.0, 8.0 ] );
var d = new Float64Array( 3 );
var e = new Float64Array( 2 );
var tau = new Float64Array( 2 );

var info = dsptrd.ndarray( 'upper', 3, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );

console.log( 'info:', info );
console.log( 'd (diagonal):', d );
console.log( 'e (off-diagonal):', e );
console.log( 'tau (reflector scalars):', tau );
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
