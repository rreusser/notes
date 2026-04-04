# dopgtr

> Generates a real orthogonal matrix Q from the elementary reflectors returned by dsptrd.

<section class="usage">

## Usage

```javascript
var dopgtr = require( '@stdlib/lapack/base/dopgtr' );
```

#### dopgtr.ndarray( uplo, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, Q, strideQ1, strideQ2, offsetQ, WORK, strideWORK, offsetWORK )

Generates a real orthogonal matrix Q from the elementary reflectors returned by dsptrd.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dopgtr = require( '@stdlib/lapack/base/dopgtr' );

// 3x3 symmetric matrix, upper packed after dsptrd:
var AP = new Float64Array( [ 5.3, -0.1, 1.7, -0.72, 3.16, 4.0 ] );
var TAU = new Float64Array( [ 0.0, 1.316 ] );
var Q = new Float64Array( 9 );
var WORK = new Float64Array( 3 );

var info = dopgtr.ndarray( 'upper', 3, AP, 1, 0, TAU, 1, 0, Q, 1, 3, 0, WORK, 1, 0 );
// info => 0
// Q => < Float64Array >
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **Q**: input matrix.
-   **strideQ1**: stride of the first dimension of `Q`.
-   **strideQ2**: stride of the second dimension of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   DOPGTR generates the orthogonal matrix Q from the elementary reflectors returned by [`dsptrd`][@stdlib/lapack/base/dsptrd] (packed storage tridiagonal reduction).
-   If `uplo` is `'upper'`, the reflectors are applied via [`dorg2l`][@stdlib/lapack/base/dorg2l]. If `uplo` is `'lower'`, they are applied via [`dorg2r`][@stdlib/lapack/base/dorg2r].
-   The output matrix Q is N-by-N and orthogonal (Q^T * Q = I).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dopgtr = require( '@stdlib/lapack/base/dopgtr' );

// 4x4 symmetric matrix, upper packed after dsptrd:
var N = 4;
var AP = new Float64Array( [
    2.26, -0.092, 1.183, -0.580, 0.896, 5.556, -0.4, -0.2, 3.0, -1.0
] );
var TAU = new Float64Array( [ 0.0, 1.496, 1.667 ] );
var Q = new Float64Array( N * N );
var WORK = new Float64Array( N );

var info = dopgtr.ndarray( 'upper', N, AP, 1, 0, TAU, 1, 0, Q, 1, N, 0, WORK, 1, 0 );
console.log( 'info:', info );
console.log( 'Q:', Q );
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
