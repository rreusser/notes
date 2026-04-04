# zupgtr

> Generates an orthogonal matrix Q which is defined as the product of n-1 elementary reflectors of order n, as returned by zhptrd.

<section class="usage">

## Usage

```javascript
var zupgtr = require( '@stdlib/lapack/base/zupgtr' );
```

#### zupgtr.ndarray( uplo, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, Q, strideQ1, strideQ2, offsetQ, WORK, strideWORK, offsetWORK )

Generates an orthogonal matrix Q which is defined as the product of n-1 elementary reflectors of order n, as returned by zhptrd.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var Complex128Array = require( '@stdlib/array/complex128' );

// After calling zhptrd on a 2x2 Hermitian matrix:
var AP = new Complex128Array( 3 );
var TAU = new Complex128Array( 1 );
var Q = new Complex128Array( 4 );
var WORK = new Complex128Array( 4 );

zupgtr.ndarray( 'upper', 2, AP, 1, 0, TAU, 1, 0, Q, 1, 2, 0, WORK, 1, 0 );
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

-   AP, TAU, Q, and WORK are Complex128Array with complex-element strides.
-   Q is an N-by-N unitary matrix on output.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zupgtr = require( '@stdlib/lapack/base/zupgtr' );

var AP = new Complex128Array( 3 );
var TAU = new Complex128Array( 1 );
var Q = new Complex128Array( 4 );
var WORK = new Complex128Array( 4 );

zupgtr( 'column-major', 'upper', 2, AP, TAU, Q, 2, WORK );
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
