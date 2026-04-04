# zhptrd

> Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.

<section class="usage">

## Usage

```javascript
var zhptrd = require( '@stdlib/lapack/base/zhptrd' );
```

#### zhptrd.ndarray( uplo, N, AP, strideAP, offsetAP, d, strideD, offsetD, e, strideE, offsetE, TAU, strideTAU, offsetTAU )

Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var Complex128Array = require( '@stdlib/array/complex128' );

var N = 2;
var AP = new Complex128Array( [ 3, 0, 1, -1, 1, 0 ] ); // upper packed
var d = new Float64Array( N );
var e = new Float64Array( N - 1 );
var TAU = new Complex128Array( N - 1 );

zhptrd.ndarray( 'upper', N, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
// d => diagonal elements, e => off-diagonal elements
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **TAU**: output array.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   AP (Complex128Array) uses complex-element strides. D and E (Float64Array) use real strides.
-   On exit, AP is overwritten with the Householder reflectors and tridiagonal elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zhptrd = require( '@stdlib/lapack/base/zhptrd' );

var N = 3;
var AP = new Complex128Array([
    3, 0, 1, -1, 2, 0, 0, 0, 1, -1, 1, 0
]);
var d = new Float64Array( N );
var e = new Float64Array( N - 1 );
var TAU = new Complex128Array( N - 1 );

zhptrd( 'upper', N, AP, d, e, TAU );
console.log( 'diagonal:', d );
console.log( 'off-diagonal:', e );
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
