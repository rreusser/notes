# zrscl

> Multiplies a complex vector by the reciprocal of a complex scalar.

<section class="usage">

## Usage

```javascript
var zrscl = require( '@stdlib/lapack/base/zrscl' );
```

#### zrscl.ndarray( N, a, x, strideX, offsetX )

Multiplies an n-element complex vector `x` by the complex scalar `1/a`, performing the scaling carefully to avoid overflow or underflow.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );

var x = new Complex128Array( [ 6.0, 8.0, 12.0, 16.0 ] );
var a = new Complex128( 2.0, 0.0 );

zrscl.ndarray( 2, a, x, 1, 0 );
// x => < 3.0, 4.0, 6.0, 8.0 >
```

The function has the following parameters:

-   **N**: number of elements in `x`.
-   **a**: complex scalar divisor (`Complex128`).
-   **x**: input/output complex vector (`Complex128Array`).
-   **strideX**: stride for `x` (in complex elements).
-   **offsetX**: starting index for `x` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine handles all combinations of real, imaginary, and general complex divisors with separate overflow/underflow protection for each case.
-   When `a` is purely real, the computation delegates to `zdrscl`. When `a` is purely imaginary or general complex, safe scaling via `zdscal` and `zscal` is applied.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zrscl = require( '@stdlib/lapack/base/zrscl' );

var x = new Complex128Array( [ 6.0, 8.0, 12.0, 16.0 ] );
var a = new Complex128( 2.0, 0.0 );

zrscl.ndarray( 2, a, x, 1, 0 );

var view = reinterpret( x, 0 );
console.log( view );
// => Float64Array [ 3.0, 4.0, 6.0, 8.0 ]
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
