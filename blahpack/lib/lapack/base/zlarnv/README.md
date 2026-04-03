# zlarnv

> Returns a vector of complex random numbers from a uniform or normal distribution.

<section class="usage">

## Usage

```javascript
var zlarnv = require( '@stdlib/lapack/base/zlarnv' );
```

#### zlarnv.ndarray( idist, iseed, strideISEED, offsetISEED, N, x, stride, offset )

Returns a vector of `N` complex random numbers from a uniform or normal distribution, controlled by the `idist` parameter.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var iseed = new Int32Array( [ 1, 2, 3, 4 ] );
var x = new Complex128Array( 5 );

zlarnv.ndarray( 1, iseed, 1, 0, 5, x, 1, 0 );
// x now contains 5 complex random numbers with real/imag parts uniform in (0,1)
```

The function has the following parameters:

-   **idist**: distribution type. `1` = uniform real/imag in (0,1), `2` = uniform real/imag in (-1,1), `3` = normal(0,1) on real and imaginary parts, `4` = uniform on the unit disc, `5` = uniform on the unit circle.
-   **iseed**: `Int32Array` seed array of 4 integers.
-   **strideISEED**: stride length for `iseed`.
-   **offsetISEED**: starting index for `iseed`.
-   **N**: number of complex random numbers to generate.
-   **x**: `Complex128Array` output array.
-   **stride**: stride length for `x` (in complex elements).
-   **offset**: starting index for `x` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The underlying random number generator is `dlaruv`, which generates uniform real numbers in (0,1). These are combined in pairs to form complex numbers according to the chosen distribution.
-   For `idist=3`, the Box-Muller transform is used to generate normal(0,1) variates for both the real and imaginary parts.
-   For `idist=4` and `idist=5`, polar form is used: `sqrt(u1) * exp(i * 2*pi*u2)` and `exp(i * 2*pi*u2)` respectively.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarnv = require( '@stdlib/lapack/base/zlarnv' );

var iseed = new Int32Array( [ 1, 2, 3, 4 ] );
var x = new Complex128Array( 5 );

zlarnv.ndarray( 1, iseed, 1, 0, 5, x, 1, 0 );

var xv = reinterpret( x, 0 );
console.log( xv );
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
