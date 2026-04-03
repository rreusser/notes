# zlargv

> Generates a vector of complex plane rotations with real cosines and complex sines.

<section class="usage">

## Usage

```javascript
var zlargv = require( '@stdlib/lapack/base/zlargv' );
```

#### zlargv.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, c, strideC, offsetC )

Generates a vector of complex plane rotations with real cosines and complex sines.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var x = new Complex128Array( [ 3.0, 1.0, 0.0, 0.0 ] );
var y = new Complex128Array( [ 4.0, 0.0, 5.0, 0.0 ] );
var c = new Float64Array( 2 );

zlargv.ndarray( 2, x, 1, 0, y, 1, 0, c, 1, 0 );
// x and y are modified in-place, c contains cosines
```

The function has the following parameters:

-   **N**: number of plane rotations to generate.
-   **x**: input/output `Complex128Array` vector (overwritten by `r` on exit).
-   **strideX**: stride for `x` (in complex elements).
-   **offsetX**: starting index for `x` (in complex elements).
-   **y**: input/output `Complex128Array` vector (overwritten by complex sines on exit).
-   **strideY**: stride for `y` (in complex elements).
-   **offsetY**: starting index for `y` (in complex elements).
-   **c**: output `Float64Array` for real cosines.
-   **strideC**: stride for `c`.
-   **offsetC**: starting index for `c`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   For each pair `(x(i), y(i))`, the routine computes a plane rotation with real cosine `c(i)` and complex sine `s(i)` such that the rotation zeros out `y(i)`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlargv = require( '@stdlib/lapack/base/zlargv' );

var x = new Complex128Array( [ 3.0, 1.0, 1.0, 2.0 ] );
var y = new Complex128Array( [ 4.0, 0.0, 3.0, 1.0 ] );
var c = new Float64Array( 2 );

zlargv.ndarray( 2, x, 1, 0, y, 1, 0, c, 1, 0 );

console.log( 'cosines:', c );
console.log( 'x (r):', reinterpret( x, 0 ) );
console.log( 'y (sines):', reinterpret( y, 0 ) );
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
