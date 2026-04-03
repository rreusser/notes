# zlarfx

> Applies an elementary reflector to a general rectangular matrix with loop unrolling when the reflector has order at most 10.

<section class="usage">

## Usage

```javascript
var zlarfx = require( '@stdlib/lapack/base/zlarfx' );
```

#### zlarfx.ndarray( side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK )

Applies an elementary reflector H to a complex M-by-N matrix C, from either the left or the right, with loop unrolling for reflector order at most 10.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );

var v = new Complex128Array( [ 1.0, 0.0, 0.5, 0.3 ] );
var tau = new Complex128( 1.6, -0.2 );
var C = new Complex128Array( [ 1.0, 2.0, 4.0, -1.0, 2.0, 0.5, 5.0, 3.0 ] );
var WORK = new Complex128Array( 10 );

zlarfx.ndarray( 'left', 2, 2, v, 1, 0, tau, C, 1, 2, 0, WORK, 1, 0 );
// C is modified in-place
```

The function has the following parameters:

-   **side**: `'left'` or `'right'`, specifying whether H is applied from the left or right.
-   **M**: number of rows of C.
-   **N**: number of columns of C.
-   **v**: `Complex128Array` containing the reflector vector.
-   **strideV**: stride for `v` (in complex elements).
-   **offsetV**: starting index for `v` (in complex elements).
-   **tau**: `Complex128` scalar tau.
-   **C**: `Complex128Array` containing the M-by-N matrix, modified in-place.
-   **strideC1**: stride of the first dimension of `C` (in complex elements).
-   **strideC2**: stride of the second dimension of `C` (in complex elements).
-   **offsetC**: starting index for `C` (in complex elements).
-   **WORK**: `Complex128Array` workspace (length N if side=`'left'`, length M if side=`'right'`).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The reflector H is represented as `H = I - tau * v * v^H`, where tau is a complex scalar and v is a complex vector.
-   If tau = 0, the function returns immediately (H is the identity).
-   For reflector order 1 through 10, inline code is used for performance. For order > 10, the routine falls back to `zlarf`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarfx = require( '@stdlib/lapack/base/zlarfx' );

var v = new Complex128Array( [ 1.0, 0.0, 0.5, 0.3 ] );
var tau = new Complex128( 1.6, -0.2 );
var C = new Complex128Array( [
    1.0, 2.0, 4.0, -1.0,
    2.0, 0.5, 5.0, 3.0,
    3.0, -1.0, 6.0, 0.0
] );
var WORK = new Complex128Array( 10 );

zlarfx.ndarray( 'left', 2, 3, v, 1, 0, tau, C, 1, 2, 0, WORK, 1, 0 );

var view = reinterpret( C, 0 );
console.log( view );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

</section>

<!-- /.links -->
