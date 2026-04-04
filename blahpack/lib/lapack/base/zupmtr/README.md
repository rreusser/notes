# zupmtr

> Overwrites a general complex matrix with a transformation from the unitary matrix Q returned by zhptrd.

<section class="usage">

## Usage

```javascript
var zupmtr = require( '@stdlib/lapack/base/zupmtr' );
```

#### zupmtr.ndarray( side, uplo, trans, M, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK )

Overwrites a general complex M-by-N matrix C with Q\_C, C\_Q, QH\_C, or C\_QH, where Q is a complex unitary matrix from ZHPTRD (packed storage).

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var AP = new Complex128Array( 10 );
var TAU = new Complex128Array( 3 );
var C = new Complex128Array( 16 );
var WORK = new Complex128Array( 4 );

var info = zupmtr.ndarray( 'left', 'upper', 'no-transpose', 4, 4, AP, 1, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **side**: specifies whether Q is applied from the left or the right.
-   **uplo**: specifies whether the upper or lower triangle was used in ZHPTRD.
-   **trans**: specifies whether to apply Q or Q^H.
-   **M**: number of rows of matrix C.
-   **N**: number of columns of matrix C.
-   **AP**: packed reflector storage from zhptrd (Complex128Array).
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **TAU**: scalar factors from zhptrd (Complex128Array).
-   **strideTAU**: stride length for `TAU` (in complex elements).
-   **offsetTAU**: starting index for `TAU` (in complex elements).
-   **C**: input/output M-by-N matrix (Complex128Array).
-   **strideC1**: stride of the first dimension of `C` (in complex elements).
-   **strideC2**: stride of the second dimension of `C` (in complex elements).
-   **offsetC**: starting index for `C` (in complex elements).
-   **WORK**: workspace array (Complex128Array).
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine modifies `AP` during computation but restores it before returning.
-   `WORK` must have length at least N when `side='left'` and at least M when `side='right'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zupmtr = require( '@stdlib/lapack/base/zupmtr' );

// Create a 4x4 complex identity matrix:
var C = new Complex128Array( 16 );

// Packed reflectors and tau from zhptrd:
var AP = new Complex128Array( 10 );
var TAU = new Complex128Array( 3 );
var WORK = new Complex128Array( 4 );

var info = zupmtr.ndarray( 'left', 'upper', 'no-transpose', 4, 4, AP, 1, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
console.log( 'info:', info );
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
