# dopmtr

> Overwrites a general real M-by-N matrix C with Q\*C, C\*Q, Q^T\*C, or C\*Q^T, where Q is a real orthogonal matrix from DSPTRD (packed storage).

<section class="usage">

## Usage

```javascript
var dopmtr = require( '@stdlib/lapack/base/dopmtr' );
```

#### dopmtr( side, uplo, trans, M, N, AP, TAU, C, LDC, WORK )

Overwrites the general real M-by-N matrix C with Q\*C, C\*Q, Q^T\*C, or C\*Q^T, where Q is a real orthogonal matrix defined as the product of NQ-1 elementary reflectors, as returned by [`dsptrd`][@stdlib/lapack/base/dsptrd] using packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// AP and TAU from a prior call to dsptrd('upper', 4, ...):
var AP = new Float64Array( [ 2.26, -0.09, 1.18, -0.58, 0.90, 5.56, -0.4, -0.2, 3.0, -1.0 ] );
var TAU = new Float64Array( [ 0.0, 1.50, 1.67 ] );
var C = new Float64Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ] );
var WORK = new Float64Array( 4 );

dopmtr( 'left', 'upper', 'no-transpose', 4, 4, AP, TAU, C, 4, WORK );
// C is overwritten with Q * C
```

The function has the following parameters:

-   **side**: `'left'` to apply Q or Q^T from the left, `'right'` from the right.
-   **uplo**: `'upper'` or `'lower'`, specifying whether upper or lower triangular packed storage was used in the call to dsptrd.
-   **trans**: `'no-transpose'` for Q, `'transpose'` for Q^T.
-   **M**: number of rows of C.
-   **N**: number of columns of C.
-   **AP**: packed reflector storage from dsptrd (modified temporarily, restored on exit).
-   **TAU**: scalar factors of the elementary reflectors from dsptrd.
-   **C**: input/output M-by-N matrix (column-major).
-   **LDC**: leading dimension of C.
-   **WORK**: workspace array of length N (if side='left') or M (if side='right').

#### dopmtr.ndarray( side, uplo, trans, M, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK )

Overwrites a general matrix with a transformation from the orthogonal matrix Q returned by dsptrd, with explicit stride and offset control.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 2.26, -0.09, 1.18, -0.58, 0.90, 5.56, -0.4, -0.2, 3.0, -1.0 ] );
var TAU = new Float64Array( [ 0.0, 1.50, 1.67 ] );
var C = new Float64Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ] );
var WORK = new Float64Array( 4 );

dopmtr.ndarray( 'left', 'upper', 'no-transpose', 4, 4, AP, 1, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
// C is overwritten with Q * C
```

The function has the following additional parameters:

-   **strideAP**: stride for AP.
-   **offsetAP**: starting index for AP.
-   **strideTAU**: stride for TAU.
-   **offsetTAU**: starting index for TAU.
-   **strideC1**: stride of the first dimension of C.
-   **strideC2**: stride of the second dimension of C.
-   **offsetC**: starting index for C.
-   **strideWORK**: stride for WORK.
-   **offsetWORK**: starting index for WORK.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   AP is temporarily modified during computation (diagonal elements set to 1.0 for reflector application) but is restored to its original values on exit.
-   This routine is the packed-storage analog of [`dormtr`][@stdlib/lapack/base/dormtr], which works with full-storage symmetric matrices.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dopmtr = require( '@stdlib/lapack/base/dopmtr' );

// AP and TAU from dsptrd('upper', 4, ...):
var AP = new Float64Array( [ 2.26, -0.09, 1.18, -0.58, 0.90, 5.56, -0.4, -0.2, 3.0, -1.0 ] );
var TAU = new Float64Array( [ 0.0, 1.50, 1.67 ] );

// Apply Q to 4x4 identity from the left (Q * I = Q):
var C = new Float64Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ] );
var WORK = new Float64Array( 4 );

dopmtr.ndarray( 'left', 'upper', 'no-transpose', 4, 4, AP, 1, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
// C now contains the orthogonal matrix Q
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/lapack/base/dsptrd]: https://github.com/stdlib-js/stdlib
[@stdlib/lapack/base/dormtr]: https://github.com/stdlib-js/stdlib

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
