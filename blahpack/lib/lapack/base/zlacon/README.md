# zlacon

> Estimates the 1-norm of a square complex matrix using reverse communication.

<section class="usage">

## Usage

```javascript
var zlacon = require( '@stdlib/lapack/base/zlacon' );
```

#### zlacon.ndarray( N, V, strideV, offsetV, X, strideX, offsetX, EST, KASE )

Estimates the 1-norm of a square complex matrix A using reverse communication. This is the older version of `zlacn2` -- same algorithm but with module-level persistent state instead of an explicit ISAVE array.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var N = 3;
var V = new Complex128Array( N );
var X = new Complex128Array( N );
var EST = new Float64Array( 1 );
var KASE = new Int32Array( 1 );

KASE[ 0 ] = 0;
zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
// KASE[ 0 ] => 1 (caller should compute X = A*X and call again)
```

The function has the following parameters:

-   **N**: order of the matrix (N >= 1).
-   **V**: [`Complex128Array`][@stdlib/array/complex128] workspace vector of length N.
-   **strideV**: stride for V (in complex elements).
-   **offsetV**: starting index for V (in complex elements).
-   **X**: [`Complex128Array`][@stdlib/array/complex128] input/output vector of length N.
-   **strideX**: stride for X (in complex elements).
-   **offsetX**: starting index for X (in complex elements).
-   **EST**: [`Float64Array`][mdn-float64array] of length 1. On exit, `EST[0]` holds the estimated 1-norm.
-   **KASE**: [`Int32Array`][mdn-int32array] of length 1. Set to 0 on first call; on intermediate returns, 1 means compute `X = A*X`, 2 means compute `X = A**H * X`; on final return, 0 means the estimate is complete.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Unlike `zlacn2`, this routine uses module-level persistent state (equivalent to Fortran SAVE) instead of an explicit ISAVE array. This means it is **not** safe for concurrent or interleaved use with multiple matrices. Prefer `zlacn2` for new code.
-   The routine implements the algorithm of Higham (1988) for estimating the 1-norm of a square complex matrix using reverse communication.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlacon = require( '@stdlib/lapack/base/zlacon' );

var N = 3;
var V = new Complex128Array( N );
var X = new Complex128Array( N );
var EST = new Float64Array( 1 );
var KASE = new Int32Array( 1 );

KASE[ 0 ] = 0;

// First call initializes X and requests A*X (KASE=1):
zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
console.log( 'After init: KASE =', KASE[ 0 ] ); // 1
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
