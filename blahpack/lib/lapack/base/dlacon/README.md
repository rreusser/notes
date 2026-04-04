# dlacon

> Estimate the 1-norm of a square real matrix using reverse communication.

<section class="usage">

## Usage

```javascript
var dlacon = require( '@stdlib/lapack/base/dlacon' );
```

#### dlacon( N, v, strideV, x, strideX, ISGN, strideISGN, EST, KASE )

Estimates the 1-norm of a square real matrix using reverse communication (strided interface).

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var v = new Float64Array( 2 );
var x = new Float64Array( 2 );
var ISGN = new Int32Array( 2 );
var EST = new Float64Array( 1 );
var KASE = new Int32Array( 1 );

dlacon( 2, v, 1, x, 1, ISGN, 1, EST, KASE );
```

#### dlacon.ndarray( N, v, strideV, offsetV, x, strideX, offsetX, ISGN, strideISGN, offsetISGN, EST, KASE )

Estimates the 1-norm of a square real matrix using reverse communication (ndarray interface).

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var v = new Float64Array( 2 );
var x = new Float64Array( 2 );
var ISGN = new Int32Array( 2 );
var EST = new Float64Array( 1 );
var KASE = new Int32Array( 1 );

dlacon.ndarray( 2, v, 1, 0, x, 1, 0, ISGN, 1, 0, EST, KASE );
```

The function has the following parameters:

-   **N**: order of the matrix (N >= 1).
-   **v**: [`Float64Array`][mdn-float64array] workspace array of length N.
-   **strideV**: stride length for `v`.
-   **offsetV**: starting index for `v` (ndarray only).
-   **x**: [`Float64Array`][mdn-float64array] input/output vector of length N.
-   **strideX**: stride length for `x`.
-   **offsetX**: starting index for `x` (ndarray only).
-   **ISGN**: [`Int32Array`][mdn-int32array] sign array of length N.
-   **strideISGN**: stride length for `ISGN`.
-   **offsetISGN**: starting index for `ISGN` (ndarray only).
-   **EST**: [`Float64Array`][mdn-float64array] single-element array. On exit, `EST[0]` holds the estimated 1-norm.
-   **KASE**: [`Int32Array`][mdn-int32array] single-element array. Set `KASE[0] = 0` on the first call. On return, `KASE[0] = 1` means compute `x = A*x`; `KASE[0] = 2` means compute `x = A^T*x`; `KASE[0] = 0` means the estimate is complete.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   This routine uses a reverse communication interface. The caller must repeatedly call `dlacon` in a loop, performing the matrix-vector product indicated by `KASE[0]` between calls.
-   Unlike [`dlacn2`][@stdlib/lapack/base/dlacn2], this routine uses module-level persistent state (equivalent to Fortran `SAVE`) instead of an `ISAVE` array. This means the routine is **not reentrant** -- only one norm estimation can be in progress at a time.
-   This is the older interface; prefer [`dlacn2`][@stdlib/lapack/base/dlacn2] for new code.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlacon = require( '@stdlib/lapack/base/dlacon' );

// Estimate 1-norm of a 3x3 identity matrix:
var n = 3;
var v = new Float64Array( n );
var x = new Float64Array( n );
var ISGN = new Int32Array( n );
var EST = new Float64Array( 1 );
var KASE = new Int32Array( 1 );

KASE[ 0 ] = 0;
while ( true ) {
    dlacon.ndarray( n, v, 1, 0, x, 1, 0, ISGN, 1, 0, EST, KASE );
    if ( KASE[ 0 ] === 0 ) break;
    // Identity: A*x = x, so x is unchanged
}
console.log( 'Estimated 1-norm:', EST[ 0 ] );
// => 'Estimated 1-norm: 1'
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
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[@stdlib/lapack/base/dlacn2]: https://github.com/stdlib-js/stdlib

</section>

<!-- /.links -->
