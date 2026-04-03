# zlatdf

> Computes a contribution to the reciprocal Dif-estimate using the LU factorization computed by zgetc2.

<section class="usage">

## Usage

```javascript
var zlatdf = require( '@stdlib/lapack/base/zlatdf' );
```

#### zlatdf.ndarray( ijob, N, Z, strideZ1, strideZ2, offsetZ, RHS, strideRHS, offsetRHS, rdsum, rdscal, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV )

Computes a contribution to the reciprocal Dif-estimate using the LU factorization computed by zgetc2.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var Z = new Complex128Array( [ 4.0, 1.0, 2.0, 0.5, 3.0, -1.0, 1.0, 2.0 ] );
var RHS = new Complex128Array( [ 1.0, 0.5, -1.0, 1.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var JPIV = new Int32Array( [ 0, 1 ] );

var out = zlatdf.ndarray( 2, 2, Z, 1, 2, 0, RHS, 1, 0, 0.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
// returns { rdsum: ..., rdscal: ... }
```

The function has the following parameters:

-   **ijob**: ijob.
-   **N**: number of columns.
-   **Z**: input matrix.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **RHS**: input array.
-   **strideRHS**: stride length for `RHS`.
-   **offsetRHS**: starting index for `RHS`.
-   **rdsum**: rdsum.
-   **rdscal**: rdscal.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **JPIV**: output array.
-   **strideJPIV**: stride length for `JPIV`.
-   **offsetJPIV**: starting index for `JPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The reference Fortran ZLATDF uses `MAXDIM=2`, which causes buffer overflows in the IJOB=2 path for N>2. This JS implementation allocates workspace dynamically and handles all N correctly.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zgetc2 = require( '@stdlib/lapack/base/zgetc2' );
var zlatdf = require( '@stdlib/lapack/base/zlatdf' );

var Z = new Complex128Array( [ 4.0, 1.0, 2.0, 0.5, 3.0, -1.0, 1.0, 2.0 ] );
var IPIV = new Int32Array( 2 );
var JPIV = new Int32Array( 2 );
zgetc2.ndarray( 2, Z, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );

var RHS = new Complex128Array( [ 1.0, 0.5, -1.0, 1.0 ] );
var out = zlatdf.ndarray( 2, 2, Z, 1, 2, 0, RHS, 1, 0, 0.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
// out => { rdsum: ..., rdscal: ... }
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
