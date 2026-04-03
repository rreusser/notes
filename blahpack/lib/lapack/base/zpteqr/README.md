# zpteqr

> Computes all eigenvalues and optionally eigenvectors of a complex Hermitian positive definite tridiagonal matrix.

<section class="usage">

## Usage

```javascript
var zpteqr = require( '@stdlib/lapack/base/zpteqr' );
```

#### zpteqr( order, compz, N, d, strideD, e, strideE, Z, LDZ, WORK, strideWORK )

Computes all eigenvalues and optionally eigenvectors of a complex Hermitian positive definite tridiagonal matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );

var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0 ] );
var Z = new Complex128Array( 9 );
var WORK = new Float64Array( 12 );

var info = zpteqr( 'column-major', 'initialize', 3, d, 1, e, 1, Z, 3, WORK, 1 );
// info => 0
```

#### zpteqr.ndarray( compz, N, d, strideD, offsetD, e, strideE, offsetE, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK )

Computes all eigenvalues and optionally eigenvectors using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );

var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0 ] );
var Z = new Complex128Array( 9 );
var WORK = new Float64Array( 12 );

var info = zpteqr.ndarray( 'initialize', 3, d, 1, 0, e, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **compz**: specifies whether eigenvectors are computed (`'none'`, `'initialize'`, or `'update'`).
-   **N**: order of the matrix.
-   **d**: diagonal elements of the tridiagonal matrix (real, length N).
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: subdiagonal elements of the tridiagonal matrix (real, length N-1).
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **Z**: unitary matrix (Complex128Array, N-by-N).
-   **strideZ1**: stride of the first dimension of `Z` (complex elements).
-   **strideZ2**: stride of the second dimension of `Z` (complex elements).
-   **offsetZ**: starting index for `Z` (complex elements).
-   **WORK**: real workspace array (length >= 4\*N).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `compz = 'none'`: compute eigenvalues only (Z is not referenced).
-   `compz = 'initialize'`: compute eigenvalues and eigenvectors of the tridiagonal matrix. Z is initialized to the identity matrix.
-   `compz = 'update'`: compute eigenvalues and eigenvectors. On entry, Z must contain the unitary matrix used to reduce the original matrix to tridiagonal form.
-   On exit, if info = 0, D contains the eigenvalues in descending order.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zpteqr = require( '@stdlib/lapack/base/zpteqr' );

var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0 ] );
var Z = new Complex128Array( 9 );
var WORK = new Float64Array( 12 );

var info = zpteqr( 'column-major', 'initialize', 3, d, 1, e, 1, Z, 3, WORK, 1 );

console.log( 'info:', info );
console.log( 'eigenvalues:', d );
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
