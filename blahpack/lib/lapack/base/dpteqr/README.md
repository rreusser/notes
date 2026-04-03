# dpteqr

> Computes all eigenvalues and optionally eigenvectors of a real symmetric positive definite tridiagonal matrix.

<section class="usage">

## Usage

```javascript
var dpteqr = require( '@stdlib/lapack/base/dpteqr' );
```

#### dpteqr( order, compz, N, d, strideD, e, strideE, Z, LDZ, WORK, strideWORK )

Computes all eigenvalues and, optionally, eigenvectors of a real symmetric positive definite tridiagonal matrix by first factoring using `dpttrf` and then calling `dbdsqr`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 SPD tridiagonal matrix:
// [ 4  1  0 ]
// [ 1  4  1 ]
// [ 0  1  4 ]
var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0 ] );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 12 );

var info = dpteqr( 'column-major', 'initialize', 3, d, 1, e, 1, Z, 3, WORK, 1 );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout ('row-major' or 'column-major').
-   **compz**: specifies whether eigenvectors are computed ('none', 'initialize', or 'update').
-   **N**: order of the matrix.
-   **d**: diagonal elements as a [`Float64Array`][mdn-float64array] (length N).
-   **strideD**: stride length for `d`.
-   **e**: subdiagonal elements as a [`Float64Array`][mdn-float64array] (length N-1).
-   **strideE**: stride length for `e`.
-   **Z**: eigenvector matrix as a [`Float64Array`][mdn-float64array] (N-by-N).
-   **LDZ**: leading dimension of `Z`.
-   **WORK**: workspace as a [`Float64Array`][mdn-float64array] (length >= 4\*N).
-   **strideWORK**: stride length for `WORK`.

#### dpteqr.ndarray( compz, N, d, strideD, offsetD, e, strideE, offsetE, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK )

Computes all eigenvalues and, optionally, eigenvectors using an alternative interface with stride and offset parameters.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0 ] );
var Z = new Float64Array( 9 );
var WORK = new Float64Array( 12 );

var info = dpteqr.ndarray( 'initialize', 3, d, 1, 0, e, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **compz**: specifies whether eigenvectors are computed ('none', 'initialize', or 'update').
-   **N**: order of the matrix.
-   **d**: diagonal elements as a [`Float64Array`][mdn-float64array].
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: subdiagonal elements as a [`Float64Array`][mdn-float64array].
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **Z**: eigenvector matrix as a [`Float64Array`][mdn-float64array].
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: workspace as a [`Float64Array`][mdn-float64array].
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The eigenvalues are returned in descending order in `d`.
-   When `compz` is `'initialize'`, Z is initialized to the identity matrix internally and eigenvectors of the tridiagonal matrix are computed.
-   When `compz` is `'update'`, Z must contain an orthogonal matrix on entry; eigenvectors of the original symmetric matrix are computed.
-   When `compz` is `'none'`, only eigenvalues are computed and Z is not referenced.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpteqr = require( '@stdlib/lapack/base/dpteqr' );

var d = new Float64Array( [ 4.0, 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0, 1.0 ] );
var Z = new Float64Array( 16 );
var WORK = new Float64Array( 16 );

var info = dpteqr( 'column-major', 'initialize', 4, d, 1, e, 1, Z, 4, WORK, 1 );
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
