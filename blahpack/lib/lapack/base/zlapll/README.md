# zlapll

> Measures the linear dependence of two vectors.

<section class="usage">

## Usage

```javascript
var zlapll = require( '@stdlib/lapack/base/zlapll' );
```

#### zlapll( N, x, strideX, y, strideY, ssmin )

Measures the linear dependence of two complex vectors X and Y by computing the QR factorization of the N-by-2 matrix `(X Y)` and returning the smallest singular value of the resulting 2-by-2 upper triangular R factor.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
var y = new Complex128Array( [ 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0 ] );
var ssmin = new Float64Array( 1 );

zlapll( 4, x, 1, y, 1, ssmin );
// ssmin[ 0 ] ~ 0.0
```

The function has the following parameters:

-   **N**: length of the vectors.
-   **x**: first complex vector (`Complex128Array`), overwritten on exit.
-   **strideX**: stride for `x` (in complex elements).
-   **y**: second complex vector (`Complex128Array`), overwritten on exit.
-   **strideY**: stride for `y` (in complex elements).
-   **ssmin**: output `Float64Array` of length 1; `ssmin[0]` receives the smallest singular value.

#### zlapll.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, ssmin )

Measures the linear dependence of two complex vectors with stride and offset parameters.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var x = new Complex128Array( [ 1.0, 2.0, 3.0, -1.0, 0.5, 4.0 ] );
var y = new Complex128Array( [ 2.0, -3.0, -1.0, 5.0, 4.0, 0.5 ] );
var ssmin = new Float64Array( 1 );

zlapll.ndarray( 3, x, 1, 0, y, 1, 0, ssmin );
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On exit, the input vectors `x` and `y` are overwritten.
-   A small `ssmin` value indicates that the two vectors are nearly linearly dependent.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlapll = require( '@stdlib/lapack/base/zlapll' );

// Parallel vectors (linearly dependent):
var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
var y = new Complex128Array( [ 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0 ] );
var ssmin = new Float64Array( 1 );

zlapll( 4, x, 1, y, 1, ssmin );
// ssmin[ 0 ] ~ 0.0 (vectors are linearly dependent)

// Independent vectors:
x = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
y = new Complex128Array( [ 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 ] );

zlapll( 3, x, 1, y, 1, ssmin );
// ssmin[ 0 ] = 1.0 (vectors are orthogonal)
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
