# dlapll

> Measure the linear dependence of two vectors.

Given two column vectors X and Y, let `A = ( X Y )`. The routine first computes the QR factorization of `A = Q*R`, then computes the SVD of the 2-by-2 upper triangular matrix R. The smaller singular value of R is returned in SSMIN, which serves as a measurement of the linear dependency of the vectors X and Y.

<section class="usage">

## Usage

```javascript
var dlapll = require( '@stdlib/lapack/base/dlapll' );
```

#### dlapll( N, x, strideX, y, strideY, ssmin )

Measures the linear dependence of two vectors by computing the smallest singular value of the N-by-2 matrix `A = ( X Y )`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var y = new Float64Array( [ 2.0, 4.0, 6.0, 8.0 ] );
var ssmin = new Float64Array( 1 );

dlapll( 4, x, 1, y, 1, ssmin );
// ssmin[ 0 ] ~ 0.0 (vectors are linearly dependent)
```

The function has the following parameters:

-   **N**: length of the vectors X and Y.
-   **x**: first input vector (overwritten on exit).
-   **strideX**: stride for `x`.
-   **y**: second input vector (overwritten on exit).
-   **strideY**: stride for `y`.
-   **ssmin**: output [`Float64Array`][mdn-float64array] of length 1; on exit, `ssmin[0]` contains the smallest singular value.

#### dlapll.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, ssmin )

Measures linear dependence using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var x = new Float64Array( [ 0.0, 1.0, 2.0, 3.0 ] );
var y = new Float64Array( [ 0.0, 4.0, 5.0, 6.0 ] );
var ssmin = new Float64Array( 1 );

dlapll.ndarray( 3, x, 1, 1, y, 1, 1, ssmin );
// ssmin[ 0 ] contains the smallest singular value
```

The function has the following additional parameters:

-   **offsetX**: starting index for `x`.
-   **offsetY**: starting index for `y`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Both `x` and `y` are overwritten on exit.
-   A small value of `ssmin[0]` indicates that the vectors are nearly linearly dependent.
-   If `N <= 1`, `ssmin[0]` is set to zero.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlapll = require( '@stdlib/lapack/base/dlapll' );

// Parallel vectors (linearly dependent):
var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var y = new Float64Array( [ 2.0, 4.0, 6.0, 8.0 ] );
var ssmin = new Float64Array( 1 );

dlapll( 4, x, 1, y, 1, ssmin );
console.log( 'Parallel vectors, ssmin:', ssmin[ 0 ] );
// => ~0.0

// Orthogonal vectors (linearly independent):
x = new Float64Array( [ 1.0, 0.0, 0.0 ] );
y = new Float64Array( [ 0.0, 1.0, 0.0 ] );
ssmin = new Float64Array( 1 );

dlapll( 3, x, 1, y, 1, ssmin );
console.log( 'Orthogonal vectors, ssmin:', ssmin[ 0 ] );
// => 1.0
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

</section>

<!-- /.links -->
