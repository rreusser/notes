# dlansf

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real symmetric matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var dlansf = require( '@stdlib/lapack/base/dlansf' );
```

#### dlansf( norm, transr, uplo, N, A, WORK )

Returns the value of a matrix norm for a real symmetric matrix stored in RFP format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 symmetric matrix in RFP format (upper, no-transpose):
var A = new Float64Array( [ 2.0, 5.0, 4.0, 1.0, 3.0, 6.0 ] );
var WORK = new Float64Array( 3 );

var result = dlansf( 'max', 'no-transpose', 'upper', 3, A, WORK );
// returns 6.0
```

The function has the following parameters:

-   **norm**: specifies the norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **transr**: specifies the RFP storage format: `'no-transpose'` or `'transpose'`.
-   **uplo**: specifies the triangle: `'upper'` or `'lower'`.
-   **N**: order of the matrix.
-   **A**: input `Float64Array` in RFP format, length `N*(N+1)/2`.
-   **WORK**: workspace `Float64Array`, length `N` (used only for `'one-norm'` and `'inf-norm'`).

#### dlansf.ndarray( norm, transr, uplo, N, A, strideA, offsetA, WORK, strideWORK, offsetWORK )

Returns the value of a matrix norm with ndarray-style stride and offset parameters.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 0.0, 2.0, 5.0, 4.0, 1.0, 3.0, 6.0 ] );
var WORK = new Float64Array( 3 );

var result = dlansf.ndarray( 'max', 'no-transpose', 'upper', 3, A, 1, 1, WORK, 1, 0 );
// returns 6.0
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   For a symmetric matrix, the one-norm and infinity-norm are equal.
-   The `'max'` norm returns the largest absolute value of any element.
-   The `'frobenius'` norm returns the square root of the sum of squares of all elements (counting off-diagonal elements twice).
-   The WORK array is only referenced when `norm` is `'one-norm'` or `'inf-norm'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlansf = require( '@stdlib/lapack/base/dlansf' );

// 3x3 symmetric matrix in RFP (upper, no-transpose):
var A = new Float64Array( [ 2.0, 5.0, 4.0, 1.0, 3.0, 6.0 ] );
var WORK = new Float64Array( 3 );

var maxNorm = dlansf( 'max', 'no-transpose', 'upper', 3, A, WORK );
console.log( 'max norm: %d', maxNorm );

WORK = new Float64Array( 3 );
var oneNorm = dlansf( 'one-norm', 'no-transpose', 'upper', 3, A, WORK );
console.log( 'one-norm: %d', oneNorm );

var frobNorm = dlansf( 'frobenius', 'no-transpose', 'upper', 3, A, WORK );
console.log( 'frobenius norm: %d', frobNorm );
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
