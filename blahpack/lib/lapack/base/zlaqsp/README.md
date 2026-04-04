# zlaqsp

> Equilibrate a complex symmetric matrix in packed storage using scaling factors.

<section class="usage">

## Usage

```javascript
var zlaqsp = require( '@stdlib/lapack/base/zlaqsp' );
```

#### zlaqsp( uplo, N, AP, S, strideS, scond, amax )

Equilibrates a complex symmetric matrix `A` in packed storage using the scaling factors in vector `S`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 9.0, 2.0, 0.5, 0.25, 2.0, 1.0, 16.0, 3.0 ] );
var S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );

var equed = zlaqsp( 'upper', 3, AP, S, 1, 0.05, 16.0 );
// returns 'yes'
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: packed symmetric matrix of length `N*(N+1)/2` stored as a [`Complex128Array`][mdn-typed-array].
-   **S**: real scaling factors of length `N` stored as a [`Float64Array`][mdn-float64array].
-   **strideS**: stride length for `S`.
-   **scond**: ratio of the smallest to largest scaling factor.
-   **amax**: absolute value of the largest matrix element.

The function returns a string indicating whether equilibration was performed:

-   `'none'`: no equilibration was needed.
-   `'yes'`: equilibration was done (i.e., `A` was replaced by `diag(S) * A * diag(S)`).

#### zlaqsp.ndarray( uplo, N, AP, strideAP, offsetAP, S, strideS, offsetS, scond, amax )

Equilibrates a complex symmetric matrix `A` in packed storage using the scaling factors in vector `S`, with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 9.0, 2.0, 0.5, 0.25, 2.0, 1.0, 16.0, 3.0 ] );
var S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );

var equed = zlaqsp.ndarray( 'upper', 3, AP, 1, 0, S, 1, 0, 0.05, 16.0 );
// returns 'yes'
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (complex elements).
-   **offsetAP**: starting index for `AP` (complex elements).
-   **offsetS**: starting index for `S`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Equilibration scales the matrix so that `A(i,j)` becomes `S(i) * A(i,j) * S(j)`. This is only done when the scaling factors are poorly conditioned (`scond` is small) or the matrix elements are very large or very small.
-   The packed storage format stores only the upper or lower triangle of the symmetric matrix in a one-dimensional array of length `N*(N+1)/2`.
-   The scaling factors `S` are real-valued. Both the real and imaginary parts of each complex element are multiplied by the same real scalar `S(i)*S(j)`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlaqsp = require( '@stdlib/lapack/base/zlaqsp' );

// 3x3 complex symmetric matrix, upper packed:
var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 9.0, 2.0, 0.5, 0.25, 2.0, 1.0, 16.0, 3.0 ] );
var S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );

var equed = zlaqsp( 'upper', 3, AP, S, 1, 0.05, 16.0 );
console.log( 'equed:', equed );
// => 'yes'
console.log( 'AP:', reinterpret( AP, 0 ) );
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
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
