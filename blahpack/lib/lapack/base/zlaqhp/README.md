# zlaqhp

> Equilibrate a complex Hermitian matrix in packed storage using scaling factors.

<section class="usage">

## Usage

```javascript
var zlaqhp = require( '@stdlib/lapack/base/zlaqhp' );
```

#### zlaqhp( uplo, N, AP, S, strideS, scond, amax )

Equilibrates a Hermitian matrix `A` in packed storage using the scaling factors in the vector `S`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// Packed upper 2x2 Hermitian: A(1,1), A(1,2), A(2,2)
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 9.0, 0.0 ] );
var S = new Float64Array( [ 0.5, 0.25 ] );

var equed = zlaqhp( 'upper', 2, AP, S, 1, 0.05, 9.0 );
// returns 'yes'
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: packed Hermitian matrix as a [`Complex128Array`][@stdlib/array/complex128], length `N*(N+1)/2`.
-   **S**: scaling factors as a [`Float64Array`][mdn-float64array], length `N`.
-   **strideS**: stride for `S`.
-   **scond**: ratio of smallest to largest scaling factor.
-   **amax**: absolute value of largest matrix element.

The function returns a string indicating whether equilibration was performed:

-   `'none'`: no equilibration was needed.
-   `'yes'`: equilibration was done.

#### zlaqhp.ndarray( uplo, N, AP, strideAP, offsetAP, S, strideS, offsetS, scond, amax )

Equilibrates a Hermitian matrix `A` in packed storage using the scaling factors in the vector `S`, with alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 9.0, 0.0 ] );
var S = new Float64Array( [ 0.5, 0.25 ] );

var equed = zlaqhp.ndarray( 'upper', 2, AP, 1, 0, S, 1, 0, 0.05, 9.0 );
// returns 'yes'
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **offsetS**: starting index for `S`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Diagonal elements of a Hermitian matrix are real. After scaling, the diagonal imaginary parts are set to zero: `A(j,j) = S(j)^2 * real(A(j,j))`.
-   Equilibration is skipped when `scond >= 0.1` and `amax` is within the safe range `[SMALL, LARGE]`, where `SMALL = safe_minimum / epsilon` and `LARGE = 1 / SMALL`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqhp = require( '@stdlib/lapack/base/zlaqhp' );

// 3x3 Hermitian packed upper:
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 9.0, 0.0, 0.5, -1.0, 2.0, 0.5, 16.0, 0.0 ] );
var S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );

var equed = zlaqhp( 'upper', 3, AP, S, 1, 0.05, 16.0 );
console.log( 'equed:', equed );
// => equed: yes
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

</section>

<!-- /.links -->
