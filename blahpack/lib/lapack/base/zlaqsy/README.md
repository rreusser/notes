# zlaqsy

> Equilibrate a symmetric matrix using the scaling factors computed by zpoequ.

<section class="usage">

## Usage

```javascript
var zlaqsy = require( '@stdlib/lapack/base/zlaqsy' );
```

#### zlaqsy.ndarray( uplo, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, scond, amax )

Equilibrates a symmetric matrix `A` using the scaling factors in the vector `s`. Sets `A[i,j] = s[i] * A[i,j] * s[j]` when the matrix is poorly scaled. Returns `'yes'` if equilibration was performed, or `'none'` if it was not needed.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 complex symmetric matrix (column-major):
var A = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 1.0, 0.5, 9.0, 2.0 ] );
var s = new Float64Array( [ 0.5, 0.25 ] );

var equed = zlaqsy.ndarray( 'upper', 2, A, 1, 2, 0, s, 1, 0, 0.05, 9.0 );
// returns 'yes'
```

The function has the following parameters:

-   **uplo**: specifies whether to use the upper or lower triangle (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **A**: input/output N-by-N complex symmetric matrix stored as a `Complex128Array`.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **s**: `Float64Array` of scaling factors, length `N`.
-   **strideS**: stride length for `s`.
-   **offsetS**: starting index for `s`.
-   **scond**: ratio of the smallest to the largest scaling factor. If `scond >= 0.1`, no scaling is done.
-   **amax**: absolute value of the largest matrix element. If `amax` is in the safe range, no scaling is done.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine only modifies the triangle specified by `uplo`. The other triangle is not referenced.
-   The scaling factors `s` are real (not complex) and are typically computed by `zpoequ`.
-   The decision to equilibrate is based on the condition number ratio `scond` and the magnitude `amax` relative to machine precision thresholds.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqsy = require( '@stdlib/lapack/base/zlaqsy' );

// 2x2 complex symmetric matrix (column-major):
var A = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 1.0, 0.5, 9.0, 2.0 ] );
var s = new Float64Array( [ 0.5, 0.25 ] );

var equed = zlaqsy.ndarray( 'upper', 2, A, 1, 2, 0, s, 1, 0, 0.05, 9.0 );
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

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
