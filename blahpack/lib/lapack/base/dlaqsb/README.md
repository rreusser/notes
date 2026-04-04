# dlaqsb

> Equilibrate a symmetric band matrix using scaling factors.

<section class="usage">

## Usage

```javascript
var dlaqsb = require( '@stdlib/lapack/base/dlaqsb' );
```

#### dlaqsb( uplo, N, KD, AB, LDAB, S, strideS, scond, amax )

Equilibrates a symmetric band matrix `A` stored in band format using the scaling factors in the vector `S`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Float64Array( [ 0.0, 4.0, 1.0, 9.0, 2.0, 16.0, 3.0, 25.0 ] );
var S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );

var equed = dlaqsb( 'upper', 4, 1, AB, 2, S, 1, 0.02, 25.0 );
// returns 'yes'
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **KD**: number of super-diagonals (upper) or sub-diagonals (lower).
-   **AB**: band matrix stored in band format, dimension `(LDAB, N)`.
-   **LDAB**: leading dimension of `AB`.
-   **S**: scaling factors, length `N`.
-   **strideS**: stride for `S`.
-   **scond**: ratio of smallest to largest scaling factor.
-   **amax**: absolute value of the largest matrix element.

#### dlaqsb.ndarray( uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, S, strideS, offsetS, scond, amax )

Equilibrates a symmetric band matrix using scaling factors, with alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Float64Array( [ 0.0, 4.0, 1.0, 9.0, 2.0, 16.0, 3.0, 25.0 ] );
var S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );

var equed = dlaqsb.ndarray( 'upper', 4, 1, AB, 1, 2, 0, S, 1, 0, 0.02, 25.0 );
// returns 'yes'
```

The function has the following additional parameters:

-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **offsetS**: starting index for `S`.

The function returns a string indicating whether equilibration was performed:

-   `'none'`: no equilibration was needed.
-   `'yes'`: equilibration was done (i.e., `A` was replaced by `diag(S) * A * diag(S)`).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Equilibration is performed when the ratio of scaling factors (`scond`) is below an internal threshold (0.1), or when the largest element (`amax`) is very small or very large relative to machine precision.
-   The routine modifies `AB` in place.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlaqsb = require( '@stdlib/lapack/base/dlaqsb' );

// 4x4 symmetric band matrix (upper, KD=1, LDAB=2):
var AB = new Float64Array( [ 0.0, 4.0, 1.0, 9.0, 2.0, 16.0, 3.0, 25.0 ] );
var S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );

var equed = dlaqsb( 'upper', 4, 1, AB, 2, S, 1, 0.02, 25.0 );
console.log( 'equed:', equed );
// => 'yes'

console.log( 'AB:', AB );
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
