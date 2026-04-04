# zlanhf

> Returns the norm of a complex Hermitian matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var zlanhf = require( '@stdlib/lapack/base/zlanhf' );
```

#### zlanhf( norm, transr, uplo, N, A, WORK )

Returns the norm of a complex Hermitian matrix in Rectangular Full Packed (RFP) format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 Hermitian matrix in RFP format (TRANSR='N', UPLO='U'):
var A = new Complex128Array( [ 1.0, 2.0, 3.0, 0.0, 5.0, 0.0 ] );
var WORK = new Float64Array( 2 );

var result = zlanhf( 'max', 'no-transpose', 'upper', 2, A, WORK );
```

#### zlanhf.ndarray( norm, transr, uplo, N, A, strideA, offsetA, WORK, strideWORK, offsetWORK )

Returns the norm using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var A = new Complex128Array( [ 1.0, 2.0, 3.0, 0.0, 5.0, 0.0 ] );
var WORK = new Float64Array( 2 );

var result = zlanhf.ndarray( 'max', 'no-transpose', 'upper', 2, A, 1, 0, WORK, 1, 0 );
```

The function has the following parameters:

-   **norm**: specifies the norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **transr**: specifies the RFP format: `'no-transpose'` or `'conjugate-transpose'`.
-   **uplo**: specifies which triangle: `'upper'` or `'lower'`.
-   **N**: order of the Hermitian matrix.
-   **A**: input Complex128Array in RFP format, length `N*(N+1)/2`.
-   **strideA**: stride length for `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **WORK**: workspace Float64Array (length >= N for one/inf norms).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   For a Hermitian matrix, the one-norm equals the infinity-norm.
-   Diagonal elements of a Hermitian matrix are real; only their real part is used.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhf = require( '@stdlib/lapack/base/zlanhf' );

// 3x3 Hermitian matrix in RFP format (TRANSR='N', UPLO='U'):
var A = new Complex128Array( [
    5.0, -1.0,  0.0, 6.0,  9.0, 0.0,
    1.0,  0.0,  2.0, 3.0,  4.0, 0.0
]);
var WORK = new Float64Array( 3 );

var maxNorm = zlanhf( 'max', 'no-transpose', 'upper', 3, A, WORK );
// returns <number>
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
