# zlanhp

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex Hermitian matrix supplied in packed storage.

<section class="usage">

## Usage

```javascript
var zlanhp = require( '@stdlib/lapack/base/zlanhp' );
```

#### zlanhp( norm, uplo, N, AP, WORK )

Returns the norm of a complex Hermitian matrix supplied in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 Hermitian matrix (upper packed):
var AP = new Complex128Array( [ 2.0, 0.0, 1.0, 2.0, 5.0, 0.0, -1.0, 3.0, 0.5, -1.5, 7.0, 0.0 ] );
var WORK = new Float64Array( 3 );

var result = zlanhp( 'max', 'upper', 3, AP, WORK );
// returns 7.0
```

The function has the following parameters:

-   **norm**: specifies which norm to compute: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the upper or lower triangular part is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **AP**: packed Hermitian matrix as a [`Complex128Array`][@stdlib/array/complex128], length >= `N*(N+1)/2`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array], length >= `N` (only referenced for `'one-norm'` and `'inf-norm'`).

#### zlanhp.ndarray( norm, uplo, N, AP, strideAP, offsetAP, WORK, strideWORK, offsetWORK )

Returns the norm of a complex Hermitian matrix supplied in packed storage using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 2.0, 0.0, 1.0, 2.0, 5.0, 0.0, -1.0, 3.0, 0.5, -1.5, 7.0, 0.0 ] );
var WORK = new Float64Array( 3 );

var result = zlanhp.ndarray( 'max', 'upper', 3, AP, 1, 0, WORK, 1, 0 );
// returns 7.0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   For a Hermitian matrix, the one-norm equals the infinity-norm.
-   The diagonal elements of a Hermitian matrix are real; only their real part is used.
-   Off-diagonal elements are complex; their complex modulus is used.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhp = require( '@stdlib/lapack/base/zlanhp' );

// 3x3 Hermitian matrix (upper packed):
var AP = new Complex128Array( [ 2.0, 0.0, 1.0, 2.0, 5.0, 0.0, -1.0, 3.0, 0.5, -1.5, 7.0, 0.0 ] );
var WORK = new Float64Array( 3 );

console.log( zlanhp( 'max', 'upper', 3, AP, WORK ) );
// => 7.0

console.log( zlanhp( 'one-norm', 'upper', 3, AP, WORK ) );
// => ~11.74

console.log( zlanhp( 'frobenius', 'upper', 3, AP, WORK ) );
// => ~10.63
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
