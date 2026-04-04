# zlantp

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the element of largest absolute value of a complex triangular matrix supplied in packed form.

<section class="usage">

## Usage

```javascript
var zlantp = require( '@stdlib/lapack/base/zlantp' );
```

#### zlantp( norm, uplo, diag, N, AP, WORK )

Returns the norm of a complex triangular matrix supplied in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 upper triangular matrix (packed):
var AP = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 7.0, 8.0, 5.0, 6.0, 9.0, 1.0, 2.0, 3.0 ] );
var WORK = new Float64Array( 3 );

var result = zlantp( 'max', 'upper', 'non-unit', 3, AP, WORK );
// returns ~10.63
```

The function has the following parameters:

-   **norm**: specifies which norm to compute: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix has a unit diagonal (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **AP**: packed triangular matrix as a [`Complex128Array`][@stdlib/array/complex128], length >= `N*(N+1)/2`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array], length >= `N` (only referenced for `'inf-norm'`).

#### zlantp.ndarray( norm, uplo, diag, N, AP, strideAP, offsetAP, WORK, strideWORK, offsetWORK )

Returns the norm of a complex triangular matrix supplied in packed storage using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 7.0, 8.0, 5.0, 6.0, 9.0, 1.0, 2.0, 3.0 ] );
var WORK = new Float64Array( 3 );

var result = zlantp.ndarray( 'max', 'upper', 'non-unit', 3, AP, 1, 0, WORK, 1, 0 );
// returns ~10.63
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

-   When `diag` is `'unit'`, the diagonal elements are not referenced and are assumed to be one.
-   The `WORK` array is only referenced when `norm` is `'inf-norm'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantp = require( '@stdlib/lapack/base/zlantp' );

// 3x3 upper triangular matrix (packed):
var AP = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 7.0, 8.0, 5.0, 6.0, 9.0, 1.0, 2.0, 3.0 ] );
var WORK = new Float64Array( 3 );

console.log( zlantp( 'max', 'upper', 'non-unit', 3, AP, WORK ) );
// => ~10.63

console.log( zlantp( 'one-norm', 'upper', 'non-unit', 3, AP, WORK ) );
// => ~20.47

console.log( zlantp( 'frobenius', 'upper', 'non-unit', 3, AP, WORK ) );
// => ~17.29
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
