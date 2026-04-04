# zlansp

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix supplied in packed storage.

<section class="usage">

## Usage

```javascript
var zlansp = require( '@stdlib/lapack/base/zlansp' );
```

#### zlansp( norm, uplo, N, AP, WORK )

Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix supplied in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
var WORK = new Float64Array( 3 );

var result = zlansp( 'max', 'upper', 3, AP, WORK );
// returns ~5.099
```

#### zlansp.ndarray( norm, uplo, N, AP, strideAP, offsetAP, WORK, strideWORK, offsetWORK )

Returns the norm of a complex symmetric matrix in packed storage using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
var WORK = new Float64Array( 3 );

var result = zlansp.ndarray( 'max', 'upper', 3, AP, 1, 0, WORK, 1, 0 );
// returns ~5.099
```

The function has the following parameters:

-   **norm**: specifies the norm: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **AP**: packed symmetric matrix as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length at least `N`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   For a symmetric matrix, the one-norm equals the infinity-norm.
-   The `WORK` array is only used for `'one-norm'` and `'inf-norm'` norms. For `'max'` and `'frobenius'`, it is not referenced.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansp = require( '@stdlib/lapack/base/zlansp' );

// 3x3 complex symmetric matrix in upper packed storage:
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
var WORK = new Float64Array( 3 );

var result = zlansp( 'max', 'upper', 3, AP, WORK );
console.log( 'max norm: %d', result );
// => ~5.099
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
