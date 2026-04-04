# dlansp

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real symmetric matrix supplied in packed storage.

<section class="usage">

## Usage

```javascript
var dlansp = require( '@stdlib/lapack/base/dlansp' );
```

#### dlansp( norm, uplo, N, AP, WORK )

Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real symmetric matrix supplied in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
var WORK = new Float64Array( 3 );

var result = dlansp( 'max', 'upper', 3, AP, WORK );
// returns 7.0
```

The function has the following parameters:

-   **norm**: specifies the norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **uplo**: specifies whether the upper or lower triangle is stored: `'upper'` or `'lower'`.
-   **N**: order of the matrix.
-   **AP**: packed symmetric matrix as a [`Float64Array`][mdn-float64array], length >= `N*(N+1)/2`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array], length >= `N`.

#### dlansp.ndarray( norm, uplo, N, AP, strideAP, offsetAP, WORK, strideWORK, offsetWORK )

Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real symmetric matrix supplied in packed storage using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
var WORK = new Float64Array( 3 );

var result = dlansp.ndarray( 'max', 'upper', 3, AP, 1, 0, WORK, 1, 0 );
// returns 7.0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   For a symmetric matrix, the one-norm equals the infinity-norm.
-   The workspace array `WORK` is only referenced when `norm` is `'one-norm'` or `'inf-norm'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlansp = require( '@stdlib/lapack/base/dlansp' );

// 3x3 symmetric matrix in upper packed storage:
var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
var WORK = new Float64Array( 3 );
var result;

result = dlansp( 'max', 'upper', 3, AP, WORK );
// returns 7.0

result = dlansp( 'one-norm', 'upper', 3, AP, WORK );
// returns 10.0

result = dlansp( 'frobenius', 'upper', 3, AP, WORK );
// returns ~10.296
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
