# zhpev

> Computes all eigenvalues and optionally eigenvectors of a complex Hermitian matrix in packed storage.

<section class="usage">

## Usage

```javascript
var zhpev = require( '@stdlib/lapack/base/zhpev' );
```

#### zhpev.ndarray( jobz, uplo, N, AP, strideAP, offsetAP, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Computes all eigenvalues and optionally eigenvectors of a complex Hermitian matrix in packed storage.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 Hermitian [[5,1-i,2+i],[1+i,4,1],[2-i,1,6]] lower packed:
var AP = new Complex128Array( [ 5, 0, 1, 1, 2, -1, 4, 0, 1, 0, 6, 0 ] );
var w = new Float64Array( 3 );
var Z = new Complex128Array( 9 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 10 );

var info = zhpev.ndarray( 'compute-vectors', 'lower', 3, AP, 1, 0, w, 1, 0, Z, 1, 3, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// w contains eigenvalues in ascending order
```

The function has the following parameters:

-   **jobz**: `'no-vectors'` (eigenvalues only) or `'compute-vectors'` (eigenvalues + eigenvectors).
-   **uplo**: `'upper'` or `'lower'`.
-   **N**: order of the matrix A.
-   **AP**: packed Hermitian matrix (`Complex128Array`).
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **w**: output array for eigenvalues (`Float64Array`, length N).
-   **strideW**: stride for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: output eigenvector matrix (`Complex128Array`, N x N).
-   **strideZ1**: stride of the first dimension of `Z` (in complex elements).
-   **strideZ2**: stride of the second dimension of `Z` (in complex elements).
-   **offsetZ**: starting index for `Z` (in complex elements).
-   **WORK**: complex workspace array (`Complex128Array`, length >= max(1, 2\*N-1)).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace array (`Float64Array`, length >= max(1, 3\*N-2)).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The eigenvalues of a Hermitian matrix are always real and returned in ascending order in `w`.
-   On exit, `AP` is overwritten by the tridiagonal reduction.
-   `WORK` is a complex workspace and `RWORK` is a real workspace.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpev = require( '@stdlib/lapack/base/zhpev' );

var AP = new Complex128Array( [ 5, 0, 1, 1, 2, -1, 4, 0, 1, 0, 6, 0 ] );
var w = new Float64Array( 3 );
var Z = new Complex128Array( 9 );
var WORK = new Complex128Array( 10 );
var RWORK = new Float64Array( 10 );

zhpev.ndarray( 'compute-vectors', 'lower', 3, AP, 1, 0, w, 1, 0, Z, 1, 3, 0, WORK, 1, 0, RWORK, 1, 0 );

console.log( 'eigenvalues:', w );
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
