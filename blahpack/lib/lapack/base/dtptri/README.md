# dtptri

> Compute the inverse of a real upper or lower triangular matrix in packed storage.

<section class="usage">

## Usage

```javascript
var dtptri = require( '@stdlib/lapack/base/dtptri' );
```

#### dtptri( uplo, diag, N, AP )

Computes the inverse of a real upper or lower triangular matrix stored in packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 upper triangular: A = [2 1 3; 0 4 5; 0 0 6]
var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );

var info = dtptri( 'upper', 'non-unit', 3, AP );
// info => 0
// AP is overwritten with the inverse in packed format
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix is unit or non-unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **AP**: packed triangular matrix of dimension `N*(N+1)/2`.

The function returns an integer status code: `0` for success, `k > 0` if `A(k,k)` is exactly zero (the matrix is singular).

#### dtptri.ndarray( uplo, diag, N, AP, strideAP, offsetAP )

Computes the inverse using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 upper triangular: A = [2 1 3; 0 4 5; 0 0 6]
var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );

var info = dtptri.ndarray( 'upper', 'non-unit', 3, AP, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The packed triangular matrix `AP` is stored column-wise in a linear array of length `N*(N+1)/2`.
-   For upper triangular storage, `AP(i + j*(j+1)/2) = A(i,j)` for `0 <= i <= j`.
-   For lower triangular storage, `AP(i-j + j*(2*N-j-1)/2) = A(i,j)` for `j <= i < N`.
-   On exit, `AP` is overwritten with the inverse of the original matrix in the same packed format.
-   If the matrix is singular (a diagonal element is zero), the function returns early with `info > 0` indicating the position of the zero diagonal.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dtptri = require( '@stdlib/lapack/base/dtptri' );

// 3x3 upper triangular packed: A = [2 1 3; 0 4 5; 0 0 6]
var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );

var info = dtptri( 'upper', 'non-unit', 3, AP );
console.log( 'info:', info );
console.log( 'inverse (packed):', AP );
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
