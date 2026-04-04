# ztptri

> Computes the inverse of a complex triangular matrix in packed storage.

<section class="usage">

## Usage

```javascript
var ztptri = require( '@stdlib/lapack/base/ztptri' );
```

#### ztptri( uplo, diag, N, AP )

Computes the inverse of a complex upper or lower triangular matrix stored in packed format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 2x2 upper triangular: A = [ (2,1) (1,0); (0,0) (3,-1) ]
// Packed column-major: [ (2,1), (1,0), (3,-1) ]
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.0, 3.0, -1.0 ] );

var info = ztptri( 'upper', 'non-unit', 2, AP );
// info => 0
// AP is overwritten with the inverse in packed format
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix is unit or non-unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **AP**: packed triangular matrix of dimension `N*(N+1)/2` (Complex128Array).

The function returns an integer status code: `0` for success, `k > 0` if `A(k,k)` is exactly zero (the matrix is singular).

#### ztptri.ndarray( uplo, diag, N, AP, strideAP, offsetAP )

Computes the inverse using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 2x2 upper triangular: A = [ (2,1) (1,0); (0,0) (3,-1) ]
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.0, 3.0, -1.0 ] );

var info = ztptri.ndarray( 'upper', 'non-unit', 2, AP, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The packed triangular matrix `AP` is stored column-wise in a Complex128Array of length `N*(N+1)/2`.
-   For upper triangular storage, `AP(i + j*(j+1)/2) = A(i,j)` for `0 <= i <= j`.
-   For lower triangular storage, `AP(i-j + j*(2*N-j-1)/2) = A(i,j)` for `j <= i < N`.
-   On exit, `AP` is overwritten with the inverse of the original matrix in the same packed format.
-   If the matrix is singular (a diagonal element is zero), the function returns early with `info > 0` indicating the position of the zero diagonal.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztptri = require( '@stdlib/lapack/base/ztptri' );

// 2x2 upper triangular: A = [ (2,1) (1,0); (0,0) (3,-1) ]
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.0, 3.0, -1.0 ] );

var info = ztptri( 'upper', 'non-unit', 2, AP );
console.log( 'info:', info );
console.log( 'inverse (packed, interleaved re/im):', reinterpret( AP, 0 ) );
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
