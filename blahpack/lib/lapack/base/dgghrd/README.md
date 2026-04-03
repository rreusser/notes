# dgghrd

> Reduces a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations.

<section class="usage">

## Usage

```javascript
var dgghrd = require( '@stdlib/lapack/base/dgghrd' );
```

#### dgghrd( order, compq, compz, N, ilo, ihi, A, LDA, B, LDB, Q, LDQ, Z, LDZ )

Reduces a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations, where `Q**T * A * Z = H` (upper Hessenberg) and `Q**T * B * Z = T` (upper triangular).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 3.0, 1.0, 1.0, 0.5, 2.0, 3.0, 0.5, 2.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0.25, 1.0, 3.0 ] );
var Q = new Float64Array( 9 );
var Z = new Float64Array( 9 );

var info = dgghrd( 'column-major', 'initialize', 'initialize', 3, 1, 3, A, 3, B, 3, Q, 3, Z, 3 );
// info => 0
```

#### dgghrd.ndarray( compq, compz, N, ilo, ihi, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, Q, strideQ1, strideQ2, offsetQ, Z, strideZ1, strideZ2, offsetZ )

Reduces a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations (ndarray interface).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 3.0, 1.0, 1.0, 0.5, 2.0, 3.0, 0.5, 2.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0.25, 1.0, 3.0 ] );
var Q = new Float64Array( 9 );
var Z = new Float64Array( 9 );

var info = dgghrd.ndarray( 'initialize', 'initialize', 3, 1, 3, A, 1, 3, 0, B, 1, 3, 0, Q, 1, 3, 0, Z, 1, 3, 0 );
// info => 0
```

The function has the following parameters:

-   **compq**: specifies whether Q is computed. Must be `'none'`, `'initialize'`, or `'update'`.
-   **compz**: specifies whether Z is computed. Must be `'none'`, `'initialize'`, or `'update'`.
-   **N**: order of the matrices A and B.
-   **ilo**: ilo index (1-based).
-   **ihi**: ihi index (1-based).
-   **A**: input/output matrix A.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input/output matrix B (upper triangular on entry).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **Q**: input/output orthogonal matrix Q.
-   **strideQ1**: stride of the first dimension of `Q`.
-   **strideQ2**: stride of the second dimension of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **Z**: input/output orthogonal matrix Z.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On entry, matrix A is a general real N-by-N matrix and B is an N-by-N upper triangular matrix. On exit, A is overwritten by the upper Hessenberg matrix H and B is overwritten by the upper triangular matrix T.
-   The `compq` and `compz` parameters control whether the orthogonal transformation matrices Q and Z are computed: `'none'` means they are not referenced, `'initialize'` means they are initialized to identity and the transformations are accumulated, and `'update'` means transformations are accumulated into existing Q and Z.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dgghrd = require( '@stdlib/lapack/base/dgghrd' );

var A = new Float64Array( [ 2.0, 3.0, 1.0, 1.0, 1.0, 0.5, 0.5, 2.0, 3.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0.25, 1.0, 3.0 ] );
var Q = new Float64Array( 9 );
var Z = new Float64Array( 9 );

var info = dgghrd( 'column-major', 'initialize', 'initialize', 3, 1, 3, A, 3, B, 3, Q, 3, Z, 3 );
// info => 0
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
