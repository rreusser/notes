# zlanht

> Returns the norm of a complex Hermitian tridiagonal matrix.

<section class="usage">

## Usage

```javascript
var zlanht = require( '@stdlib/lapack/base/zlanht' );
```

#### zlanht.ndarray( norm, N, d, strideD, offsetD, e, strideE, offsetE )

Returns the norm of a complex Hermitian tridiagonal matrix `A` with real diagonal `d` and complex off-diagonal `e`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );

var d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0 ] );
var e = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ] );

var result = zlanht.ndarray( 'max', 4, d, 1, 0, e, 1, 0 );
// returns 40.0
```

The function has the following parameters:

-   **norm**: specifies the norm type (`'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`).
-   **N**: order of the matrix.
-   **d**: diagonal elements as a `Float64Array` (real, length N).
-   **strideD**: stride for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: off-diagonal elements as a `Complex128Array` (length N-1).
-   **strideE**: stride for `e` (in complex elements).
-   **offsetE**: starting index for `e` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlanht` supports the following norm types:
    -   `'max'`: maximum absolute value of any element.
    -   `'one-norm'`: maximum column sum of absolute values.
    -   `'inf-norm'`: maximum row sum of absolute values.
    -   `'frobenius'`: square root of sum of squares of all elements.
-   For a Hermitian matrix, the one norm equals the infinity norm.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlanht = require( '@stdlib/lapack/base/zlanht' );

var d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
var e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );

var maxNorm = zlanht.ndarray( 'max', 5, d, 1, 0, e, 1, 0 );
console.log( 'Max norm:', maxNorm );

var oneNorm = zlanht.ndarray( 'one-norm', 5, d, 1, 0, e, 1, 0 );
console.log( 'One norm:', oneNorm );

var infNorm = zlanht.ndarray( 'inf-norm', 5, d, 1, 0, e, 1, 0 );
console.log( 'Infinity norm:', infNorm );

var frobNorm = zlanht.ndarray( 'frobenius', 5, d, 1, 0, e, 1, 0 );
console.log( 'Frobenius norm:', frobNorm );
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
