# zlangt

> Returns the norm of a complex tridiagonal matrix.

<section class="usage">

## Usage

```javascript
var zlangt = require( '@stdlib/lapack/base/zlangt' );
```

#### zlangt.ndarray( norm, N, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU )

Returns the norm of a complex general tridiagonal matrix `A` with sub-diagonal `DL`, diagonal `d`, and super-diagonal `DU`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var dl = new Complex128Array( [ 3.0, 2.0, 1.0, 4.0, 2.0, 1.0 ] );
var d = new Complex128Array( [ 2.0, 1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 1.0 ] );
var du = new Complex128Array( [ -1.0, 3.0, -2.0, 1.0, -3.0, 2.0 ] );

var result = zlangt.ndarray( 'max', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
// returns ~6.083
```

The function has the following parameters:

-   **norm**: specifies the norm type (`'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`).
-   **N**: order of the matrix.
-   **DL**: sub-diagonal elements as a `Complex128Array` (length N-1).
-   **strideDL**: stride for `DL` (in complex elements).
-   **offsetDL**: starting index for `DL` (in complex elements).
-   **d**: diagonal elements as a `Complex128Array` (length N).
-   **strideD**: stride for `d` (in complex elements).
-   **offsetD**: starting index for `d` (in complex elements).
-   **DU**: super-diagonal elements as a `Complex128Array` (length N-1).
-   **strideDU**: stride for `DU` (in complex elements).
-   **offsetDU**: starting index for `DU` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlangt` supports the following norm types:
    -   `'max'`: maximum absolute value of any element.
    -   `'one-norm'`: maximum column sum of absolute values.
    -   `'inf-norm'`: maximum row sum of absolute values.
    -   `'frobenius'`: square root of sum of squares of all elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlangt = require( '@stdlib/lapack/base/zlangt' );

var dl = new Complex128Array( [ 3.0, 2.0, 1.0, 4.0, 2.0, 1.0 ] );
var d = new Complex128Array( [ 2.0, 1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 1.0 ] );
var du = new Complex128Array( [ -1.0, 3.0, -2.0, 1.0, -3.0, 2.0 ] );

var maxNorm = zlangt.ndarray( 'max', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
console.log( 'Max norm:', maxNorm );

var oneNorm = zlangt.ndarray( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
console.log( 'One norm:', oneNorm );

var infNorm = zlangt.ndarray( 'inf-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
console.log( 'Infinity norm:', infNorm );

var frobNorm = zlangt.ndarray( 'frobenius', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
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
