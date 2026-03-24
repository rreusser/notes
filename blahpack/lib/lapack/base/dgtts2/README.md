# dgtts2

> Solves a real tridiagonal system using LU factorization from dgttrf (unblocked)

<section class="usage">

## Usage

```javascript
var dgtts2 = require( '@stdlib/lapack/base/dgtts2' );
```

#### dgtts2.ndarray( itrans, N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Solves a real tridiagonal system using LU factorization from dgttrf (unblocked)

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **itrans**: itrans.
-   **N**: number of columns.
-   **nrhs**: nrhs.
-   **DL**: input array.
-   **strideDL**: stride length for `DL`.
-   **offsetDL**: starting index for `DL`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **DU**: input array.
-   **strideDU**: stride length for `DU`.
-   **offsetDU**: starting index for `DU`.
-   **DU2**: input array.
-   **strideDU2**: stride length for `DU2`.
-   **offsetDU2**: starting index for `DU2`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: output matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   TODO: Add notes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
// TODO: Add examples
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
