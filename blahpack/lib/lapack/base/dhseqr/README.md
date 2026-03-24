# dhseqr

> Computes eigenvalues and Schur decomposition of an upper Hessenberg matrix

<section class="usage">

## Usage

```javascript
var dhseqr = require( '@stdlib/lapack/base/dhseqr' );
```

#### dhseqr.ndarray( job, compz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork )

Computes eigenvalues and Schur decomposition of an upper Hessenberg matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: specifies the operation type.
-   **compz**: specifies the operation type.
-   **N**: number of columns.
-   **ilo**: ilo.
-   **ihi**: ihi.
-   **H**: input matrix.
-   **strideH1**: stride of the first dimension of `H`.
-   **strideH2**: stride of the second dimension of `H`.
-   **offsetH**: starting index for `H`.
-   **WR**: input array.
-   **strideWR**: stride length for `WR`.
-   **offsetWR**: starting index for `WR`.
-   **WI**: input array.
-   **strideWI**: stride length for `WI`.
-   **offsetWI**: starting index for `WI`.
-   **Z**: input matrix.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.

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
