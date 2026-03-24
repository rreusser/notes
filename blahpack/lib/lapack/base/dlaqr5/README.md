# dlaqr5

> Performs a single small-bulge multi-shift QR sweep

<section class="usage">

## Usage

```javascript
var dlaqr5 = require( '@stdlib/lapack/base/dlaqr5' );
```

#### dlaqr5.ndarray( wantt, wantz, kacc22, N, ktop, kbot, nshfts, SR, strideSR, offsetSR, SI, strideSI, offsetSI, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, V, strideV1, strideV2, offsetV, U, strideU1, strideU2, offsetU, nv, WV, strideWV1, strideWV2, offsetWV, nh, WH, strideWH1, strideWH2, offsetWH )

Performs a single small-bulge multi-shift QR sweep

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **wantt**: wantt.
-   **wantz**: wantz.
-   **kacc22**: kacc22.
-   **N**: number of columns.
-   **ktop**: ktop.
-   **kbot**: kbot.
-   **nshfts**: nshfts.
-   **SR**: input array.
-   **strideSR**: stride length for `SR`.
-   **offsetSR**: starting index for `SR`.
-   **SI**: input array.
-   **strideSI**: stride length for `SI`.
-   **offsetSI**: starting index for `SI`.
-   **H**: input matrix.
-   **strideH1**: stride of the first dimension of `H`.
-   **strideH2**: stride of the second dimension of `H`.
-   **offsetH**: starting index for `H`.
-   **iloz**: iloz.
-   **ihiz**: ihiz.
-   **Z**: input matrix.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **V**: input matrix.
-   **strideV1**: stride of the first dimension of `V`.
-   **strideV2**: stride of the second dimension of `V`.
-   **offsetV**: starting index for `V`.
-   **U**: input matrix.
-   **strideU1**: stride of the first dimension of `U`.
-   **strideU2**: stride of the second dimension of `U`.
-   **offsetU**: starting index for `U`.
-   **nv**: nv.
-   **WV**: input matrix.
-   **strideWV1**: stride of the first dimension of `WV`.
-   **strideWV2**: stride of the second dimension of `WV`.
-   **offsetWV**: starting index for `WV`.
-   **nh**: nh.
-   **WH**: output matrix.
-   **strideWH1**: stride of the first dimension of `WH`.
-   **strideWH2**: stride of the second dimension of `WH`.
-   **offsetWH**: starting index for `WH`.

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
