# dlaqr3

> Performs aggressive early deflation with blocked operations

<section class="usage">

## Usage

```javascript
var dlaqr3 = require( '@stdlib/lapack/base/dlaqr3' );
```

#### dlaqr3.ndarray( wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, ns, nd, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork )

Performs aggressive early deflation with blocked operations

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **wantt**: wantt.
-   **wantz**: wantz.
-   **N**: number of columns.
-   **ktop**: ktop.
-   **kbot**: kbot.
-   **nw**: nw.
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
-   **ns**: ns.
-   **nd**: nd.
-   **SR**: input array.
-   **strideSR**: stride length for `SR`.
-   **offsetSR**: starting index for `SR`.
-   **SI**: input array.
-   **strideSI**: stride length for `SI`.
-   **offsetSI**: starting index for `SI`.
-   **V**: input matrix.
-   **strideV1**: stride of the first dimension of `V`.
-   **strideV2**: stride of the second dimension of `V`.
-   **offsetV**: starting index for `V`.
-   **nh**: nh.
-   **T**: input matrix.
-   **strideT1**: stride of the first dimension of `T`.
-   **strideT2**: stride of the second dimension of `T`.
-   **offsetT**: starting index for `T`.
-   **nv**: nv.
-   **WV**: input matrix.
-   **strideWV1**: stride of the first dimension of `WV`.
-   **strideWV2**: stride of the second dimension of `WV`.
-   **offsetWV**: starting index for `WV`.
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
