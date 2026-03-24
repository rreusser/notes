# dlasy2

> Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices

<section class="usage">

## Usage

```javascript
var dlasy2 = require( '@stdlib/lapack/base/dlasy2' );
```

#### dlasy2.ndarray( ltranl, ltranr, isgn, n1, n2, TL, strideTL1, strideTL2, offsetTL, TR, strideTR1, strideTR2, offsetTR, B, strideB1, strideB2, offsetB, scale, X, strideX1, strideX2, offsetX, xnorm )

Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **ltranl**: ltranl.
-   **ltranr**: ltranr.
-   **isgn**: isgn.
-   **n1**: n1.
-   **n2**: n2.
-   **TL**: input matrix.
-   **strideTL1**: stride of the first dimension of `TL`.
-   **strideTL2**: stride of the second dimension of `TL`.
-   **offsetTL**: starting index for `TL`.
-   **TR**: input matrix.
-   **strideTR1**: stride of the first dimension of `TR`.
-   **strideTR2**: stride of the second dimension of `TR`.
-   **offsetTR**: starting index for `TR`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **scale**: scale.
-   **X**: output matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **xnorm**: xnorm.

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
