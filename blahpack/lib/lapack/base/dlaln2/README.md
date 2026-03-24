# dlaln2

> Solves a 1x1 or 2x2 linear system with scaling to prevent overflow

<section class="usage">

## Usage

```javascript
var dlaln2 = require( '@stdlib/lapack/base/dlaln2' );
```

#### dlaln2.ndarray( ltrans, na, nw, smin, ca, A, strideA1, strideA2, offsetA, d1, d2, B, strideB1, strideB2, offsetB, wr, wi, X, strideX1, strideX2, offsetX, scale, xnorm )

Solves a 1x1 or 2x2 linear system with scaling to prevent overflow

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **ltrans**: ltrans.
-   **na**: na.
-   **nw**: nw.
-   **smin**: smin.
-   **ca**: ca.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **d1**: d1.
-   **d2**: d2.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **wr**: wr.
-   **wi**: wi.
-   **X**: output matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **scale**: scale.
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
