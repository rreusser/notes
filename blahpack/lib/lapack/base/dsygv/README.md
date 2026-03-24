# dsygv

> Computes eigenvalues and optionally eigenvectors of a generalized symmetric-definite eigenproblem

<section class="usage">

## Usage

```javascript
var dsygv = require( '@stdlib/lapack/base/dsygv' );
```

#### dsygv.ndarray( itype, jobz, uplo, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, w, strideW, offsetW, WORK, strideWORK, offsetWORK, lwork )

Computes eigenvalues and optionally eigenvectors of a generalized symmetric-definite eigenproblem

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **itype**: itype.
-   **jobz**: specifies the operation type.
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
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
